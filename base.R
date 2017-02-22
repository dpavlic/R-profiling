.address <- function(x)
  substring(capture.output(.Internal(inspect(x)))[1],2,17)

.mode <- function(x) {
  ux <- unique(na.omit(x))
  tab <- tabulate(match(x, ux))
  mode <- ux[tab == max(tab)]
  if (is.factor(mode)) mode <- as.character(mode)
  mode
}

.is.date <- function(vect) inherits(vect, "POSIXt") || inherits(vect, "Date")

.render <- function(...) whisker::whisker.render(...)

describeNumeric1d <- function(vect, ...) {
  stats <- list(
    mean = mean(vect, na.rm = TRUE),
    std = sd(vect, na.rm = TRUE),
    variance = var(vect, na.rm = TRUE),
    min = min(vect, na.rm = TRUE),
    max = max(vect, na.rm = TRUE),
    kurtosis = moments::kurtosis(vect, na.rm = TRUE) - 3, # Fisher def
    skewness = moments::skewness(vect, na.rm = TRUE),
    sum = sum(vect, na.rm = TRUE),
    mad = (function(x) {
      x <- na.omit(vect)
      sum(abs(x - mean(x))) / length(x)
    })(vect),
    type = 'NUM',
    nZeros = sum(vect == 0, na.rm = TRUE),
    histogram = .histogram(vect)
  )
  stats$range <- stats$max - stats$min

  stats <- append(
    stats,
    as.list(quantile(vect, c(0.05, 0.25, 0.75, 0.95), na.rm = TRUE))
  )

  stats$iqr <- stats$`75%` - stats$`25%`
  stats$cv <- ifelse(is.finite(stats$mean), stats$std / stats$mean, NA)
  stats$pZeros <- stats$nZeros / length(vect)

  stats
}

.histogram <- function(vect, ...) {
  h <- hist(vect, ...)
  list(breaks = h$breaks, counts = h$counts)
}

describeDate1d <- function(vect) {
  stats <- list(
    min = min(vect, na.rm = TRUE),
    max = max(vect, na.rm = TRUE),
    type = 'DATE',
    histogram <- .histogram(vect)
  )
  stats$range <- stats$max - stats$min

  stats
}

describeCategorical1d <- function(vect) {
  objcounts <- sort(table(vect), decreasing = TRUE)
  result <- NULL

  if (is.character(vect) || is.factor(vect)) {
    result <- list(top = names(objcounts)[[1]],
                   freq = objcounts[[1]],
                   type = 'CAT')
  }

  result
}

describeConstant1d <- function(vect) {
  list(type = 'CONST')
}

describeUnique1d <- function(vect) {
  list('type' = 'UNIQUE')
}

describe1d <- function(vect, ...) {
  leng <- length(vect)
  count <- length(na.omit(vect))
  # This is a departure from pandas code, but not specifying numeric structure
  # massively slows the program down while devouring memory.
  if (is.numeric(vect) | .is.date(vect))
    nInfinite <- sum(sapply(vect, is.infinite))
  else
    nInfinite <- 0

  # Replace infinite values?
  # data.replace(toReplace=[np.inf, np.NINF, np.PINF], value=np.nan, inplace=True)
  distinctCount <- length(unique(vect))
  # NOTE: This will not return an empty value if each occurs only once.
  #       Should this be >=?
  if (count > distinctCount & count > 1) mode <- .mode(vect) #[[1]]
    else mode <- vect[[1]]

  results <- list(
    count = count,
    distinctCount = distinctCount,
    pMissing = 1 - count / leng,
    nMissing = leng - count,
    pInfinite = nInfinite / leng,
    nInfinite = nInfinite,
    isUnique = distinctCount == leng,
    mode = mode,
    pUnique = distinctCount / count,
    memorysize = object.size(vect)
  )

  if (distinctCount <= 1)
    results <- append(results, describeConstant1d(vect))
  else if (is.numeric(vect))
    results <- append(results, describeNumeric1d(vect))
  else if (.is.date(vect))
    results <- append(results, describeDate1d(vect))
  else if (distinctCount == leng)
    results <- append(results, describeUnique1d(vect))
  else
    results <- append(results, describeCategorical1d(vect))

  results
}

describe <- function(df, bins=10, correlationOverrides=None) {
  results <- lapply(df, describe1d)

  # Correlation map
  cormtx <- cor(df[, sapply(df, is.numeric)], use = 'pairwise.complete.obs')
  cormtx[lower.tri(cormtx, diag = TRUE)] <- NA
  cors <- subset(
    na.omit(data.frame(expand.grid(dimnames(cormtx)), value = c(cormtx))),
    value > .9
  )

  if (nrow(cors) > 0) {
    for (i in 1:nrow(cors)) {
      corvar <- as.character(cors[i, 'Var2'])
      corvar2 <- as.character(cors[i, 'Var1'])
      results[[corvar]] <- list(
        type = 'CORR', correlationVar = corvar2, correlation = cors[i, 'value']
      )
    }
  }

  # FIXME: Even if there's no changes so this is stuck as a reference, it is
  #        rather pointless.
  variableStats <- results

  # General statistics
  nr = nrow(df)
  tableStats <- list(
    n = nr,
    nvar = ncol(df),
    totalMissing = length(na.omit(df)),
    nDuplicates = nrow(df[duplicated(df), ]),
    memsize = object.size(df),
    recordsize = object.size(df) / nr
  )

  extraStats <- as.list(
    table(
      factor(unlist(lapply(results, function(x) x$type), use.names = FALSE),
             levels = c('NUM', 'DATE', 'CONST', 'CAT', 'UNIQUE', 'CORR'))
    )
  )
  tableStats <- append(tableStats, extraStats)
  tableStats[['REJECTED']] <- sum(extraStats$CONST, extraStats$CORR)

  list(
    table = tableStats,
    variables = variableStats,
    freq = lapply(df, table)
  )
}

toHtml <- function(sample, statsObject) {
#  "Generate a HTML report from summary statistics and a given sample.
#  Parameters
#  ----------
#  sample: DataFrame containing the sample you want to print
#  stats_object: Dictionary containing summary statistics. Should be generated with an appropriate describe() function
#  Returns
#  -------
#  str, containing profile report in HTML format

  nObs = stats_object$table$n

  didYou <- 'Did you generate this using the R-profiling describe() function?'
  if (!is.data.frame(sample)) stop('Sample must be a data frame.')
  if (!is.list(statsObject))
    stop(paste('statsObject must be a list.', didYou))
  if (!identical(names(statsObject), c('table', 'variables', 'freq')))
    stop(paste('statsObject badly formatted.', didYou))

  fmt <- function(value, name) {
    if (is.na(value)) ''
    if (name %in% valueFormatters)
      get(valueFormatters[[name]])(value)
    else if (is.numeric(value))  # Is this correct from float?
      get(valueFormatters$DEFAULT_FLOAT_FORMATTER)(value)
    else
      as.character(value)
  }

  .formatRow <- function(
    freq, label, maxFreq, rowTemplate, n, extraClass = ''
  ) {
    width <- int(freq / maxFreq * 99) + 1

    if (width > 20) {
      labelInBar <- freq
      labelAfterBar <- ''
    } else {
      labelInBar <- '&nbsp;'
      labelAfterBar <- freq
    }

    .render(
      rowTemplate,
      list(
        label = label,
        width = width,
        count = freq,
        percentage = sprintf('%2.1f%%', freq / n * 100),
        extraClass = extraClass,
        labelInBar = labelInBar,
        labelAfterBar = labelAfterBar
      )
    )
  }

  freqTable <- function(
    freqtable, n, tableTemplate, rowTemplate, maxNumberToPrint
  ) {
    freqRowsHtml <- ''
    if (maxNumberToPrint > n) maxNumberToPrint <- n
    if (maxNumberToPrint < length(freqtable)) {
      freqOther <- sum(freqtable[1:maxNumberToPrint])
      minFreq <- freqtable[[maxNumberToPrint]]
    } else {
      freqOther <- 0
      minFreq <- 0
    }

    freqMissing <- n - sum(freqtable)
    maxFreq <- max(freqtable[[0]], freqOther, freqMissing)

    for (row in seq_along(freqtable[1:maxNumberToPrint])) {
      .label <- names(freqtable)[row]
      .freq <- freqtable[[row]]

      freqRowsHtml <- append(
        freqRowsHtml,
        .formatRow(.freq, .label, maxFreq, rowTemplate, n)
      )
    }

    if (freqOther > minFreq)
      freqRowsHtml <- append(
        freqRowsHtml,
        .formatRow(
          freqOther,
          sprintf('Other values (%s)', length(freqtable) - maxNumberToPrint),
          maxFreq,
          rowTemplate,
          n,
          extraClass = 'other'
        )
      )

    if (freqMissing > minFreq)
      freqRowsHtml <- append(
        freqRowsHtml,
        .formatRow(
          freqMissing,
          '(Missing)',
          maxFreq,
          rowTemplate,
          n,
          extraClass = 'missing'
        )
      )

    .render(
      tableTemplate,
      list(rows = freqRowsHtml, varid = .address(vObj$varname))
    )
  }

  extremeObsTable <- function(
    freqtable, tableTemplate, rowTemplate, numberToPrint, n, ascending = TRUE
  ) {
    if (ascending)
      obsToPrint <- sort(freqtable)[1:numberToPrint]
    else
      obsToPrint = sort(freqtable, decreasing = TRUE)[numberToPrint:1]

    freqRowsHtml <- ''
    maxFreq <- max(obsToPrint)

    for (row in seq_along(freqtable[1:obsToPrint])) {
      .label <- names(freqtable)[row]
      .freq <- freqtable[[row]]

      freqRowsHtml <- append(
        freqRowsHtml,
        .formatRow(.freq, .label, maxFreq, rowTemplate, n)
      )
    }

    .render(tableTemplate, list(rows = freqRowsHtml))
  }

  # Variables
  rowsHtml <- ''
  messages <- ''

  vObj <- statsObject$variables
  fObj <- statsObject$freq
  tObj <- statsObject$table

  for (varname in names(vObj)) {
    formattedValues <- list(
      varname = varname,
      varid = .address(vObj$varname)
    )
    rowClasses <- list()

    for (stat in names(vObj$varname))
      formattedValues[stat] <- fmt(vObj$varname$stat, stat)

    for (col in intersect(names(vObj$varname), names(rowFormatters))) {
      rowClasses[col] <- get(rowFormatters[[col]])(vObj$varname[[col]])
      if (rowClasses[[col]] == 'alert' & col %in% names(templateMessages))
        append(
          messages,
          .render(
            templateMessages[[col]]),
            append(formattedValues, list(varname = fmtVarname(varname)))
        )
    }

    if (vObj$varname$type == 'CAT')
      formattedValues['minifreqtable'] <- freqTable(
        fObj[[varname]],
        nObs,
        templates.template('miniFreqTable'),
        templates.template('miniFreqTableRow'),
        3
      )

      if (vObj$varname$distinctCount > 50) {
        # FIXME
        append(
          messages,
          .render(
            templateMessages$HIGH_CARDINALITY,
            list(varname = varname,  distCount = vObj$varname$distinctCount)
          )
        )
        rowClasses['distinctCount'] <- alert
      } else {
        rowClasses['distinctCount'] <- ''
      }

    if (vObj$varname$type == 'UNIQUE') {
      obs <- fObj$varname
      lobs <- length(fObj$varname)

      # FIXME
      formattedValues['firstn'] <- to_html(
        obs[1:3],
        columns ='First 3 values',
        classes = 'exampleValues'
      )
      # FIXME
      formattedValues['lastn'] <- to_html(
        obs[(lobs - 3):lobs],
        columns = 'Last 3 values',
        claases = 'exampleValues'
      )
    }

    if (vObj$varname$type %in% c('CORR', 'CONST')) {
      formattedValues['varname'] <- formatters.fmt_varname(varname)
      # FIXME
      append(
        messages,
        .render(templateMessages[[vObj$varname$type]], formattedValues)
      )
    } else {
      formattedValues['freqtable'] <- freqTable(
        fObj$varname,
        nObs,
        template('freqTable'),
        template('freqTableRow'),
        10
      )
      formattedValues['firstnExpanded'] <- extremeObsTable(
        fObj$varname,
        template('freqTable'),
        template('freqTableRow'),
        5,
        nObs,
        ascending = TRUE
      )
      formattedValues['lastNExpanded'] <- extremeObsTable(
        fObj$varname,
        template('freqTable'),
        template('freqTableRow'),
        5,
        nObs,
        ascending = FALSE
      )
    }

    rowsHtml <- append(
      rowsHtml,
      .render(rowTemplatesDict[[vObj$type]],
              list(values = formattedValues, rowClasses = rowClasses))
    )
  }

  # Overview
  # FIXME
  #formatted_values = {k: fmt(v, k) for k, v in six.iteritems(stats_object['table'])}

  rowClasses <- list()

  # FIXME
  #for col in six.viewkeys(stats_object['table']) & six.viewkeys(row_formatters):
  #  row_classes[col] = row_formatters[col](stats_object['table'][col])
  #if row_classes[col] == "alert" and col in templates.messages:
  #  messages.append(templates.messages[col].format(formatted_values, varname = formatters.fmt_varname(idx)))

  messagesHtml <- ''
  for (msg in messages) {
    messagesHtml <- append(
      messagesHtml,
      .render(templateMessageRow, list(message = msg))
    )
  }

  overviewHtml <- .render(template('overview'), list(
    values = formattedValues,
    rowClasses = rowClasses,
    messages = messagesHtml
  ))

  # Sample
  # FIXME
  #sample_html = templates.template('sample').render(sample_table_html=sample.to_html(classes="sample"))

  # TODO: should be done in the template
  .render(
    template('base'),
    list(overviewHtml = overviewHtml,
         rowsHtml = rowsHtml,
         sampleHtml = sampleHtml)
  )
}

a <- read.csv("~/projects/Meteorite_Landings.csv")

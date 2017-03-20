.htmlTableVector <- function(vec, tableHeading = NULL, extraClass = '') {
  if (!is.atomic(vec)) stop('Not a vector')
  if (extraClass != '')
    extraClass <- paste0(' ', extraClass)
  if (!is.null(tableHeading))
    tableHeading <-  paste0('<th>', tableHeading, '</th>')

  header <- c(
    sprintf('<table border="1" class="dataframe%s">', extraClass),
    '<thead>',
    '<tr style="text-align: right;">',
    tableHeading,
    '</tr>',
    '</thead>',
    '<tbody>')

  body <- sapply(
    vec, function(x) paste0('<tr><td>', x, '</tr></td>'), USE.NAMES = FALSE
  )

  footer <- c(
    '</tbody>',
    '</table>'
  )

  paste(c(header, body, footer), collapse = '\n')
}

.htmlTableDF <- function(df, extraClass = '') {
  if (extraClass != '')
    extraClass <- paste0(' ', extraClass)

  header <- c(
    '<div class="row variablerow">',
    '<div class="col-md-12" style="overflow:scroll; width: 100%%; overflow-y: hidden;">',
    sprintf('<table border="1" class="dataframe%s">', extraClass)
  )

  tableNames <- c(
    '<thead>',
    '<tr style="text-align: right;">',
    '<th></th>', # Empty th for obs counter
    sapply(names(df), function(x) paste0('<th>', x, '</th>'), USE.NAMES = FALSE),
    '</tr>',
    '</thead>'
  )

  # The table body
  tbody <- c(
    '<tbody>',
    {
      crow <- c()
      for (row in 1:nrow(df)) {
        crow <- c(crow, c('<tr><th>', row, '</th>',
                          lapply(df[row, ], function(x) paste0('<td>', x, '</td>')), '</tr>'))

      }
      paste(crow, collapse = '')
    },
    '</tbody>'
  )

  # End
  tEnd <- '</table></div></div>'

  paste(c(header, tableNames, tbody, tEnd), collapse = '\n')
}


.address <- function(x)
  gsub(
    ' ',
    '-',
    substring(capture.output(.Internal(inspect(x)))[1],2,17),
    fixed = TRUE
  )

.mode <- function(x) {
  ux <- unique(na.omit(x))
  tab <- tabulate(match(x, ux))
  mode <- ux[tab == max(tab)]
  if (is.factor(mode)) mode <- as.character(mode)
  mode
}

.is.date <- function(vect) inherits(vect, "POSIXt") || inherits(vect, "Date")

.render <- function(...) whisker::whisker.render(...)

.histogram <- function(vect, bins = 10) {
  histogram <- hist(vect, breaks = bins, plot = FALSE)

  tf <- tempfile('r_prof_fhist_')
  png(tf, width = 600, height = 400)
  plot(histogram, col = '#337ab7', xlab = NULL, main = NULL)
  dev.off()
  fullHistogram <- paste0('data:image/png;base64,',
                           base64enc::base64encode(tf))
  unlink(tf)

  tf <- tempfile('r_prof_fhist_')
  png(tf, width = 200, height = 75)
  par(mar = c(0,0,0,0))
  plot(histogram, col = '#337ab7', xlab = NULL, main = NULL,
       ylab = NULL, xaxt = 'n', yaxt = 'n', lty = 'blank')

  dev.off()
  miniHistogram <- paste0('data:image/png;base64,',
                           base64enc::base64encode(tf))
  unlink(tf)

  list(fullHistogram = fullHistogram, miniHistogram = miniHistogram)
}

describeNumeric1d <- function(vect, ...) {
  hists <- .histogram(vect)

  stats <- list(
    mean = mean(vect, na.rm = TRUE),
    std = sd(vect, na.rm = TRUE),
    variance = var(vect, na.rm = TRUE),
    min = min(vect, na.rm = TRUE),
    max = max(vect, na.rm = TRUE),
    kurtosis = moments::kurtosis(vect, na.rm = TRUE) - 3, # Fisher def
    skewness = moments::skewness(vect, na.rm = TRUE),
    mad = (function(x) {
      x <- na.omit(vect)
      sum(abs(x - mean(x))) / length(x)
    })(vect),
    type = 'NUM',
    nZeros = sum(vect == 0, na.rm = TRUE),
    histogram = hists$fullHistogram,
    miniHistogram = hists$miniHistogram
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

describeDate1d <- function(vect) {
  hists <- .histogram(vect)

  stats <- list(
    min = min(vect, na.rm = TRUE),
    max = max(vect, na.rm = TRUE),
    type = 'DATE',
    histogram <- hists$fullHistogram,
    miniHistogram <- hists$miniHistogram
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
  nvar = ncol(df)
  tableStats <- list(
    n = nr,
    nvar = nvar,
    totalMissing = Reduce('+', lapply(variableStats, function(x) x$nMissing)) /
                   (nr * nvar),
    nDuplicates = nrow(df[duplicated(df), ]),
    memsize = fmtByteSize(object.size(df)),
    recordsize = fmtByteSize(object.size(df) / nr)
  )

  extraStats <- as.list(
    table(
      factor(unlist(lapply(results, function(x) x$type), use.names = FALSE),
             levels = c('NUM', 'DATE', 'CONST', 'CAT', 'UNIQUE', 'CORR'))
    )
  )
  tableStats <- append(tableStats, extraStats)
  tableStats[['REJECTED']] <- sum(extraStats$CONST, extraStats$CORR)

  tableFreq <- lapply(
    names(df),
    function(x) {
      tmpdf <- plyr::count(df, x)
      na.omit(tmpdf[order(-tmpdf$freq), ])
    }
  )
  names(tableFreq) <- names(df)

  list(table = tableStats, variables = variableStats, freq = tableFreq)
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

  nObs <- statsObject$table$n

  didYou <- 'Did you generate this using the R-profiling describe() function?'
  if (!is.data.frame(sample)) stop('Sample must be a data frame.')
  if (!is.list(statsObject))
    stop(paste('statsObject must be a list.', didYou))
  if (!identical(names(statsObject), c('table', 'variables', 'freq')))
    stop(paste('statsObject badly formatted.', didYou))

  fmt <- function(value, name) {
    # NOTE: Any really serves really no point here except to shut down warning
    #       messages.
    # FIXME: This is NOT quite right because we need to pass HISTOGRAMS
    #        properly.
    if (any(is.na(value))) return(NULL)
    if (name %in% names(valueFormatters))
      get(valueFormatters[[name]])(value)
    else if (is.numeric(value))  # Is this correct from float?
      get(valueFormatters$DEFAULT_FLOAT_FORMATTER)(value)
    else
      as.character(value)
  }

  .formatRow <- function(
    freq, label, maxFreq, rowTemplate, n, extraClass = NULL
  ) {
    width <- as.integer(freq / maxFreq * 99) + 1

    if (width > 20) {
      labelInBar <- freq
      labelAfterBar <- NULL
    } else {
      labelInBar <- '&nbsp;'
      labelAfterBar <- freq
    }

    .render(
      paste(rowTemplate, collapse = '\n'),
      list(
        label = label,
        width = width,
        count = freq,
        percentage = sprintf('%2.1f', freq / n * 100), # FIXME PCT------------
        extraClass = extraClass,
        labelInBar = labelInBar,
        labelAfterBar = labelAfterBar
      )
    )
  }

  freqTable <- function(
    freqTable, n, tableTemplate, rowTemplate, maxNumberToPrint
  ) {
    freqRowsHtml <- NULL

    tableLength <- nrow(freqTable)
    if (maxNumberToPrint > tableLength) maxNumberToPrint <- tableLength
    if (maxNumberToPrint < tableLength) {
      freqOther <- sum(freqTable$freq[1:maxNumberToPrint])
      minFreq <- freqTable$freq[[maxNumberToPrint]]
    } else {
      freqOther <- 0
      minFreq <- 0
    }

    freqMissing <- n - sum(freqTable$freq)
    maxFreq <- max(freqTable$freq[1], freqOther, freqMissing)

    for (r in 1:maxNumberToPrint)
      freqRowsHtml <- append(
        freqRowsHtml,
        .formatRow(freqTable[r, 2], freqTable[r, 1], maxFreq, rowTemplate, n)
      )

    if (freqOther > minFreq)
      freqRowsHtml <- append(
        freqRowsHtml,
        .formatRow(
          freqOther,
          sprintf('Other values (%s)', nrow(freqTable) - maxNumberToPrint),
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
      paste(tableTemplate, collapse = '\n'),
      list(rows = paste(freqRowsHtml, collapse = '\n'), varid = .address(vObj[[varname]]))
    )
  }

  extremeObsTable <- function(
    freqTable, tableTemplate, rowTemplate, numberToPrint, n, ascending = TRUE
  ) {
    if (nrow(freqTable) < numberToPrint) numberToPrint <- nrow(freqTable)
    if (ascending)
      obsToPrint <- freqTable[order(freqTable[[1]]), ][1:numberToPrint, ]
    else {
      obsToPrint <- freqTable[order(freqTable[[1]],
                                    decreasing = TRUE), ][numberToPrint:1, ]
    }

    freqRowsHtml <- NULL
    maxFreq <- max(obsToPrint$freq)

    for (r in 1:nrow(obsToPrint))
      freqRowsHtml <- append(
        freqRowsHtml,
        .formatRow(obsToPrint[r, 2], obsToPrint[r, 1], maxFreq, rowTemplate, n)
      )

#    for (row in seq_along(obsToPrint[1:length(obsToPrint)])) {
#      .label <- names(obsToPrint)[row]
#      .freq <- obsToPrint[[row]]
#
#      freqRowsHtml <- append(
#        freqRowsHtml,
#        .formatRow(.freq, .label, maxFreq, rowTemplate, n)
#      )
#    }
    .render(paste(tableTemplate, '\n'), list(rows = paste(freqRowsHtml, collapse = '\n')))
  }

  # Variables
  rowsHtml <- NULL
  messages <- NULL

  vObj <- statsObject$variables
  fObj <- statsObject$freq
  tObj <- statsObject$table

  for (varname in names(vObj)) {
    formattedValues <- list(
      varname = varname,
      varid = .address(vObj[[varname]])
    )
    rowClasses <- list()

    for (stat in names(vObj[[varname]]))
      formattedValues[stat] <- fmt(vObj[[varname]][[stat]], stat)

    for (col in intersect(names(vObj[[varname]]), names(rowFormatters))) {
      rowClasses[col] <- get(rowFormatters[[col]])(vObj[[varname]][[col]])
      if (rowClasses[[col]] == 'alert' & col %in% names(templateMessages))
        messages <- append(
          messages,
          .render(
            paste(templateMessages[[col]], collapse = '\n'),
            append(formattedValues[names(formattedValues) != 'varname'],
                   list(varname = fmtVarname(varname)))
          )
        )
    }

    if (vObj[[varname]]$type == 'CAT') {
      formattedValues['miniFreqTable'] <- freqTable(
        fObj[[varname]],
        nObs,
        template('miniFreqTable'),
        template('miniFreqTableRow'),
        3
      )

      if (vObj[[varname]]$distinctCount > 50) {
        # FIXME
        messages <- append(
          messages,
          .render(
            paste(templateMessages$HIGH_CARDINALITY, collapse = '\n'),
            list(varname = fmtVarname(varname),
                 distCount = vObj[[varname]]$distinctCount)
          )
        )
        rowClasses['distinctCount'] <- 'alert'
      } else {
        rowClasses['distinctCount'] <- NULL
      }
    }

    # FIXMEX2 ----------------------------------------
    if (vObj[[varname]]$type == 'UNIQUE') {
      obs <- fObj[[varname]]
      lobs <- nrow(obs)
      if (lobs < 3) obsx <- lobs else obsx <- 3 # FIXME: I think this works

      # FIXME, FORMATTING, columns, classes.
      formattedValues['firstn'] <-
        .htmlTableVector(head(obs, n = obsx)[[1]],
                         tableHeading = sprintf('First %s values', obsx),
                         extraClass = 'example_values')

      formattedValues['lastn'] <-
        .htmlTableVector(tail(obs, n = obsx)[[1]],
                         tableHeading = sprintf('Last %s values', obsx),
                         extraClass = 'example_values')
    }

    if (vObj[[varname]]$type %in% c('CORR', 'CONST')) {
      formattedValues['varname'] <- fmtVarname(varname)
      # FIXME
      messages <- append(
        messages,
        .render(paste(templateMessages[[vObj[[varname]]$type]], collapse = '\n'),
                append(formattedValues, list(varname = fmtVarname(varname))))
      )
    } else {
      formattedValues['freqTable'] <- freqTable(
        fObj[[varname]],
        nObs,
        template('freqTable'),
        template('freqTableRow'),
        10
      )
      # FIXME DEBUG
      formattedValues['firstnExpanded'] <- extremeObsTable(
        fObj[[varname]],
        template('freqTable'),
        template('freqTableRow'),
        5,
        nObs,
        ascending = TRUE
      )
      formattedValues['lastnExpanded'] <- extremeObsTable(
        fObj[[varname]],
        template('freqTable'),
        template('freqTableRow'),
        5,
        nObs,
        ascending = FALSE
      )
    }
    rowsHtml <- append(
      rowsHtml,
      .render(
        paste(rowTemplatesDict[[vObj[[varname]]$type]], collapse = '\n'),
        list(values = formattedValues, classes = rowClasses),
        partials = list(
          rowHeaderIgnore = template('.rowHeaderIgnore'),
          rowHeader = template('.rowHeader'),
          rowFooter = template('.rowFooter'),
          varType = varType[[vObj[[varname]]$type]]
        )
      )
    )
  }

  # Overview
  formattedValues <- mapply(function(k, v) fmt(k, v), tObj, names(tObj))
  rowClasses <- list()

  # FIXME
  # TODO: Check to make sure th/s is an accurate PY conversion.
  for (col in intersect(names(tObj), names(rowFormatters))) {
    rowClasses[col] <- get(rowFormatters[[col]])(tObj[[col]])
  }
  for (col in intersect(names(tObj[[varname]]), names(rowFormatters))) {
    rowClasses[col] <- get(rowFormatters[[col]])(tObj[[varname]][[col]])
    if (rowClasses[[col]] == 'alert' & col %in% names(templateMessages))
      messages <- append(
        messages,
        .render(
          paste(templateMessages[[col]], collapse = '\n'),
          append(formattedValues, list(varname = fmtVarname(varname)))
        )
      )
  }

  messagesHtml <- NULL
  for (msg in messages) {
    messagesHtml <- append(
      messagesHtml,
      .render(paste(templateMessageRow, collapse = '\n'), list(message = msg))
    )
  }

  overviewHtml <- .render(paste(template('overview'), collapse = '\n'), list(
    values = formattedValues,
    rowClasses = rowClasses,
    messages = paste(messagesHtml, collapse = '\n')
  ))

  # Sample FIXME FIXME FIXME
  #sampleHtml = .render(paste(template('sample'), collapse = '\n'),
  #                     list(sampleTableHtml = capture.output(xtable::print.xtable(xtable::xtable(sample)))))
  # TEMP
  sampleHtml <- .htmlTableDF(sample, extraClass = 'sample')

  # TODO: should be done in the template
  .render(
    paste(template('base'), collapse = '\n'),
    list(overviewHtml = paste(overviewHtml, collapse = '\n'),
         rowsHtml = paste(rowsHtml, collapse = '\n'),
         sampleHtml = paste(sampleHtml, collapse = '\n'))
  )
}

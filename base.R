.mode <- function(x) {
  ux <- unique(na.omit(x))
  tab <- tabulate(match(x, ux))
  mode <- ux[tab == max(tab)]
  if (is.factor(mode)) mode <- as.character(mode)
  mode
}

.is.date <- function(vect) inherits(vect, "POSIXt") || inherits(vect, "Date")

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

  #return {'table': tableStats, 'variables': variableStats.T, 'freq': {k: df[k].valueCounts() for k in df.columns}}
  list(table = tableStats)

  list(
    table = tableStats,
    variables = variableStats,
    freq = lapply(df, table)
  )
}

toHtml <- function(sample, statsObject) {


}

a <- read.csv("~/projects/Meteorite_Landings.csv")

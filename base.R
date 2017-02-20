.mode <- function(x) {
  ux <- unique(na.omit(x))
  tab <- tabulate(match(x, ux))
  mode <- ux[tab == max(tab)]
  if (is.factor(mode)) mode <- as.character(mode)
  mode
}

.is.date <- function(vect) inherits(vect, "POSIXt") || inherits(vect, "Date")

describe_numeric_1d <- function(vect, ...) {
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
    n_zeros = sum(vect == 0, na.rm = TRUE),
    histogram = .histogram(vect)
  )
  stats$range <- stats$max - stats$min

  stats <- append(
    stats,
    as.list(quantile(vect, c(0.05, 0.25, 0.75, 0.95), na.rm = TRUE))
  )

  stats$iqr <- stats$`75%` - stats$`25%`
  stats$cv <- ifelse(is.finite(stats$mean), stats$std / stats$mean, NA)
  stats$p_zeros <- stats$n_zeros / length(vect)

  stats
}

.histogram <- function(vect, ...) {
  h <- hist(vect, ...)
  list(breaks = h$breaks, counts = h$counts)
}

describe_date_1d <- function(vect) {
  stats <- list(
    min = min(vect, na.rm = TRUE),
    max = max(vect, na.rm = TRUE),
    type = 'DATE',
    histogram <- .histogram(vect)
  )
  stats$range <- stats$max - stats$min

  stats
}

describe_categorical_1d <- function(vect) {
  objcounts <- sort(table(vect), decreasing = TRUE)
  result <- NULL

  if (is.character(vect) || is.factor(vect)) {
    result <- list(top = names(objcounts)[[1]],
                   freq = objcounts[[1]],
                   type = 'CAT')
  }

  result
}

describe_constant_1d <- function(vect) {
  list(type = 'CONST')
}

describe_unique_1d <- function(vect) {
  list('type' = 'UNIQUE')
}

describe_1d <- function(vect, ...) {
  leng <- length(vect)
  count <- length(na.omit(vect))
  # This is a departure from pandas code, but not specifying numeric structure
  # massively slows the program down while devouring memory.
  if (is.numeric(vect) | .is.date(vect))
    n_infinite <- sum(sapply(vect, is.infinite))
  else
    n_infinite <- 0

  # Replace infinite values?
  # data.replace(to_replace=[np.inf, np.NINF, np.PINF], value=np.nan, inplace=True)
  distinct_count <- length(unique(vect))
  # NOTE: This will not return an empty value if each occurs only once.
  #       Should this be >=?
  if (count > distinct_count & count > 1) mode <- .mode(vect) #[[1]]
    else mode <- vect[[1]]

  results <- list(
    count = count,
    distinct_count = distinct_count,
    p_missing = 1 - count / leng,
    n_missing = leng - count,
    p_infinite = n_infinite / leng,
    n_infinite = n_infinite,
    is_unique = distinct_count == leng,
    mode = mode,
    p_unique = distinct_count / count,
    memorysize = format(object.size(vect), units = 'KiB')
  )

  if (distinct_count <= 1)
    results <- append(results, describe_constant_1d(vect))
  else if (is.numeric(vect))
    results <- append(results, describe_numeric_1d(vect))
  else if (.is.date(vect))
    results <- append(results, describe_date_1d(vect))
  else if (distinct_count == leng)
    results <- append(results, describe_unique_1d(vect))
  else
    results <- append(results, describe_categorical_1d(vect))

  results
}

describe <- function(df, bins=10, correlation_overrides=None) {
  results <- lapply(df, describe_1d)

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
        type = 'CORR', correlation_var = corvar2, correlation = cors[i, 'value']
      )
    }
  }

  # General statistics
  table_stats <- new.env()
  table_stats[['n']] <- nrow(df)
  table_stats[['nvar']] <- ncol(df)
  table_stats[['total_missing']] <- length(na.omit(df))
  table_stats[['n_duplicates']] <- nrow(df[duplicated(df), ])

  memsize <- object.size(df)
  table_stats[['memsize']] <- format(object.size(df), units = 'MiB')
  table_stats[['recordsize']] <- memsize / table_stats[['n']]

  extra_stats <- as.list(
    table(
      factor(unlist(lapply(results, function(x) x$type), use.names = FALSE),
             levels = c('NUM', 'DATE', 'CONST', 'CAT', 'UNIQUE', 'CORR'))
    )
  )

  for (vals in seq_along(extra_stats)) {
    table_stats[[names(extra_stats)[vals]]] <- extra_stats[[vals]]
  }

  table_stats[['REJECTED']] <- sum(extra_stats$CONST, extra_stats$CORR)

  #ldesc_df
  as.list(table_stats)
  #results
}

to_html <- function(sample, stats_object) {


}

a <- read.csv("~/projects/Meteorite_Landings.csv")

SKEWNESS_CUTOFF <- 20
DEFAULT_FLOAT_FORMATTER <- function(v) sprintf('%5g', v)

gradientFormat <- function(value, limit1, limit2, c1, c2) {
  lerpColour <- function(c1, c2, t)
    list(as.integer(c1[[1]] + (c2[[1]] - c1[[1]]) * t),
         as.integer(c1[[2]] + (c2[[2]] - c1[[2]]) * t),
         as.integer(c1[[3]] + (c2[[3]] - c1[[3]]) * t))
  c = lerpColour(c1, c2, (value - limit1) / (limit2 - limit1))
  fmtColor(value, sprintf('rgb(%s)', paste(c, collapse = ', ')))
}

fmtFreq <- function(v)
  gradientFormat(v, 0, 62000, list(30, 198, 244), list(99, 200,72))

fmtColor <- function(text, color)
  sprintf('<span style="color:%s">%s</span>', color, text)

fmtClass <- function(text, cls)
  sprintf('<span class="%s">%s</span', cls, text)

fmtByteSize <- function(num, suffix = 'B') {
  for (unit in list('', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi', 'Ei', 'Zi')) {
    if (abs(num) < 1024) {
      return(sprintf('%3.1f %s%s', num, unit, suffix))
    }
    num = num / 1024
  }
}

fmtPercent <- function(v)
  sprintf('%2.1f%%', v * 100)

fmtVarname <- function(v)
  sprintf('<code>%s</code>', v)

valueFormatters <- list(
  freq = 'fmtFreq',
  pMissing = 'fmtPercent',
  pInfinite = 'fmtPercent',
  pUnique = 'fmtPercent',
  pZeros = 'fmtPercent',
  memorysize = 'fmtByteSize',
  totalMissing = 'fmtPercent',
  DEFAULT_FLOAT_FORMATTER = 'DEFAULT_FLOAT_FORMATTER',
  correlationVar = 'fmtVarname'
)

fmtRowSeverity <- function(v) {
  if (is.na(v) | v <= 0.01)
    'ignore'
  else
    'alert'
}

fmtSkewness <- function(v) {
  if (!is.na(v) & (v < -SKEWNESS_CUTOFF | v > SKEWNESS_CUTOFF))
    'alert'
  else
    ''
}

rowFormatters <- list(
  pZeros = 'fmtRowSeverity',
  pMissing = 'fmtRowSeverity',
  pInfinite = 'fmtRowSeverity',
  nDuplicates = 'fmtRowSeverity', # This will never get called on row?
  skewness = 'fmtSkewness'
)

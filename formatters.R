SKEWNESS_CUTOFF = 20
DEFAULT_FLOAT_FORMATTER =

gradientFormat <- function(value, limit1, limit2, c1, c2) {
  lerpColour <- function(c1, c2, t)
    # FIXME: This needds modifiers to work in R
    list(int(c1[0] + (c2[0]-c1[0]) * t), int(c1[1] + (c2[1] - c1[1]) * t), int(c1[2] + (c2[2] - c1[2]) * t))
  c = lerpColour(c1, c2, (valu - limit1) / (limit2 = limit1))
  fmtColor(value, sprintf('rgb%s', c))
}

fmtColor <- function(text, color)
  sprintf('<span style="color:%s">%s</span>', color, text)

fmtClass <- function(text, cls)
  sprintf('<span class="%s">%s</span', cls, text)

fmtBytesize <- function(num, suffix = 'B') {
  for (unit in list('', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi', 'Ei', 'Zi')) {
    if (abs(num) < 1024) {
      sprintf('%3.1f %s%s', num, unit, suffix)
      num = num / 1024
    }
  }
}

fmtPercent <- function(v)
  sprintf('%1.2f%%', v * 100)

fmtVarname <- function(v)
  sprintf('<code>%s</code>', v)

# FIXME! Using funs???
value_formatters = list(
  freq = '(lambda v: gradientFormat(v, 0, 62000, (30, 198, 244), (99, 200, 72)))',
  p_missing = 'fmtPercent',
  p_infinite = 'fmtPercent',
  p_unique = 'fmtPercent',
  p_zeros = 'fmtPercent',
  memorysize = 'fmtBytesize',
  total_missing = 'fmtPercent',
  DEFAULT_FLOAT_FORMATTER = 'lambda v = str(float("{:.5g}".format(v))).rstrip("0").rstrip(".")',
  correlation_var = 'lambda v: fmtVarname(v)'
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

rowFormatters = list(
  p_zeros: 'fmtRowSeverity',
  p_missing: 'fmtRowSeverity',
  p_infinite: 'fmtRowSeverity',
  n_duplicates: 'fmtRowSeverity',
  skewness: 'fmtSkewness'
)
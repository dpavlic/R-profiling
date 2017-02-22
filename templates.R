# Mapping between template name and file
templates = list(
  freqTableRow = 'freq_table_row.html',
  miniFreqTableRow = 'mini_freq_table_row.html',
  freqTable = 'freq_table.html',
  miniFreqTable = 'mini_freq_table.html',
  rowNum = 'row_num.html',
  rowDate = 'row_date.html',
  rowCat = 'row_cat.html',
  rowCorr = 'row_corr.html',
  rowConst = 'row_const.html',
  rowUnique = 'row_unique.html',
  overview = 'overview.html',
  sample = 'sample.html',
  base = 'base.html',
  wrapper = 'wrapper.html'
)

# Mapping between row type and var type
varType = list(
  NUM = 'Numeric',
  DATE = 'Date',
  CAT = 'Categorical',
  UNIQUE = 'Categorical, Unique',
  CONST = 'Constant',
  CORR = 'Highly correlated'
)

template <- function(templateName)
  readLines(sprintf('templates/%s', templates[[templateName]]), warn = FALSE)

# mapping between row type and template name
rowTemplatesDict = list(
  NUM = template('rowNum'),
  DATE = template('rowDate'),
  DISCRETE = template('rowNum'),
  CAT = template('rowCat'),
  UNIQUE = template('rowUnique'),
  CONST = template('rowConst'),
  CORR = template('rowCorr')
)

templateMessages = list(
  CONST = '{0[varname]} has constant value {0[mode]} <span class="label label-primary">Rejected</span>',
  CORR = '{0[varname]} is highly correlated with {0[correlation_var]} (ρ = {0[correlation]}) <span class="label label-primary">Rejected</span>',
  HIGH_CARDINALITY = '{{varname}} has a high cardinality: {{distCount}} distinct values  <span class="label label-warning">Warning</span>',
  nDuplicates = 'Dataset has {0[n_duplicates]} duplicate rows <span class="label label-warning">Warning</span>',
  skewness = '{varname} is highly skewed (γ1 = {0[skewness]})',
  pMissing = '{varname} has {0[n_missing]} / {0[p_missing]} missing values <span class="label label-default">Missing</span>',
  pInfinite = '{varname} has {0[n_infinite]} / {0[p_infinite]} infinite values <span class="label label-default">Infinite</span>',
  pZeros = '{varname} has {0[n_zeros]} / {0[p_zeros]} zeros'
)

templateMessageRow = '<li>{{message}}</l>'

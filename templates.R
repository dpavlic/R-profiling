# Mapping between template name and file
templates <- list(
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
  wrapper = 'wrapper.html',
  .rowHeaderIgnore = '_row_header_ignore.html',
  .rowHeader = '_row_header.html',
  .rowFooter = '_row_footer.html'
)

# Mapping between row type and var type
varType <- list(
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
rowTemplatesDict <- list(
  NUM = template('rowNum'),
  DATE = template('rowDate'),
  DISCRETE = template('rowNum'),
  CAT = template('rowCat'),
  UNIQUE = template('rowUnique'),
  CONST = template('rowConst'),
  CORR = template('rowCorr')
)

templateMessages <- list(
  CONST = '{{{varname}}} has constant value {{mode}} <span class="label label-primary">Rejected</span>',
  CORR = '{{{varname}}} is highly correlated with {{correlationVar}} (ρ = {{correlation}}) <span class="label label-primary">Rejected</span>',
  HIGH_CARDINALITY = '{{{varname}}} has a high cardinality: {{distCount}} distinct values  <span class="label label-warning">Warning</span>',
  nDuplicates = 'Dataset has {{nDuplicates}} duplicate rows <span class="label label-warning">Warning</span>',
  skewness = '{{{varname}}} is highly skewed (γ1 = {{skewness}})',
  pMissing = '{{{varname}}} has {{nMissing}} / {{pMissing}} missing values <span class="label label-default">Missing</span>',
  pInfinite = '{{{varname}}} has {{nInfinite}} / {{pInfinite}} infinite values <span class="label label-default">Infinite</span>',
  pZeros = '{{{varname}}} has {{nZeros}} / {{pZeros}} zeros'
)

templateMessageRow <- '<li>{{{message}}}</l>'

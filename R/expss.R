#' expss: Some Useful Functions from Spreadsheets and SPSS Statistics
#' 
#' 'expss' package implements some popular functions from spreadsheets and SPSS
#' Statistics software. Implementations are not complete copies of their
#' originals. I try to make them consistent with other R functions. See examples in vignette and in help.
#' 
#' @section Excel:
#' \itemize{
#' \item{IF }{\link[base]{ifelse}}
#' \item{AVERAGE }{\link{mean_row}}
#' \item{SUM }{\link{sum_row}}
#' \item{MIN }{\link{min_row}}
#' \item{MAX }{\link{max_row}}
#' \item{VLOOKUP }{\link{vlookup}}
#' \item{COUNTIF }{\link{count_if}}
#' \item{AVERAGEIF }{\link{mean_row_if}}
#' \item{SUMIF }{\link{sum_row_if}}
#' \item{MINIF }{\link{min_row_if}}
#' \item{MAXIF }{\link{max_row_if}}
#' \item{IFS }{\link{ifs}}
#' \item{IFNA }{\link{if_na}}
#' \item{MATCH }{\link{match_row}}
#' \item{INDEX }{\link{index_row}}
#' }
#' @section SPSS:
#' \itemize{
#' \item{COMPUTE }{\link{compute}}
#' \item{DO IF }{\link{do_if}}
#' \item{RECODE }{\link{recode}}
#' \item{COUNT }{\link{count_row_if}}
#' \item{VARIABLE LABELS }{\link{var_lab}}
#' \item{VALUE LABELS }{\link{val_lab}}
#' \item{ANY }{\link{any_in_row}}
#' \item{FREQUENCIES }{\link{fre}}
#' \item{CROSSTABS }{\link{cro}}
#' }
#' 
#' 
#' @docType package
#' @name expss
NULL


#' @import data.table
#' @import magrittr
#' @import foreign stats utils matrixStats


data.table = data.table::data.table
as.data.table = data.table::as.data.table
setkeyv = data.table::setkeyv
# fwrite = data.table::fwrite
# fread = data.table::fread
# '[.data.table' = data.table::`[.data.table`



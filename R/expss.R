#' expss: Tables with Labels and Some Useful Functions from Spreadsheets and SPSS Statistics
#' 
#' 'expss' package implements some popular functions from spreadsheets and SPSS 
#' Statistics software. Implementations are not complete copies of their 
#' originals. I try to make them consistent with other R functions. See examples
#' in the vignette and in the help.
#' 
#' @section Excel:
#' \itemize{
#' \item{IF }{\link[base]{ifelse}}
#' \item{AVERAGE }{\link{mean_row}}
#' \item{SUM }{\link{sum_row}}
#' \item{MIN }{\link{min_row}}
#' \item{MAX }{\link{max_row}}
#' \item{VLOOKUP }{\link[maditr]{vlookup}}
#' \item{COUNTIF }{\link{count_if}}
#' \item{AVERAGEIF }{\link{mean_row_if}}
#' \item{SUMIF }{\link{sum_row_if}}
#' \item{MINIF }{\link{min_row_if}}
#' \item{MAXIF }{\link{max_row_if}}
#' \item{IFS }{\link{ifs}}
#' \item{IFNA }{\link{if_na}}
#' \item{MATCH }{\link{match_row}}
#' \item{INDEX }{\link{index_row}}
#' \item{PIVOT TABLES }{\link{tables}, \link{cross_fun}, \link{cross_cpct}}
#' }
#' @section SPSS:
#' \itemize{
#' \item{RECODE }{\link{recode}}
#' \item{COUNT }{\link{count_row_if}}
#' \item{VARIABLE LABELS }{\link{var_lab}}
#' \item{VALUE LABELS }{\link{val_lab}}
#' \item{ANY }{\link{any_in_row}}
#' \item{FREQUENCIES }{\link{fre}}
#' \item{CROSSTABS }{\link{cro}}
#' \item{CUSTOM TABLES }{\link{tables}}
#' }
#' 
#' 
#' @docType package
#' @name expss
NULL


#' @import maditr
#' @import htmlTable 
#' @import stats utils matrixStats
#' @importFrom data.table data.table

data.table = maditr::data.table
as.data.table = maditr::as.data.table
setkeyv = maditr::setkeyv
# fwrite = data.table::fwrite
# fread = data.table::fread
# '[.data.table' = data.table::`[.data.table`


#' @export
htmlTable::htmlTable

#' @export
htmlTable::htmlTableWidget

#' @export
htmlTable::htmlTableWidgetOutput

#' @export
htmlTable::interactiveTable



.onAttach = function(...) {
    rnd = runif(1)
    if(rnd<0.2){
        packageStartupMessage("\nUse 'expss_output_viewer()' to display tables in the RStudio Viewer.\n To return to the console output, use 'expss_output_default()'.\n")
    }
    if(rnd>0.8){
        packageStartupMessage("\nUse 'expss_output_rnotebook()' to display tables inside R Notebooks.\n To return to the console output, use 'expss_output_default()'.\n")
    }
}

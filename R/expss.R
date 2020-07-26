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
#' \item{PIVOT TABLES }{\link{tables}, \link{cro_fun}, \link{cro}}
#' }
#' @section SPSS:
#' \itemize{
#' \item{COMPUTE }{\link{compute}}
#' \item{RECODE }{\link{recode}}
#' \item{COUNT }{\link{count_row_if}}
#' \item{DO IF }{\link{do_if}}
#' \item{DO REPEAT }{\link{do_repeat}}
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


#' @import data.table
#' @import htmlTable 
#' @import magrittr
#' @import foreign stats utils matrixStats


data.table = data.table::data.table
as.data.table = data.table::as.data.table
setkeyv = data.table::setkeyv
# fwrite = data.table::fwrite
# fread = data.table::fread
# '[.data.table' = data.table::`[.data.table`


#' @export
magrittr::`%>%`

#' @export
magrittr::`%<>%`

#' @export
magrittr::`%$%`

#' @export
data.table::data.table

#' @export
data.table::as.data.table

#' @export
data.table::is.data.table

#' @export
data.table::setDF

#' @export
data.table::setDT

#' @export
data.table::setkey

#' @export
data.table::setkeyv

#' @export
data.table::fread

#' @export
data.table::fwrite

#' @export
data.table::shift

#' @export
data.table::first

#' @export
data.table::last


#' @export
data.table::setorder

#' @export
data.table::setcolorder

#' @export
data.table::between

#' @export
data.table::chgroup

#' @export
data.table::chmatch

#' @export
data.table::chorder

#' @export
data.table::CJ

#' @export
data.table::fintersect

#' @export
data.table::foverlaps

#' @export
data.table::frank

#' @export
data.table::frankv

#' @export
data.table::fsetdiff

#' @export
data.table::fsetequal

#' @export
data.table::fsort

#' @export
data.table::funion

#' @export
data.table::getDTthreads


#' @export
data.table::haskey

#' @export
data.table::indices

#' @export
data.table::inrange

#' @export
data.table::key


#' @export
data.table::rbindlist

#' @export
data.table::rleid

#' @export
data.table::rleidv

#' @export
data.table::rowid

#' @export
data.table::rowidv

#' @export
data.table::set

#' @export
data.table::setattr

#' @export
data.table::setcolorder

#' @export
data.table::setDTthreads

#' @export
data.table::setindex

#' @export
data.table::setindexv

#' @export
data.table::setkey

#' @export
data.table::setkeyv

#' @export
data.table::setnames


#' @export
data.table::setorder

#' @export
data.table::setorderv

#' @export
data.table::SJ

#' @export
data.table::transpose

#' @export
data.table::tstrsplit

#' @export
data.table::uniqueN


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

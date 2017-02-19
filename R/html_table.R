#' Title
#'
#' @param x a data object (result of \link{fre}/\link{cro} and etc)
#' @param digits integer If it is not NULL than all numeric columns will be
#'   rounded to specified number of digits.
#' @param ... further parameters for \link[htmlTable]{htmlTable}
#'
#' @return Returns a string of class htmlTable
#' @export
#'
#' @examples
#' a = 1
htmlTable.simple_table = function(x, digits = NULL, ...){
    if(!is.null(digits)){
        for (i in seq_len(NCOL(x))){
            if(is.numeric(x[[i]])){
                x[[i]] = round(x[[i]], digits)
            }
        }
    }
    stopif(ncol(x)<2, "'x' should have at least two columns.")
    NextMethod("htmlTable")
    
}

#' @rdname htmlTable.simple_table
#' @export
knit_print.simple_table = function(x, digits = NULL, ...){
    knitr::asis_output(htmlTable(x, digits = digits, ...))
    
}

#' @rdname htmlTable.simple_table
#' @export
knit_print.etable = function(x, digits = NULL, ...){
    knitr::asis_output(htmlTable(x, digits = digits, ...))    
}
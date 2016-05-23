#' Get range of variables/variables by pattern
#'
#' @param start character Name of start variable (e. g. a_1)
#' @param end character Name of start variable (e. g. a_5)
#' @param e1 unquoted name of start variable (e. g. a_1)
#' @param e2 unquoted name of start variable (e. g. a_5) 
#'
#' @return  list of variables
#'
#' @examples
#' a = 1
#' @export
var_range = function(start, end){
    stopif(length(start)!=1 | length(end)!=1, "Length of 'start' and 'end' arguments shoud be equal to 1. 
           But length(start)=", length(start), " and length(end) = ", length(end) )
    stopif(!all(grepl("^(.+?)([\\d]+)$", c(start, end), perl = TRUE)),
           "'start' or 'end' arguments doesn't have correct pattern: ", start, " ", end)
    patt1 = gsub("^(.+?)([\\d]+)$", "\\1", start, perl = TRUE)
    patt2 = gsub("^(.+?)([\\d]+)$", "\\1", end, perl = TRUE)
    stopif(patt1!=patt2, "Start and end variables begin from different patterns: ", patt1, " ", patt2)
    digits1 = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", start, perl = TRUE))
    digits2 = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", end, perl = TRUE))
    stopif(digits1>digits2, "Name of start variables greater than name of end variables: ", start," ",end)
    
    scope = 1
    e = parent.frame(scope)
    var_names = ls(name = e, pattern = paste0("^", patt1, "[0-9]+$"))
   
    while(length(var_names)==0 & !identical(e, globalenv())){
        scope = scope + 1
        e = parent.frame(scope)
        var_names = ls(name = e, pattern = paste0("^", patt1, "[0-9]+$"))  
    }  
    
    stopif(length(var_names)==0, "Variables not found: ", start," ",end)
    digits = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", var_names, perl = TRUE))
    var_names = var_names[order(digits)]
    digits = digits[order(digits)]
    var_names = var_names[digits>=digits1 & digits<=digits2] 
    mget(var_names, envir = e)

}

#' @export
#' @rdname var_range
'%to%' = function(e1, e2){
    e1 = as.character(substitute(e1))
    e2 = as.character(substitute(e2))
    var_range(e1, e2)
}









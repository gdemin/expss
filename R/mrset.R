#' Create multiple response set/multiple dichotomy set from variables 
#' 
#' This functions are intended for usage with tables - \link{table_cases}, 
#' \link{table_summary}. Result of \code{mrset} is considered as 
#' muliple-response set with category encoding and result of \code{mdset} is 
#' considered as multiple response set with dichotomy encoding e. g. with 0 or 1
#' in the each column. Each column in the \code{dichotomy} is indicator of 
#' absense or presense of particular feature. Both functions don't convert its 
#' arguments to anything - it is supposed that arguments already have 
#' appropriate encoding. For conversation see \link{dichotomy} or
#' \link{category}.
#'
#' @param ... variables
#'
#' @return data.frame of class \code{category}/\code{dichotomy}
#' @export
#'
#' @examples
#' 
#' data(product_test)
#' # a1 = with(product_test, mrset(a2_1 %to% a2_99))
#' 
mrset = function(...){
    args = list(...)
    stopif(!length(args), "`mrset` - you should provide at least one argument.")
    if(length(args)==1){
        res = args[[1]]
        if(!is.data.frame(res)){
            res = as.dtfrm(res)
        }
    } else {
        res = as.dtfrm(args)
    }
    class(res) = union("category", class(res) %d% "dichotomy")
    res

}

#' @export
#' @rdname mrset
mdset = function(...){
    args = list(...)
    stopif(!length(args), "`mdset` - you should provide at least one argument.")
    if(length(args)==1){
        res = args[[1]]
        if(!is.data.frame(res)){
            res = as.dtfrm(res)
        }
    } else {
        res = as.dtfrm(args)
    }
    empty = !(res %in_row% c(1, TRUE))
    na_if(res) = empty
    if_val(res[!empty,]) = c(c(NA, 0) ~ 0, other ~ 1)
    class(res) = union("dichotomy", class(res) %d% "category")
    res
}
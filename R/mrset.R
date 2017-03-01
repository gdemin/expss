#' Create multiple response set/multiple dichotomy set from variables 
#' 
#' These functions are intended for usage with tables - \link{table_cases}, 
#' \link{table_summary}. Result of \code{mrset} is considered as 
#' muliple-response set with category encoding and result of \code{mdset} is 
#' considered as multiple response set with dichotomy (dummy) encoding e. g.
#' with 0 or 1 in the each column. Each column in the \code{dichotomy} is
#' indicator of absense or presense of particular feature. Both functions don't
#' convert its arguments to anything - it is supposed that arguments already
#' have appropriate encoding. For conversation see \link{as.dichotomy} or 
#' \link{as.category}.
#'
#' @param ... variables
#' @param label character optional label for multiple response set
#'
#' @return data.frame of class \code{category}/\code{dichotomy}
#' @seealso \link{as.dichotomy}, \link{as.category}
#' @export
#'
#' @examples
#' 
#' data(product_test)
#' a1 = calculate(product_test, mrset(a1_1 %to% a1_6))
#' 
mrset = function(..., label = NULL){
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
    if(!is.null(label)){
        var_lab(res[[1]]) = label
    }
    class(res) = union("category", class(res) %d% "dichotomy")
    res

}

#' @export
#' @rdname mrset
mdset = function(..., label = NULL){
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
    if_val(res) = c(c(NA, 0) ~ 0, other ~ 1)
    empty = !(res %row_in% 1)
    na_if(res) = empty
    if(!is.null(label)){
        for(each in seq_along(res)){
            curr_lab = var_lab(res[[each]])
            if(!is.null(curr_lab) && !(curr_lab=="")){
                var_lab(res[[each]]) = paste0(label, "|", curr_lab) 
            } else {
                var_lab(res[[each]]) = paste0(label, "|", names(res)[each])
            }
        }
    }
    class(res) = union("dichotomy", class(res) %d% "category")
    res
}
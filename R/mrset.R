#' Create multiple response set/multiple dichotomy set from variables 
#' 
#' These functions are intended for usage with tables - \link{tables},
#' \link{cro}, \link{cro_fun}. Result of \code{mrset} is considered as 
#' muliple-response set with category encoding and result of \code{mdset} is 
#' considered as multiple response set with dichotomy (dummy) encoding e. g. 
#' with 0 or 1 in the each column. Each column in the \code{dichotomy} is 
#' indicator of absense or presense of particular feature. Both functions don't 
#' convert its arguments to anything - it is supposed that arguments already 
#' have appropriate encoding. For conversation see \link{as.dichotomy} or 
#' \link{as.category}. \code{mrset_p} and \code{mdset_p} select variables for
#' multiple-responses by perl-style regular expresssion.
#' \code{mrset_f} and \code{mdset_f} select variables by fixed pattern. Fixed
#' pattern can be unquoted. For details see \link{..p}.
#'
#' @param ... variables
#' @param label character optional label for multiple response set
#'
#' @return data.frame of class \code{category}/\code{dichotomy}
#' @seealso \link{as.dichotomy}, \link{as.category}
#' @export
#'
#' @examples
#' data(product_test)
#' 
#' calc_cro_cpct(product_test, mrset(a1_1 %to% a1_6))
#' 
#' # same result
#' calc_cro_cpct(product_test, mrset_f(a1_))
#' 
#' # same result
#' calc_cro_cpct(product_test, mrset_p("a1_"))
mrset = function(..., label = NULL){
    args = list(...)
    stopif(!length(args), "`mrset` - you should provide at least one argument.")
    if(length(args)==1 && is.data.frame(args[[1]])){
        res = args[[1]]
    } else {
        res = sheet(...)
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
    if(length(args)==1 && is.data.frame(args[[1]])){
        res = args[[1]]
    } else {
        res = sheet(...)
    }
    if_val(res) = c(c(NA, 0) ~ 0, other ~ 1)
    res = make_labels_from_names(res)
    if(!is.null(label)){
        for(each in seq_along(res)){
            # we have labels in any case because we made them from names
            curr_lab = var_lab(res[[each]])  
            var_lab(res[[each]]) = paste0(label, "|", curr_lab) 
        }
    }
    class(res) = union("dichotomy", class(res) %d% "category")
    res
}

#' @export
#' @rdname mrset
mrset_f = function(..., label = NULL){
    res = eval.parent(substitute(..f(...)))
    do.call(mrset, c(list(res), list(label = label)))
}

#' @export
#' @rdname mrset
mdset_f = function(..., label = NULL){
    res = eval.parent(substitute(..f(...)))
    do.call(mdset, c(list(res), list(label = label)))
}

#' @export
#' @rdname mrset
mrset_p = function(..., label = NULL){
    res = eval.parent(substitute(..p(...)))
    do.call(mrset, c(list(res), list(label = label)))    
}

#' @export
#' @rdname mrset
mdset_p = function(..., label = NULL){
    res = eval.parent(substitute(..p(...)))
    do.call(mdset, c(list(res), list(label = label)))    
}
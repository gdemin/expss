#' Make data.frame without conversion to factors and without fixing names
#' 
#' \code{dtfrm} and \code{as.dtfrm} are shortcuts to \code{data.frame} and 
#' \code{as.data.frame} with stringsAsFactors = FALSE, check.names = FALSE.
#' \code{lst} creates list with names.
#'
#' @param ... objects, possibly named
#' @param x object to be coerced 
#'
#' @return data.frame/list
#' @export
#' @seealso \link[base]{data.frame}, \link[base]{as.data.frame},  \link[base]{list}
#' @examples
#' 
#' # see the difference
#' df1 = data.frame(a = letters[1:3], "This is my long name" = 1:3)
#' df2 = dtfrm(a = letters[1:3], "This is my long name" = 1:3)
#' 
#' str(df1)
#' str(df2)
#' 
#' # lst
#' a = 1:3
#' b = 3:1
#' 
#' list1 = list(a, b)
#' list2 = lst(a, b)
#' 
#' str(list1)
#' str(list2)
#' 
dtfrm = function(...){
    data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)
}

#' @export
#' @rdname dtfrm
as.dtfrm = function(x, ...) {
    as.data.frame(x, optional = FALSE, check.names = FALSE, ...,
                  cut.names = FALSE, col.names = names(x),
                  stringsAsFactors = FALSE)
    
}

#' @export
#' @rdname dtfrm
lst = function(...){
    res = list(...)
    names(res) = sapply(as.list(substitute(list(...)))[-1], deparse)
    res
 
}
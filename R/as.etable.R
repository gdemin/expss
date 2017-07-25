#' Convert data.frame/matrix to object of class 'etable'
#' 
#' If \code{x} is \code{data.frame} then \code{as.etable} just adds
#' \code{etable} to \code{class} attribute of \code{x}. If \code{x} is matrix
#' then it will be converted to data.frame.
#'
#' @param x data.frame/matrix
#'
#' @return object of class \code{etable}
#' @export
#'
#' @examples
#' data(mtcars)
#' etable_mtcars = as.etable(mtcars)
#' is.etable(etable_mtcars) #TRUE
#' 
#' etable_mtcars #another 'print' method is used
as.etable = function(x){
    UseMethod("as.etable")
}

#' @export
as.etable.data.frame = function(x){
    class(x) = union("etable", class(x))
    x
}

#' @export
as.etable.matrix = function(x){
    res = as.dtfrm(x)
    if(is.null(colnames(x))){
        colnames(res) = rep("", NCOL(res))
    }
    as.etable(res)
}

#' @export
#' @rdname as.etable
is.etable = function(x){
    inherits(x, "etable")
}
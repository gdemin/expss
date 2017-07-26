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
as.etable.table = function(x){
    res = do.call(expand.grid, c(dimnames(provideDimnames(x, unique = FALSE)), 
                                 KEEP.OUT.ATTRS = FALSE))
    res = prepend_names(res)
    weight = c(x)
    if(NCOL(res)<2){
        cro(cell_vars = res[[1]], 
            col_vars = list("|"),
            weight = weight,
            total_row_position = "none")
    } else {
        if(NCOL(res)<3){
            cro(cell_vars = res[[1]], 
                col_vars = res[[2]], 
                weight = weight,
                total_row_position = "none")
        } else {
            rowvars = res
            rowvars[, 1:2] = NULL
            rowvars = rowvars[, rev(seq_len(NCOL(rowvars))), drop = FALSE]
            if(NCOL(rowvars)>1){
                rowvars = do.call(nest, rowvars)
            }
            cro(cell_vars = res[[1]], 
                col_vars = res[[2]], 
                row_vars = rowvars,
                weight = weight,
                total_row_position = "none"
                )
        }
    }
}

#' @export
#' @rdname as.etable
is.etable = function(x){
    inherits(x, "etable")
}
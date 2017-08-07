#' Convert data.frame/matrix to object of class 'etable'
#' 
#' If \code{x} is \code{data.frame} then \code{as.etable} just adds
#' \code{etable} to \code{class} attribute of \code{x}. If \code{x} is matrix
#' then it will be converted to data.frame.
#'
#' @param x data.frame/matrix
#' @param rownames_as_row_labels logical. If it is TRUE than rownames of 
#'   \code{x} will be added to result as first column with name 
#'   \code{row_labels}. By default row names will be added if they are not NULL
#'   and are not sequential numerics.
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
#' 
#' cor(mtcars) %>% as.etable()
as.etable = function(x, rownames_as_row_labels = NULL){
    UseMethod("as.etable")
}

#' @export
as.etable.etable = function(x, rownames_as_row_labels = NULL){
    x
}

#' @export
as.etable.default = function(x, rownames_as_row_labels = NULL){
    as.etable(as.dtfrm(x), rownames_as_row_labels = rownames_as_row_labels)
}

#' @export
as.etable.data.frame = function(x, rownames_as_row_labels = NULL){
    rownames_as_row_labels = if_null(rownames_as_row_labels, has_rownames(x))
    if(rownames_as_row_labels){
        x = dtfrm(row_labels = rownames(x), x)
    }
    class(x) = union("etable", class(x))
    x
}

#' @export
as.etable.matrix = function(x, rownames_as_row_labels = NULL){
    res = as.dtfrm(x)
    if(is.null(colnames(x))){
        colnames(res) = rep("", NCOL(res))
    }
    as.etable(res, rownames_as_row_labels = rownames_as_row_labels)
}

# return FALSE if rownames are trivial
has_rownames = function(x){
   curr  = rownames(x)
   !(is.null(curr) |
       identical(as.character(curr), as.character(seq_len(NROW(x))))
   )
}

#' @export
as.etable.table = function(x, rownames_as_row_labels = NULL){
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
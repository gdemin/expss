#' Subsetting Data Frames
#'
#' \code{cond} will be evaluated in the context of the data frame, so columns can be referred to (by
#' name) as variables in the expression (see the examples).
#' \code{.where} is version for working with default dataset. See \link{default_dataset}.
#'  \code{\%where\%} is infix function with the same functional. See examples.
#' 
#' @param data data.frame to be subsetted
#' @param cond logical or numeric expression indicating elements or rows to
#'   keep: missing values (NA) are taken as false.
#'
#' @return data.frame which contains just selected rows.
#' @export
#'
#' @examples
#' # leave only 'setosa'
#' where(iris, Species == "setosa")
#' # leave only first five rows
#' where(iris, 1:5)
#' 
#' # infix version
#' # note that '%where%' have higher precendence than '=='
#' # so we need to put condition inside brackets
#' iris %where% (Species == "setosa")
#' 
#' iris %where% 1:5
#' 
where = function (data, cond) {
    UseMethod("where")
}

#' @export
where.data.frame = function (data, cond) {
    parent = parent.frame()
    cond = substitute(cond)
    cond = eval(cond, data, parent.frame())
    if (!is.logical(cond) && !is.numeric(cond)){ 
        stop("'cond' must be logical or numeric.")
    }    
    if(is.logical(cond)) cond = cond & !is.na(cond)
    data[cond,, drop = FALSE]
}

#' @rdname where
#' @export
'%where%' = function(data, cond){
    parent = parent.frame()
    cond = substitute(cond)
    cond = eval(cond, data, parent.frame())
    if (!is.logical(cond) && !is.numeric(cond)){ 
        stop("'cond' must be logical or numeric.")
    }    
    if(is.logical(cond)) cond = cond & !is.na(cond)
    data[cond,, drop = FALSE]
}


#' @rdname where
#' @export
.where = function (cond) {
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    cond = substitute(cond)
    cond = eval(cond, data, parent.frame())
    if (!is.logical(cond) && !is.numeric(cond)){ 
        stop("'cond' must be logical or numeric.")
    }    
    if(is.logical(cond)) cond = cond & !is.na(cond)
    new_data = data[cond,, drop = FALSE]
    ref(reference) = new_data
    invisible(NULL)
}


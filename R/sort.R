#' Sort data.frames/matrices/vectors
#' 
#' \code{sort_asc} sorts in ascending order and \code{sort_desc} sorts in 
#' descending order. \code{.sort_asc}/\code{.sort_desc} are versions for working
#' with \link{default_dataset}.
#'
#' @param data data.frame/matrix/vector
#' @param ... character/numeric or criteria/logical functions (see
#'   \link{criteria}). Column names/numbers for data.frame/matrix by which
#'   object will be sorted. Names at the top-level can be unquoted (non-standard
#'   evaluation). For standard evaluation of parameters you can surround them by
#'   round brackets. See examples. Ignored for vectors.
#' @param na.last for controlling the treatment of NAs. If TRUE, missing values
#'   in the data are put last; if FALSE, they are put first; if NA, they are
#'   removed.
#'
#' @return sorted \code{data}
#' @export
#'
#' @examples
#' data(mtcars)
#' sort_asc(mtcars, mpg)
#' sort_asc(mtcars, cyl, mpg) # by two column
#' 
#' # same results with column nums
#' sort_asc(mtcars, 1)
#' sort_asc(mtcars, 2:1) # by two column
#' sort_asc(mtcars, 2, 1) # by two column
#' 
#' # call with parameter
#' sorting_columns = c("cyl", "mpg")
#' sort_asc(mtcars, (sorting_columns)) 
#' 
sort_asc = function(data, ..., na.last = FALSE){
    UseMethod("sort_asc")
}

#' @export
sort_asc.default = function(data, ..., na.last = FALSE){
    sort(data, decreasing = FALSE, na.last = na.last, ...)
}

#' @export
sort_asc.data.frame = function(data, ..., na.last = FALSE){
    sort_internal(data, ..., decreasing = FALSE, na.last = na.last, envir = parent.frame())
}

#' @export
sort_asc.matrix = function(data, ..., na.last = FALSE){
    sort_internal(data, ..., decreasing = FALSE, na.last = na.last, envir = parent.frame())
}


sort_internal = function(data, ..., decreasing, na.last, envir){
    variables_names = substitute(list(...))
    curr_names = colnames(data)
    if(is.null(curr_names)){
        curr_names = rep("", NCOL(data))
    }
    var_indexes = variables_names_to_indexes(curr_names, variables_names, envir = envir)
    stopif(length(var_indexes)==0, 
           "Column name or column number should be provided for data.frame/matrix.")
    if(is.matrix(data)){
        columns_list = lapply(var_indexes, function(each_col) data[, each_col]) 
    } else {
        columns_list = as.list(data[, var_indexes, drop = FALSE])   
    }
    new_order = do.call(order, c(columns_list, decreasing = decreasing, na.last = na.last))
    data[new_order, , drop = FALSE]    
}

#' @export
sort_asc.list= function(data, ..., na.last = FALSE){
    stop("Sorting not yet implemented for lists.")
}




#' @rdname sort_asc
#' @export
.sort_asc = function(..., na.last = FALSE){
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    data = sort_internal(data, ..., decreasing = FALSE, na.last = na.last, envir = parent.frame())
    ref(reference) = data
    invisible(data)
}


######### sort_desc ##############

#' @rdname sort_asc
#' @export
sort_desc = function(data, ..., na.last = TRUE){
    UseMethod("sort_desc")
}

#' @rdname sort_asc
#' @export
.sort_desc = function(..., na.last = TRUE){
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    data = sort_internal(data, ..., decreasing = TRUE, na.last = na.last, envir = parent.frame())
    ref(reference) = data
    invisible(data)
}

#' @export
sort_desc.default = function(data, ..., na.last = TRUE){
    sort(data, decreasing = TRUE, na.last = na.last, ...)
}

#' @export
sort_desc.data.frame = function(data, ..., na.last = TRUE){
    sort_internal(data, ..., decreasing = TRUE, na.last = na.last, envir = parent.frame())
}

#' @export
sort_desc.matrix = function(data, ..., na.last = TRUE){
    sort_internal(data, ..., decreasing = TRUE, na.last = na.last, envir = parent.frame())
}

#' @export
sort_desc.list= function(data, ..., na.last = TRUE){
    stop("Sorting in not implemented for lists.")
}




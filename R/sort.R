#' Sort data.frames/matrices/vectors
#' 
#' \code{sort_asc} sorts in ascending order and \code{sort_desc} sorts in 
#' descending order. There is no non-standard evaluation in these functions by 
#' design so use quotes for names of your variables or use \link{qc}.
#' \code{\%sort_asc\%}/\code{\%sort_desc\%} are infix versions of these functions. 
#' \code{.sort_asc}/\code{.sort_desc} are versions for working with
#' \link{default_dataset}.
#'
#' @param data data.frame/matrix/vector
#' @param ... character/numeric. Column names/numbers for data.frame/matrix by
#'   which object will be sorted. Ignored for vectors.
#' @param variables character/numeric. Column names/numbers for data.frame/matrix by
#'   which object will be sorted for infix functions. Ignored for vectors.
#' @param na.last for controlling the treatment of NAs. If TRUE, missing values
#'   in the data are put last; if FALSE, they are put first; if NA, they are
#'   removed.
#'
#' @return sorted \code{data}
#' @export
#'
#' @examples
#' data(mtcars)
#' sort_asc(mtcars, "mpg")
#' sort_asc(mtcars, "cyl", "mpg") # by two column
#' 
#' # same results with column nums
#' sort_asc(mtcars, 1)
#' sort_asc(mtcars, 2:1) # by two column
#' sort_asc(mtcars, 2, 1) # by two column
#' 
#' # 'qc'  usage
#' sort_asc(mtcars, qc(cyl, mpg)) 
#' 
#' # infix version
#' mtcars %sort_asc% "mpg"
#' mtcars %sort_asc% c("cyl", "mpg")
#' mtcars %sort_asc% qc(cyl, mpg) 
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
    sort_internal(data, ..., decreasing = FALSE, na.last = na.last)
}

#' @export
sort_asc.matrix = function(data, ..., na.last = FALSE){
    sort_internal(data, ..., decreasing = FALSE, na.last = na.last)
}


sort_internal = function(data, ..., decreasing, na.last){
    args = list(...)
    if(any(sapply(args, is.list))){
        args = unlist(args, recursive = FALSE)
    }
    numeric_args = args 
    colnum = ncol(data)    
    varnames = colnames(data)
    for(each in seq_along(args)){
        curr = args[[each]]
        if(is.character(curr) & length(curr)>0){
            stopif(!all(curr %in% varnames), "some column names don't exist in 'data': ", curr %d% varnames)
            numeric_args[[each]] = match(curr, varnames)        
        }
    }
    numeric_args = unlist(numeric_args)
    stopif(length(args)==0, "Column name or column number should be provided for data.frame/matrix.")
    stopif(max(numeric_args, na.rm = TRUE) > colnum,
        "some column numbers are larger than number of columns in 'data': ", numeric_args[numeric_args>colnum])
    if(is.matrix(data)){
        columns_list = lapply(numeric_args, function(each_col) data[, each_col]) 
    } else {
        columns_list = lapply(numeric_args, function(each_col) data[[each_col]])   
    }
    new_order = do.call(order, c(columns_list, decreasing = decreasing, na.last = na.last))
    data[new_order, , drop = FALSE]    
}

#' @export
sort_asc.list= function(data, ..., na.last = FALSE){
    stop("Sorting in not implemented for lists.")
}

#' @rdname sort_asc
#' @export
'%sort_asc%' = function(data, variables){
    sort_asc(data, variables)
}


#' @rdname sort_asc
#' @export
.sort_asc = function(..., na.last = FALSE){
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    ref(reference) = sort_asc(data, ..., na.last = na.last)
    invisible(NULL)
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
    ref(reference) = sort_desc(data, ..., na.last = na.last)
    invisible(NULL)
}

#' @export
sort_desc.default = function(data, ..., na.last = TRUE){
    sort(data, decreasing = TRUE, na.last = na.last, ...)
}

#' @export
sort_desc.data.frame = function(data, ..., na.last = TRUE){
    sort_internal(data, ..., decreasing = TRUE, na.last = na.last)
}

#' @export
sort_desc.matrix = function(data, ..., na.last = TRUE){
    sort_internal(data, ..., decreasing = TRUE, na.last = na.last)
}

#' @export
sort_desc.list= function(data, ..., na.last = TRUE){
    stop("Sorting in not implemented for lists.")
}

#' @rdname sort_asc
#' @export
'%sort_desc%' = function(data, variables){
    sort_desc(data, variables)
}






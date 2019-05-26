#' Match finds value in rows or columns/index returns value by index from rows or columns
#' 
#' \code{match} finds value in rows or columns. \code{index} returns value by index
#' from row or column. One can use functions as criteria for \code{match}. In 
#' this case position of first value on which function equals to TRUE will be 
#' returned. For convenience there are special predefined functions - see
#' \link{criteria}. If value is not found then NA will be returned. 
#'
#' @param criterion Vector of values to be matched, or
#'   function. 
#' @param index vector of positions in rows/columns from which values should be returned.
#' @param ... data. Vectors, matrixes, data.frames, lists. Shorter arguments
#'   will be recycled.
#'
#' @return vector with length equals to number of rows for *_row and equals to
#'   number of columns for *_col.
#'
#' @examples
#' # toy data
#' v1 = 1:3
#' v2 = 2:4
#' v3 = 7:5
#' 
#' # postions of 1,3,5 in rows
#' match_row(c(1, 3, 5), v1, v2, v3) # 1:3
#' # postions of 1,3,5 in columns
#' match_col(1, v1, v2, v3) # c(v1 = 1, v2 = NA, v3 = NA)
#' 
#' # postion of first value greater than 2
#' ix = match_row(gt(2), v1, v2, v3) 
#' ix # c(3,2,1)
#' # return values by result of previous 'match_row' 
#' index_row(ix, v1, v2, v3) # c(7,3,3)
#' 
#' # the same actions with data.frame
#' dfs = data.frame(v1, v2, v3)
#' 
#' # postions of 1,3,5 in rows
#' match_row(c(1, 3, 5), dfs) # 1:3
#' # postions of 1,3,5 in columns
#' match_col(1, dfs) # c(v1 = 1, v2 = NA, v3 = NA)
#' 
#' # postion of first value greater than 2
#' ix = match_row(gt(2), dfs) 
#' ix # c(3,2,1)
#' # return values by result of previous 'match_row' 
#' index_row(ix, dfs) # c(7,3,3)
#' @export
match_row = function(criterion, ...){
    cond = build_condition_matrix(criterion, ...)
    res = numeric(nrow(cond))
    for(i in seq_len(nrow(cond))){
        res[i] = which(cond[i, ])[1]    
    }
    res
}


#' @export
#' @rdname match_row
match_col = function(criterion, ...){
    cond = build_condition_matrix(criterion, ...)
    res = numeric(ncol(cond))
    for(i in seq_len(ncol(cond))){
        res[i] = which(cond[, i])[1]    
    }
    names(res) = colnames(cond)
    res
    
}

#' @export
#' @rdname match_row
index_row = function(index, ...){
    data = dots2matrix(...)
    stopifnot(
        is.numeric(index) || is.logical(index),
        length(index)==1 || length(index) == nrow(data),
        all(index<=ncol(data), na.rm = TRUE)
    )
    if(is.logical(index)) index = as.numeric(index)
    matrixStats::rowCollapse(data, index)
    
}

#' @export
#' @rdname match_row
index_col = function(index, ...){
    data = dots2matrix(...)
    stopifnot(
        is.numeric(index) || is.logical(index),
        length(index)==1 || length(index) == ncol(data),
        all(index<=nrow(data), na.rm = TRUE)
    )
    if(is.logical(index)) index = as.numeric(index)
    res = matrixStats::colCollapse(data, index)
    names(res) = colnames(data)
    res
    
}


#' @export
#' @rdname match_row
value_row_if = function(criterion, ...){
    index_row(match_row(criterion, ...), ...)
}

#' @export
#' @rdname match_row
value_col_if = function(criterion, ...){
    index_col(match_col(criterion, ...), ...)
}
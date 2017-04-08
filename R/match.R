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
    stopif(length(criterion)==0, "Zero-length 'criterion'.")
    dfs = dots2data_frame(...) 
    if(is.function(criterion)){
        cond = build_criterion(criterion, dfs)
        apply(cond, 1, function(x) which(x)[1])
    } else {
        if(length(criterion)>1){
            stopif(length(criterion)!=nrow(dfs), "Length of 'criterion' should be
               1 or equals number of rows of '...' but length(criterion)=",length(criterion),", NROW(...)=", NROW(dfs))
            unname(unlist(lapply(seq_along(criterion), function(row_num) match(criterion[row_num], unlist(dfs[row_num, ])))))
        } else {
            res = apply(dfs, 1, function(row) match(criterion, row))
            unname(res)
        }    
    } 
    
}


#' @export
#' @rdname match_row
match_col = function(criterion, ...){
    stopif(length(criterion)==0, "Zero-length 'criterion'.")
    dfs = dots2data_frame(...)   
    if(is.function(criterion)){
        cond = build_criterion(criterion, dfs)
        res = apply(cond, 2, function(x) which(x)[1])
        stats::setNames(res, colnames(dfs))
    } else {
        if(length(criterion)>1){
            stopif(length(criterion)!=nrow(dfs), "Length of 'criterion' should be
                   1 or equals number of columns of '...' but length(criterion)=",length(criterion),", NCOL(...)=", NCOL(dfs))
            res = unlist(lapply(seq_along(criterion), function(col_num) match(criterion[col_num], dfs[[col_num]])))
            stats::setNames(res, colnames(dfs))
        } else {
            res = apply(dfs, 2, function(col) match(criterion, col))
            stats::setNames(res, colnames(dfs))
        }    
    } 
    
}

#' @export
#' @rdname match_row
index_row = function(index, ...){
    stopif(length(index)==0, "Zero-length 'index'.")
    dfs = dots2data_frame(...) 
    max_index = max_col(index)
    stopif(!is.na(max_index) && max(index, na.rm = TRUE)>ncol(dfs), "Max 'index' should be
               less than  or equals to number of columns in  '...' but max(index)=",max(index, na.rm = TRUE),", NCOL(...)=", ncol(dfs))
    if(length(index)==1){
        if(!is.na(index)){
            dfs[[index]]
        } else {
           rep(NA, NROW(dfs)) 
        }
    } else {
        stopif(length(index)!=nrow(dfs), "Length of 'index' should be
               1 or equals number of rows of '...' but length(index)=",length(index),", NROW(...)=", NROW(dfs))
        
        # unlist(dfs[row_num, ])[index[row_num]] because if value of index is NA we want NA as a result
        unname(unlist(lapply(seq_along(index), function(row_num) unlist(dfs[row_num, ])[index[row_num]])))
    }
    
}

#' @export
#' @rdname match_row
index_col = function(index, ...){
    stopif(length(index)==0, "Zero-length 'index'.")
    dfs = dots2data_frame(...)
    max_index = max_col(index)
    stopif(!is.na(max_index) && max_index>nrow(dfs), "Max 'index' should be
               less than or equals to number of rows in  '...' but max(index)=",max(index, na.rm = TRUE),", NROW(...)=", nrow(dfs))
    if(length(index)==1){
        if(!is.na(index)){
            unlist(dfs[index,]) 
        } else {
            stats::setNames(rep(NA, NROW(dfs)), colnames(dfs))
        }    
    } else {
        stopif(length(index)!=ncol(dfs), "Length of 'index' should be
               1 or equals number of columns of '...' but length(index)=",length(index),", NCOL(...)=", NCOL(dfs))
        res = unlist(lapply(seq_along(index), function(col_num) dfs[[col_num]][index[col_num]]))
        stats::setNames(res, colnames(dfs))
    }
    
}
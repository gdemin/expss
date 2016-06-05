#' Match finds value in row or column/index returns value by index from row or column
#'
#' @param criterion Vector with values to be matched, or
#'   function. 
#' @param index vector of positions in rows/columns from which values should be returned.
#' @param ... data. Vectors, matrixes, data.frames, list. Shorter arguments
#'   will be recycled.
#'
#' @return vector with length equals to number of rows for *_row and equals to
#'   number of columns for *_col.
#'
#' @examples
#' 
#' a = 1
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
        setNames(res, colnames(dfs))
    } else {
        if(length(criterion)>1){
            stopif(length(criterion)!=nrow(dfs), "Length of 'criterion' should be
                   1 or equals number of columns of '...' but length(criterion)=",length(criterion),", NCOL(...)=", NCOL(dfs))
            res = unlist(lapply(seq_along(criterion), function(col_num) match(criterion[col_num], dfs[[col_num]])))
            setNames(res, colnames(dfs))
        } else {
            res = apply(dfs, 2, function(col) match(criterion, col))
            setNames(res, colnames(dfs))
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
            setNames(rep(NA, NROW(dfs)), colnames(dfs))
        }    
    } else {
        stopif(length(index)!=ncol(dfs), "Length of 'index' should be
               1 or equals number of columns of '...' but length(index)=",length(index),", NCOL(...)=", NCOL(dfs))
        res = unlist(lapply(seq_along(index), function(col_num) dfs[[col_num]][index[col_num]]))
        setNames(res, colnames(dfs))
    }
    
}
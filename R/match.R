#' Match
#'
#' @param criterion 
#' @param ... 
#'
#' @return vector
#' @export
#'
#' @examples
#' 
#' a = 1
match_row = function(criterion, ...){
    stopif(is.logical(criterion), "Logical 'criterion' not yet implemented.")
    dfs = dots2data_frame(...)   
    cond = build_criterion(criterion, dfs)
    apply(cond, 1, function(x) which(x)[1])

}


#' @export
#' @rdname match_row
match_col = function(criterion, ...){
    stopif(is.logical(criterion), "Logical 'criterion' not yet implemented.")
    dfs = dots2data_frame(...)   
    cond = build_criterion(criterion, dfs)
    apply(cond, 2, function(x) which(x)[1])
    
}

#' @export
#' @rdname match_row
index_row = function(index, ...){
    stopif(length(index)==0, "Zero-length 'index'.")
    dfs = dots2data_frame(...) 
    stopif(max(index, na.rm = TRUE)>ncol(dfs), "Max 'index' should be
               less than number of columns in  '...' but max(index)=",max(index, na.rm = TRUE),", NCOL(...)=", ncol(dfs))
    if(length(index)<2){
        if(!is.na(index)){
            dfs[[index]]
        } else {
           rep(NA, NROW(dfs)) 
        }
    } else {
        stopif(length(index)!=nrow(dfs), "Length of 'index' should be
               1 or equals number of rows of '...' but length(index)=",length(index),", NROW(...)=", NROW(dfs))
        
        # unlist(dfs[row_num, ])[index[row_num]] - if value of index is NA we want NA as a result
        unlist(lapply(seq_along(index), function(row_num) unlist(dfs[row_num, ])[index[row_num]]))
    }
    
}

#' @export
#' @rdname match_row
index_col = function(index, ...){
    stopif(length(index)==0, "Zero-length 'index'.")
    dfs = dots2data_frame(...) 
    stopif(max(index, na.rm = TRUE)>nrow(dfs), "Max 'index' should be
               less than number of rows in  '...' but max(index)=",max(index, na.rm = TRUE),", NROW(...)=", nrow(dfs))
    if(length(index)<2){
        unlist(dfs[index,]) 
    } else {
        stopif(length(index)!=ncol(dfs), "Length of 'index' should be
               1 or equals number of columns of '...' but length(index)=",length(index),", NCOL(...)=", NCOL(dfs))
        unlist(lapply(seq_along(index), function(col_num) dfs[[col_num]][index[row_num]]))
    }
    
}
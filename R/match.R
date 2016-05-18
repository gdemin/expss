#' Title
#'
#' @param criterion 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
match_row = function(criterion, ...){
    dfs = dots2data_frame(...)   
    cond = build_criterion(criterion, dfs)
    apply(cond, 1, function(x) which(x)[1])

}


#' @export
#' @rdname match_row
match_col = function(criterion, ...){
    dfs = dots2data_frame(...)   
    cond = build_criterion(criterion, dfs)
    apply(cond, 2, function(x) which(x)[1])
    
}

#' @export
#' @rdname match_row
index_row = function(index, ...){
    dfs = dots2data_frame(...) 
    stopif(max(index, na.rm = TRUE)>ncol(dfs), "Max 'index' should be
               less than number of columns in  '...' but max(index)=",max(index, na.rm = TRUE),", NCOL(...)=", ncol(dfs))
    if(length(index)<2){
       dfs[[index]] 
    } else {
        stopif(length(index)!=nrow(dfs), "Length of 'index' should be
               1 or equals number of rows of '...' but length(index)=",length(index),", NROW(...)=", NROW(dfs))
        unlist(lapply(seq_along(index), function(row_num) dfs[[index[row_num]]][row_num]))
    }
    
}

#' @export
#' @rdname match_col
index_row = function(index, ...){
    stop("not implemented")
    dfs = dots2data_frame(...) 
    stopif(max(index, na.rm = TRUE)>ncol(dfs), "Max 'index' should be
               less than number of columns in  '...' but max(index)=",max(index, na.rm = TRUE),", NCOL(...)=", ncol(dfs))
    if(length(index)<2){
        dfs[[index]] 
    } else {
        stopif(length(index)!=nrow(dfs), "Length of 'index' should be
               1 or equals number of rows of '...' but length(index)=",length(index),", NROW(...)=", NROW(dfs))
        unlist(lapply(seq_along(index), function(row_num) dfs[[index[row_num]]][row_num]))
    }
    
}
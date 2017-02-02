drop_empty_rows = function(x, excluded_columns = 1, excluded_rows = grep("^#", x[[1]], perl= TRUE)){
    UseMethod("drop_empty_rows")
}

drop_empty_columns = function(x, excluded_columns = 1, excluded_rows = grep("^#", x[[1]], perl= TRUE)){
    UseMethod("drop_empty_columns")   
}

drop_empty_rows.data.frame = function(x, excluded_columns = 1, excluded_rows = grep("^#", x[[1]], perl= TRUE)){
    stopif(!is.null(excluded_columns) && !is.numeric(excluded_columns) && !is.character(excluded_columns) &&
               is.logical(excluded_columns),
           "`excluded_columns` should be character, numeric or logical.")
    stopif(!is.null(excluded_rows) && !is.numeric(excluded_rows) && !is.logical(excluded_rows),
           "`excluded_columns` should be numeric or logical.")
    if(is.null(excluded_columns)) {
        not_empty = rowSums(!is.na(x))>0    
    } else {
        if(is.character(excluded_columns)) excluded_columns = colnames(x) %in%  excluded_columns
        if(is.numeric(excluded_columns)) excluded_columns = seq_along(x) %in%  excluded_columns
        not_empty = rowSums(!is.na(x[,!excluded_columns]))>0  
    }
    if(is.null(excluded_rows)){
        excluded_rows = TRUE
    } else {
        if(is.numeric(excluded_rows)) excluded_rows = seq_len(nrow(x)) %in% excluded_rows
    }
    x[not_empty | excluded_rows, , drop = FALSE]
    
}


drop_empty_columns.data.frame = function(x, excluded_columns = 1, excluded_rows = grep("^#", x[[1]], perl= TRUE)){
    stopif(!is.null(excluded_columns) && !is.numeric(excluded_columns) && !is.character(excluded_columns) &&
               is.logical(excluded_columns),
           "`excluded_columns` should be character, numeric or logical.")
    stopif(!is.null(excluded_rows) && !is.numeric(excluded_rows) && !is.logical(excluded_rows),
           "`excluded_columns` should be numeric or logical.")
    not_empty = colSums(!is.na(x[, -1 , drop = FALSE]))>0
    x[, c(TRUE, not_empty), drop = FALSE]
    
}
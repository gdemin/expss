#' Title
#'
#' @param x character vector which will be splitted
#' @param remove_repeated logical. Shoul we remove repeated labels? 
#' @param split character vector (or object which can be coerced to such)
#'   containing regular expression(s) (unless fixed = TRUE) to use for
#'   splitting.
#' @param fixed logical. If TRUE, pattern is a string to be matched as is.
#'   Overrides all conflicting arguments.
#' @param perl logical. Should Perl-compatible regexps be used?
#'
#' @return \code{split_labels} returns matrix, \code{split_columns} returns
#'   data.frame with columns replaced by possibly multiple columns with splitted
#'   labels.
#' @export
#' @seealso \link[base]{strsplit}
#' @examples
#' a = 1
split_labels = function(x, remove_repeated = TRUE, split = "|", fixed = TRUE, perl = FALSE){
    x_splitted = strsplit(x, split = split, fixed = fixed, perl = perl)
    max_length = max(lengths(x_splitted))
    x_splitted = lapply(x_splitted, function(each) {
        if(length(each)<max_length){
            each[max_length] = NA
        }
        each
    })
    res = do.call(rbind, x_splitted)
    res[is.na(res)] = ""
    if (remove_repeated){
        for(i in rev(seq_len(nrow(res))[-1])){
            repeats = res[i-1, ] ==  res[i, ]
            first_no_repeat = which(!repeats)[1]-1
            if(!is.na(first_no_repeat) && first_no_repeat>0){
                res[i, 1:first_no_repeat] = ""
            }
            
        }
    }
    t(res)    
}

#' @export
#' @rdname split_labels
split_columns  = function(data, columns = 1, remove_repeated = TRUE, split = "|", fixed = TRUE, perl = FALSE){
   UseMethod("split_columns")
}

#' @export
split_columns.data.frame  = function(data, columns = 1, remove_repeated = TRUE, split = "|", fixed = TRUE, perl = FALSE){
    stopif(!is.numeric(columns) && !is.character(columns) &&
               !is.logical(columns),
           "`columns` should be character, numeric or logical.")
   
    if(is.character(columns)) {
        stopif(!all(columns %in% colnames(x)), "some of the 'columns' not found: ",
               paste(columns %d% colnames(x), collapse = ", ")
        )
        columns = which(colnames(x) %in%  columns)
    }
    if(is.numeric(columns)) {
        stopif(!all(columns %in% seq_along(x)) , "some of the 'columns' not found: ",
               paste(columns %d% seq_along(x), collapse = ", ")
        )
        
    }
    if(is.logical(excluded_rows)){ 
        stopif(length(excluded_rows) != nrow(x),
               "length of the 'excluded_rows' not equals to number of rows in 'x'"
        )
        columns = which(columns)
    }
    columns = sort(columns)
    for(each_column in rev(columns)){
        new_columns = as.dtfrm(
            split_labels(data[[each_column]], 
                         remove_repeated = remove_repeated, 
                         split = split, 
                         fixed = fixed, 
                         perl = perl)
        )
    }
}

#' @export
split_columns.matrix  = function(data, columns = 1, remove_repeated = TRUE, split = "|", fixed = TRUE, perl = FALSE){
    data = as.data.frame(data)
    split_columns(data, 
                  columns = columns, 
                  remove_repeated = remove_repeated, 
                  split = split, 
                  fixed = fixed, 
                  perl = perl
    )
}
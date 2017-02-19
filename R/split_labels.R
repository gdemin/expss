#' Split character vector to matrix/split columns in data.frame
#' 
#' \code{split_labels}/\code{split_columns} are auxiliary functions for 
#' post-processing tables resulted from \link{table_cases}/\link{table_summary} 
#' and etc. In these tables all labels collapsed in the first column with "|" 
#' separator. \code{split_columns} split first column into multiple columns 
#' with separator (\code{split} argument).
#'
#' @param data data.frame vector which will be split
#' @param x character vector which will be split
#' @param columns character/numeric/logical  columns in the data.frame
#'   \code{data} which should be split
#' @param remove_repeated logical. Should we remove repeated labels? 
#' @param split character vector (or object which can be coerced to such)
#'   containing regular expression(s) (unless \code{fixed = TRUE}) to use for
#'   splitting.
#' @param fixed logical. If TRUE match split exactly, otherwise use regular
#'   expressions. Has priority over \code{perl}.
#' @param perl logical. Should Perl-compatible regexps be used?
#'
#' @return \code{split_labels} returns character matrix, \code{split_columns} returns
#'   data.frame with columns replaced by possibly multiple columns with split
#'   labels.
#' @export
#' @seealso \link[base]{strsplit}
#' @examples
#' data(mtcars)
#'
#' # apply labels
#' mtcars = apply_labels(mtcars,
#'     cyl = "Number of cylinders",
#'     vs = "Engine",
#'     vs = c("V-engine" = 0,
#'                     "Straight engine" = 1),
#'     am = "Transmission",
#'     am = c(automatic = 0,
#'                     manual=1),
#'     gear = "Number of forward gears",
#'     carb = "Number of carburetors"
#' )
#' 
#' # all row labels in the first column
#' tabl = mtcars %>% calculate(table_cpct(list(cyl, gear, carb), list("#total", vs, am)))
#' split_labels(tabl[[1]])
#' split_labels(colnames(tabl))
#' 
#' # replace first column with new columns 
#' split_columns(tabl) # remove repeated
#' 
#' split_columns(tabl, remove_repeated = FALSE)
split_labels = function(x, remove_repeated = TRUE, split = "|", fixed = TRUE, perl = FALSE){
    if(length(x)==0){
        return(matrix(NA, ncol=0, nrow = 0))
    }
    x_split = strsplit(x, split = split, fixed = fixed, perl = perl)
    max_length = max(lengths(x_split))
    x_split = lapply(x_split, function(each) {
        if(length(each)<max_length){
            each[max_length] = NA
        }
        each
    })
    res = do.call(rbind, x_split)
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
    res    
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
        stopif(!all(columns %in% colnames(data)), "some of the 'columns' not found: ",
               paste(columns %d% colnames(data), collapse = ", ")
        )
        columns = which(colnames(data) %in%  columns)
    }
    if(is.numeric(columns)) {
        stopif(!all(columns %in% seq_along(data)) , "some of the 'columns' not found: ",
               paste(columns %d% seq_along(data), collapse = ", ")
        )
        
    }
    if(is.logical(columns)){ 
        stopif(length(columns) != ncol(data),
               "length of the 'columns' not equals to number of columns in 'data'"
        )
        columns = which(columns)
    }
    class_data = class(data)
    columns = sort(unique(columns))
    for(each_column in rev(columns)){
        curr_col = data[[each_column]]
        if(is.factor(curr_col)) curr_col = as.character(curr_col)
        new_columns = as.dtfrm(
            split_labels(curr_col, 
                         remove_repeated = remove_repeated, 
                         split = split, 
                         fixed = fixed, 
                         perl = perl)
        )
        colnames(new_columns) = paste0("..new_columns__",each_column,"_", seq_len(ncol(new_columns)))
        # to prevent name changes
        part1_names = colnames(data)[seq_len(each_column)[-each_column]]
        part2_names = colnames(data)[-seq_len(each_column)]
        data = dtfrm(
            setNames(data[, seq_len(each_column)[-each_column], drop = FALSE], part1_names), 
            new_columns, 
            setNames(data[, -seq_len(each_column), drop = FALSE], part2_names)
        )
    }
    if_val(colnames(data)) = perl("^\\.\\.new_columns__\\d+_\\d+$") ~ ""
    class(data) = class_data
    data
}

#' @export
split_columns.matrix  = function(data, columns = 1, remove_repeated = TRUE, split = "|", fixed = TRUE, perl = FALSE){
    data = as.dtfrm(data)
    split_columns(data, 
                  columns = columns, 
                  remove_repeated = remove_repeated, 
                  split = split, 
                  fixed = fixed, 
                  perl = perl
    )
}
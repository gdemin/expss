#' Drop empty (with all NA's) rows/columns from data.frame/table
#' 
#' By default tables produced by functions \link{table_cases}, 
#' \link{table_summary} and \link{table_summary_df} are created with all 
#' possible value labels. If values for this labels are absent in variable there 
#' are NA's in rows and columns. 
#' \code{drop_empty_rows}/\code{drop_empty_columns} are intended to remove 
#' these empty rows/columns. \code{drop_r} and \code{drop_c} are the same
#' functions with shorter names. \code{drop_rc} drops rows and columns
#' simultaneously.
#' 
#' @param x data.frame
#' @param excluded_rows logical/numeric rows which won't be dropped and in which
#'   NAs won't be counted. By default, it is rows which have values with "#" at 
#'   the beginning  in the first column. it is total rows in tables ("#" marks
#'   total in the result of \link{table_cases}).
#' @param excluded_columns logical/numeric/characters columns which won't be
#'   dropped and in which NAs won't be counted. By default, it is first column -
#'   column with labels in table.
#'
#' @return data.frame with removed rows/columns
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'             vs = "Engine",
#'             vs = num_lab("
#'                       0 V-engine 
#'                       1 Straight engine
#'                       9 Other
#'                       "),
#'             am = "Transmission",
#'             am = num_lab("
#'                      0 Automatic 
#'                      1 Manual
#'                      9 Other
#'                      ")
#'          )
#' with_empty = calculate(mtcars, table_cases(am, vs))
#' 
#' drop_empty_rows(with_empty)
#' drop_empty_columns(with_empty)
#' drop_rc(with_empty)
#'                         
drop_empty_rows = function(x, excluded_rows = grep("^#", x[[1]], perl= TRUE), excluded_columns = 1){
    UseMethod("drop_empty_rows")
}

#' @export
#' @rdname drop_empty_rows
drop_empty_columns = function(x, excluded_rows = grep("^#", x[[1]], perl= TRUE), excluded_columns = 1){
    UseMethod("drop_empty_columns")   
}

#' @export
drop_empty_rows.data.frame = function(x, excluded_rows = grep("^#", x[[1]], perl= TRUE), excluded_columns = 1){
    stopif(!is.null(excluded_columns) && !is.numeric(excluded_columns) && !is.character(excluded_columns) &&
               !is.logical(excluded_columns),
           "`excluded_columns` should be character, numeric or logical.")
    stopif(!is.null(excluded_rows) && !is.numeric(excluded_rows) && !is.logical(excluded_rows),
           "`excluded_rows` should be numeric or logical.")
    if(is.null(excluded_columns)) {
        not_empty = rowSums(!is.na(x))>0    
    } else {
        if(is.character(excluded_columns)) excluded_columns = colnames(x) %in%  excluded_columns
        if(is.numeric(excluded_columns)) excluded_columns = seq_along(x) %in%  excluded_columns
        not_empty = rowSums(!is.na(x[,!excluded_columns]))>0  
    }
    if(is.null(excluded_rows)){
        excluded_rows = FALSE
    } else {
        if(is.numeric(excluded_rows)) excluded_rows = seq_len(nrow(x)) %in% excluded_rows
    }
    x[not_empty | excluded_rows, , drop = FALSE]
    
}

#' @export
drop_empty_columns.data.frame = function(x, excluded_rows = grep("^#", x[[1]], perl= TRUE), excluded_columns = 1){
    stopif(!is.null(excluded_columns) && !is.numeric(excluded_columns) && !is.character(excluded_columns) &&
               !is.logical(excluded_columns),
           "`excluded_columns` should be character, numeric or logical.")
    stopif(!is.null(excluded_rows) && !is.numeric(excluded_rows) && !is.logical(excluded_rows),
           "`excluded_rows` should be numeric or logical.")
    if(is.null(excluded_rows)) {
        empty = colSums(!is.na(x))==0    
    } else {
        if(is.numeric(excluded_rows)) excluded_rows = seq_len(nrow(x)) %in%  excluded_rows
        empty = colSums(!is.na(x[!excluded_rows,]))==0 
    }
    if(is.null(excluded_columns)){
        excluded_columns = FALSE
    } else {
        if(is.character(excluded_columns)) excluded_columns = colnames(x) %in%  excluded_columns
        if(is.numeric(excluded_columns)) excluded_columns = seq_along(x) %in%  excluded_columns
        
    }
    x[, empty & (!excluded_columns)] = NULL # this notation doesn't change colnames if some of them are duplicated
    x
}

#' @export
#' @rdname drop_empty_rows
drop_r = drop_empty_rows

#' @export
#' @rdname drop_empty_rows
drop_c = drop_empty_columns

#' @export
#' @rdname drop_empty_rows
drop_rc = function(x){
    drop_empty_columns(drop_empty_rows(x))
}
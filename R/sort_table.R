#' Partially (inside blocks) sort tables/data.frames
#' 
#' \code{sort_table_asc} sorts tables (usually result of \link{table_cases}) in
#' ascending order and \code{sort_table_desc} sorts tables in descending order.
#' There is no non-standard evaluation in these functions by design so use
#' quotes for names of your columns or use \link{qc}.
#' @param x data.frame 
#' @param columns character/numeric. Column names/numbers for data.frame/table
#'   by which object will be sorted. By default it is 2 - the first column with 
#'   numbers in the table (there are row labels in the first column).
#' @param excluded_rows logical/numeric rows which won't be sorted. Rows of the
#'   table will be sorted between excluded rows. By default, it is rows which
#'   have values with "#" at the beginning  in the first column. It is total
#'   rows in tables ("#" marks total in the result of \link{table_cases}).
#' @param na.last for controlling the treatment of NAs. If TRUE, missing values
#'   in the data are put last; if FALSE, they are put first; if NA, they are
#'   removed.
#'
#' @return sorted table('etable')/data.frame
#' @export
#'
#' @examples
#' 1
sort_table_asc = function(x, columns = 2, excluded_rows = grep("^#", x[[1]], perl= TRUE), na.last = FALSE){
    UseMethod("sort_table_asc")
}

#' @export
#' @rdname sort_table_asc
sort_table_desc = function(x, columns = 2, excluded_rows = grep("^#", x[[1]], perl= TRUE), na.last = TRUE){
    UseMethod("sort_table_desc")
}


#' @export
sort_table_asc.data.frame = function(x, columns = 2, excluded_rows = grep("^#", x[[1]], perl= TRUE, na.last = FALSE)){
    sort_table_helper(x = x, columns = columns, excluded_rows = excluded_rows, na.last = na.last, asc = TRUE)
}

#' @export
sort_table_desc.data.frame = function(x, columns = 2, excluded_rows = grep("^#", x[[1]], perl= TRUE, na.last = TRUE)){
    sort_table_helper(x = x, columns = columns, excluded_rows = excluded_rows, na.last = na.last, desc = TRUE)
}

sort_table_helper = function(x, columns, excluded_rows, asc){
    stopif(!is.numeric(columns) && !is.character(columns),
           "`columns` should be character or numeric.")
    stopif(!is.null(excluded_rows) && !is.numeric(excluded_rows) && !is.logical(excluded_rows),
           "`excluded_rows` should be numeric or logical.")
    
    if(is.character(columns)) columns = colnames(x) %in%  columns
    if(is.numeric(columns)) columns = seq_along(x) %in%  columns
    
    if(is.null(excluded_rows)){
        excluded_rows = FALSE
    } else {
        if(is.numeric(excluded_rows)) excluded_rows = seq_len(nrow(x)) %in% excluded_rows
    }
    x[not_empty | excluded_rows, , drop = FALSE]    
}
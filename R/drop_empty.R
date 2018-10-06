#' Drop empty (with all NA's) rows/columns from data.frame/table
#' 
#' By default tables produced by functions \link{tables}, \link{cro}, 
#' \link{cro_fun} and \link{cro_fun_df} are created with all 
#' possible value labels. If values for this labels are absent in variable there 
#' are NA's in rows and columns. 
#' \code{drop_empty_rows}/\code{drop_empty_columns} are intended to remove 
#' these empty rows/columns. \code{drop_r} and \code{drop_c} are the same
#' functions with shorter names. \code{drop_rc} drops rows and columns
#' simultaneously.
#' 
#' @param x data.frame/etable(result of \link{cro} and etc.)
#' @param excluded_rows character/logical/numeric rows which won't be dropped
#'   and in which NAs won't be counted. If it is characters then they will be
#'   considered as pattern/vector of patterns. Patterns will be matched with
#'   Perl-style regular expression with values in the first column of \code{x}
#'   (see \link[base]{grep}, \code{perl = TRUE} argument). Rows which have such
#'   patterns will be excluded. By default for class 'etable' pattern is "#"
#'   because "#" marks totals in the result of \link{cro}.
#' @param excluded_columns logical/numeric/characters columns which won't be
#'   dropped and in which NAs won't be counted. By default for class 'etable' it
#'   is first column - column with labels in table.
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
#' with_empty = calculate(mtcars, cro(am, vs))
#' 
#' drop_empty_rows(with_empty)
#' drop_empty_columns(with_empty)
#' drop_rc(with_empty)
#'                         
drop_empty_rows = function(x, excluded_rows = NULL, excluded_columns = NULL){
    UseMethod("drop_empty_rows")
}

#' @export
#' @rdname drop_empty_rows
drop_empty_columns = function(x, excluded_rows = NULL, excluded_columns = NULL){
    UseMethod("drop_empty_columns")   
}

#' @export
drop_empty_rows.etable = function(x, excluded_rows = "#", excluded_columns = 1){
    drop_empty_rows.data.frame(x, excluded_rows = excluded_rows, excluded_columns = excluded_columns)    
}

#' @export
drop_empty_rows.data.frame = function(x, excluded_rows = NULL, excluded_columns = NULL){
    stopif(!is.null(excluded_columns) && !is.numeric(excluded_columns) && !is.character(excluded_columns) &&
               !is.logical(excluded_columns),
           "`excluded_columns` should be character, numeric or logical.")
    stopif(!is.null(excluded_rows) && !is.numeric(excluded_rows) && 
               !is.logical(excluded_rows) && !is.character(excluded_rows) ,
           "`excluded_rows` should be character/numeric or logical.")
    x_mat = mis_val(x, perl("^[\\s\\t]*$"))
    if(is.null(excluded_columns)) {
        not_empty = rowSums(!is.na(x_mat))>0    
    } else {
        
        if(is.character(excluded_columns)) {
            stopif(!all(excluded_columns %in% colnames(x)), "some of the 'excluded_columns' not found: ",
                   paste(excluded_columns %d% colnames(x), collapse = ", ")
                   )
            excluded_columns = colnames(x) %in%  excluded_columns
        }
        if(is.numeric(excluded_columns)) {
            stopif(!all(excluded_columns %in% seq_along(x)) , "some of the 'excluded_columns' not found: ",
                   paste(excluded_columns %d% seq_along(x), collapse = ", ")
            )
            excluded_columns = seq_along(x) %in%  excluded_columns
        }
        not_empty = rowSums(!is.na(x_mat[,!excluded_columns]))>0  
    }
    if(is.null(excluded_rows)){
        excluded_rows = FALSE
    } else {
        if(is.numeric(excluded_rows)) {
            stopif(!all(excluded_rows %in% seq_len(nrow(x))) , "some of the 'excluded_rows' not found: ",
                   paste(excluded_rows %d% seq_len(nrow(x)), collapse = ", ")
            )
            excluded_rows = seq_len(nrow(x)) %in% excluded_rows
            
        }
        if(is.character(excluded_rows)){
            excluded_rows = lapply(excluded_rows, grepl, x[[1]], perl = TRUE)
            excluded_rows = Reduce("|", excluded_rows)
        }
    }
    x[not_empty | excluded_rows, , drop = FALSE]
    
}

#' @export
drop_empty_columns.etable = function(x, excluded_rows = "#", excluded_columns = 1){
    drop_empty_columns.data.frame(x, excluded_rows = excluded_rows, excluded_columns = excluded_columns)    
}

#' @export
drop_empty_columns.data.frame = function(x, excluded_rows = NULL, excluded_columns = NULL){
    stopif(!is.null(excluded_columns) && !is.numeric(excluded_columns) && !is.character(excluded_columns) &&
               !is.logical(excluded_columns),
           "`excluded_columns` should be character, numeric or logical.")
    stopif(!is.null(excluded_rows) && !is.numeric(excluded_rows) && 
               !is.logical(excluded_rows) && !is.character(excluded_rows) ,
           "`excluded_rows` should be character/numeric or logical.")
    # has_characters = any(vapply(x, FUN = is.character, FUN.VALUE = NA))
    x_mat = mis_val(x, perl("^[\\s\\t]*$"))
    if(is.null(excluded_rows)) {
        empty = colSums(!(is.na(x_mat)))==0    
    } else {
        if(is.numeric(excluded_rows)) {
            stopif(!all(excluded_rows %in% seq_len(nrow(x))) , "some of the 'excluded_rows' not found: ",
                   paste(excluded_rows %d% seq_len(nrow(x)), collapse = ", ")
            )
            excluded_rows = seq_len(nrow(x)) %in%  excluded_rows
        }
        if(is.character(excluded_rows)){
            excluded_rows = lapply(excluded_rows, grepl, x[[1]], perl = TRUE)
            excluded_rows = Reduce("|", excluded_rows)
        }
        
        empty = colSums(!is.na(x_mat[!excluded_rows,]))==0 
    }
    if(is.null(excluded_columns)){
        excluded_columns = FALSE
    } else {
        if(is.character(excluded_columns)) {
            
            stopif(!all(excluded_columns %in% colnames(x)), "some of the 'excluded_columns' not found: ",
                   paste(excluded_columns %d% colnames(x), collapse = ", ")
            )
            excluded_columns = colnames(x) %in%  excluded_columns
        }
        if(is.numeric(excluded_columns)) {
            stopif(!all(excluded_columns %in% seq_along(x)) , "some of the 'excluded_columns' not found: ",
                   paste(excluded_columns %d% seq_along(x), collapse = ", ")
            )
            excluded_columns = seq_along(x) %in%  excluded_columns
        }   
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
#' Add columns to data.frame.
#' 
#' \code{add_columns} inspired by MATCH FILES (Add
#' variables...) from SPSS Statistics. It works similar to SQL left join but
#' number of cases in the left part always remain the same. If there are
#' duplicated keys in the \code{y} then error will be raised by default.
#' 
#' @param x data.frame to be joined with \code{y}.
#' @param y data.frame.
#' @param by character vector or NULL(default) or 1. Names of common variables
#'   in the \code{x} and \code{y} by which we will attach \code{y} to
#'   \code{x}. If it is NULL then common names will be used. If it is equals
#'   to 1 then we will use the first column from both dataframes. To add columns
#'   by different variables on \code{x} and \code{y} use a named vector.
#'   For example, \code{by = c("a" = "b")} will match x.a to y.b.
#' @param ignore_duplicates logical Should we ignore duplicates in the \code{by}
#'   variables in the \code{y}? If it is TRUE than first occurrence of duplicated
#'   key will be used.
#' @param ... arguments for further methods
#' @return data.frame
#'   
#' @export
#' @examples
#'
#' # example for 'add_columns' from base 'merge'
#' authors = data.frame(
#'     surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
#'     nationality = c("US", "Australia", "US", "UK", "Australia"),
#'     deceased = c("yes", rep("no", 4))
#' )
#' 
#' books = data.frame(
#'     surname = c("Tukey", "Venables", "Tierney",
#'                 "Ripley", "Ripley", "McNeil", "R Core"),
#'     title = c("Exploratory Data Analysis",
#'               "Modern Applied Statistics ...",
#'               "LISP-STAT",
#'               "Spatial Statistics", "Stochastic Simulation",
#'               "Interactive Data Analysis",
#'               "An Introduction to R")
#' )
#' 
#' add_columns(books, authors)
#' 
#' @export
add_columns = function(x, y, 
                       by = NULL, 
                       ignore_duplicates = FALSE,
                       ...
){
    UseMethod("add_columns")
    
}

#' @export
add_columns.etable = function(x, y, by = 1, ignore_duplicates = FALSE, ...){
    merge(x, y, by = by, ...)
}

#' @export
add_columns.data.frame = function(x, y, 
                       by = NULL, 
                       ignore_duplicates = FALSE,
                       ...
){
    if(!is.data.frame(x)) x = as.sheet(x)
    if(!is.data.frame(y)) y = as.sheet(y)
    # ..by_data = NULL
    # ..by = NULL
    colnames_data = colnames(x)
    colnames_dict = colnames(y)
    if(is.null(by)){
        by = intersect(colnames_data, colnames_dict)
        stopif(length(by)==0, "'add_columns' - no common column names between 'x' and 'y'.")
        message(paste0("Adding columns by ", paste(dQuote(by), collapse = ", ")))
    }
    if(identical(by, 1) || identical(by, 1L)){
        lookup_value = x[[1]]    
        lookup_column = y[[1]]   
        col_nums_dict = 1
    } else {
        stopif(!is.character(by), "'add_columns' - 'by' should be character, NULL or 1.")
        if(!is.null(names(by))){
            by_data = names(by)
            by_data[by_data==""] = by[by_data==""]
        } else {
            by_data = by
        }
        stop_if_columns_not_exist(colnames_data, by_data)
        stop_if_columns_not_exist(colnames_dict, by)
        if(length(by)>1){
            stopif(anyDuplicated(by), "'add_columns'- duplicated variable names in 'by': ", 
                   paste(dQuote(by[duplicated(by)]), collapse = ", "))
            stopif(anyDuplicated(by_data), "'add_columns'- duplicated variable names in 'by': ", 
                   paste(dQuote(by_data[duplicated(by_data)]), collapse = ", "))
            if(is.data.table(x)){
                lookup_value = x[ , by_data, with = FALSE]     
            } else {
                lookup_value = x[ , by_data]
            }
            if(is.data.table(y)){
                lookup_column = y[ , by, with = FALSE]                     
            } else {
                lookup_column = y[ , by] 
            }
            lookup_value = do.call("paste", c(unlab(lookup_value), sep = "\r"))
            lookup_column = do.call("paste", c(unlab(lookup_column), sep = "\r"))
        } else {
            lookup_value = x[[by_data]]    
            lookup_column = y[[by]]  
        }
        col_nums_dict = match(by, colnames_dict)
    }
    if(!ignore_duplicates){
        stopif(anyDuplicated(lookup_column), 
               "'add_columns' - duplicated values in 'by' columns in 'y'")
    }
    # calculate index
    ind = fast_match(lookup_value, lookup_column, NA_incomparable = FALSE)
    if(is.data.table(y)){
        res = subset_dataframe(y[,-col_nums_dict, with = FALSE], ind, drop = FALSE)    
    } else {
        res = subset_dataframe(y[,-col_nums_dict, drop = FALSE], ind, drop = FALSE)    
    }
    
    # make unique names in res
    colnames_res = colnames(res)
    dupl = intersect(colnames_res, colnames_data)
    if(length(dupl)>0){
        warning(
            paste0("'add_columns' - some names in 'y' duplicate names in 'x': ",
                   paste(dupl, collapse = ", ")
                   )
            )
        all_names = make.unique(c(colnames_data, colnames_res), sep = "_")
        # we change only dictionary names 
        colnames(res) = all_names[-seq_along(colnames_data)]
    }
    if(is.data.table(x)){
        x[, colnames(res):=res]    
    } else {
        x[, colnames(res)] = res    
    }
    
    x

}

#' @export
add_columns.huxtable = function(...){
    huxtable::add_columns(...)
}


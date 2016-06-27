#' Get range of variables/variables by pattern/by name
#' 
#' \itemize{
#' \item{\code{\%to\%}}{ returns all variables in range from first argument to second 
#' (similar to SPSS 'to'). Result doesn't depend from order of variables in 
#' data.frame. Results are always arranged in ascending order and include all
#' variables with such pattern even if these variables located in different
#' parts of dataframe. \code{vars_range} has the same functionality but
#' intended for programming.}
#' \item{\code{vars_pattern}}{ returns all variables by pattern (regular expression).
#' Functions with word 'list' in name return lists of variables instead of
#' dataframes.}
#' \item{\code{vars}}{ returns all variables by their names. Expressions in backticks
#' inside characters will be expanded as with \link{subst}.}
#' }
#' Functions with word 'list' in name return lists of variables instead of
#' dataframes.
#'
#' @param start character Name of start variable (e. g. a_1)
#' @param end character Name of start variable (e. g. a_5)
#' @param e1 unquoted name of start variable (e. g. a_1)
#' @param e2 unquoted name of start variable (e. g. a_5) 
#' @param pattern character pattern of variable(-s) name
#' @param ... characters names of variables.
#'
#' @return  data.frame/list with variables
#' 
#' @examples
#' 
#' # In global environement
#' aa = rep(10, 5)
#' b = rep(20, 5)
#' a_1 = rep(1, 5)
#' a_2 = rep(2, 5)
#' a_3 = rep(3, 5)
#' a_4 = rep(4, 5)
#' a_5 = rep(5, 5)
#' 
#' # identical results
#' vars_range("a_1", "a_5")
#' a_1 %to% a_5
#' vars("a_`1:5`")
#' vars_pattern("^a_[0-9]$")
#' 
#' # sum each row
#' sum_row(a_1 %to% a_5)
#' 
#' # In data.frame
#' dfs = data.frame(
#'     aa = rep(10, 5),
#'     b_ = rep(20, 5),
#'     b_1 = rep(11, 5),
#'     b_2 = rep(12, 5),
#'     b_3 = rep(13, 5),
#'     b_4 = rep(14, 5),
#'     b_5 = rep(15, 5) 
#' )
#' 
#' # all variables that starts with 'b'
#' with(dfs, vars_pattern("^b"))
#' 
#' # calculate sum of b_* variables
#' modify(dfs,{
#'     b_total = sum_row(b_1 %to% b_5)
#'     b_total2 = sum_row(vars("b_`1:5`"))
#' })
#' 
#' 
#' @export
vars_range = function(start, end){
    
    as.data.frame(vars_range_list(start, end), stringsAsFactors = FALSE, check.names = FALSE)
    
}

#' @export
#' @rdname vars_range
vars_range_list = function(start, end){
    stopif(length(start)!=1 | length(end)!=1, "Length of 'start' and 'end' arguments shoud be equal to 1. 
           But length(start)=", length(start), " and length(end) = ", length(end) )
    stopif(!all(grepl("^(.+?)([\\d]+)$", c(start, end), perl = TRUE)),
           "'start' or 'end' arguments doesn't have correct pattern: ", start, " ", end)
    patt1 = gsub("^(.+?)([\\d]+)$", "\\1", start, perl = TRUE)
    patt2 = gsub("^(.+?)([\\d]+)$", "\\1", end, perl = TRUE)
    stopif(patt1!=patt2, "Start and end variables begin from different patterns: ", patt1, " ", patt2)
    digits1 = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", start, perl = TRUE))
    digits2 = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", end, perl = TRUE))
    stopif(digits1>digits2, "Name of start variables greater than name of end variables: ", start," ",end)
    
    scope = 1
    e = parent.frame(scope)
    var_names = ls(name = e, pattern = paste0("^", patt1, "[0-9]+$"))
   
    while(length(var_names)==0 & !identical(e, globalenv())){
        scope = scope + 1
        e = parent.frame(scope)
        var_names = ls(name = e, pattern = paste0("^", patt1, "[0-9]+$"))  
    }  
    
    stopif(length(var_names)==0, "Variables not found: ", start," ",end)
    digits = as.numeric(gsub("^(.+?)([\\d]+)$", "\\2", var_names, perl = TRUE))
    var_names = var_names[order(digits)]
    digits = digits[order(digits)]
    var_names = var_names[digits>=digits1 & digits<=digits2] 
    mget(var_names, envir = e)

}

#' @export
#' @rdname vars_range
'%to%' = function(e1, e2){
    e1 = as.character(substitute(e1))
    e2 = as.character(substitute(e2))
    vars_range(e1, e2)
}

#' @export
#' @rdname vars_range
'%to_list%' = function(e1, e2){
    e1 = as.character(substitute(e1))
    e2 = as.character(substitute(e2))
    vars_range_list(e1, e2)
}

#' @export
#' @rdname vars_range
vars_pattern_list = function(pattern){
    stopif(length(pattern)!=1, "Length of 'pattern' shoud be equal to 1. 
           But length(pattern)=", length(pattern))
    scope = 1
    e = parent.frame(scope)
    var_names = ls(name = e, pattern = pattern)
    
    while(length(var_names)==0 & !identical(e, globalenv())){
        scope = scope + 1
        e = parent.frame(scope)
        var_names = ls(name = e, pattern = pattern)  
    }  
    
    stopif(length(var_names)==0, "Variables not found: ", pattern)
    mget(var_names, envir = e) 
    
} 

#' @export
#' @rdname vars_range
vars_pattern = function(pattern){
    as.data.frame(vars_pattern_list(pattern), stringsAsFactors = FALSE, check.names = FALSE) 
} 


#' @export
#' @rdname vars_range
vars_list = function(...){
    .var_names_ = c(lapply(c(list(...), recursive = TRUE), subst), recursive = TRUE)
    .res_ = vector(mode = "list", length = length(.var_names_))
    for(.each_ in seq_along(.var_names_)){
        .res_[[.each_]] = get(.var_names_[.each_], pos = parent.frame(), inherits = TRUE)
    }
    stats::setNames(.res_, .var_names_)
}

#' @export
#' @rdname vars_range
vars = function(...){
    .var_names_ = c(lapply(c(list(...), recursive = TRUE), subst), recursive = TRUE)
    .res_ = vector(mode = "list", length = length(.var_names_))
    for(.each_ in seq_along(.var_names_)){
        .res_[[.each_]] = get(.var_names_[.each_], pos = parent.frame(), inherits = TRUE)
    }
    as.dfrm(stats::setNames(.res_, .var_names_))
}


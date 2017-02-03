#' Get range of variables/variables by pattern/by name
#' 
#' \itemize{
#' \item{\code{\%to\%}}{ returns all variables with names  in range from
#' pattern_num1 to pattern_num2 (similar to SPSS 'to'). Result doesn't depend
#' from order of variables in data.frame. 'num1' and 'num2' should be numbers.
#' Results are always arranged in ascending order and include all variables with
#' such pattern even if these variables located in different parts of dataframe.
#' \code{vars_range} has the same functionality but intended for programming.}
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
#' a1 = rep(1, 5)
#' a2 = rep(2, 5)
#' a3 = rep(3, 5)
#' a4 = rep(4, 5)
#' a5 = rep(5, 5)
#' 
#' # identical results
#' vars_range("a1", "a5")
#' a1 %to% a5
#' vars("a`1:5`")
#' vars_pattern("^a[0-9]$")
#' 
#' # sum each row
#' sum_row(a1 %to% a5)
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
vars = function(...){
    args = substitute(list(...))
    res = eval(bquote(expss::vars_list(.(args))),
               envir = parent.frame(),
               enclos = baseenv()
    )
    as.dtfrm(res)
}

#' @export
#' @rdname vars
vars_list = function(...){
    if(exists(".internal_column_names0", envir = parent.frame())){
        var_names = internal_ls(parent.frame()[[".internal_column_names0"]], env = parent.frame())
    } else {
        var_names = ls(envir = parent.frame())
    }
    args = substitute(list(...))
    args = substitute_symbols(args,
                              list("%to%" = "internal_to")
    )
    args = eval(args)
    selected_names = keep_helper(var_names, args)
    mget(var_names[selected_names], envir = parent.frame(), inherits = TRUE)
}

#' @export
#' @rdname vars
'%to%' = function(e1, e2){
    e1 = substitute(e1)
    e2 = substitute(e2)
    eval(bquote(expss::vars(.(e1) %to% .(e2))),
         envir = parent.frame(),
         enclos = baseenv()
    )
}

#' @export
#' @rdname vars
'%to_list%' = function(e1, e2){
    e1 = substitute(e1)
    e2 = substitute(e2)
    eval(bquote(expss::vars_list(.(e1) %to% .(e2))),
         envir = parent.frame(),
         enclos = baseenv()
    )
}

###################################


#' @export
#' @rdname vars
vars_range = function(start, end){
    .Deprecated(msg = 
                    paste0("'vars_range' is deprecated. Use 'vars(from('", start,"') & to('", end, "'))' instead."))
    eval(substitute(expss::vars(from(start) & to(end))),
         envir = parent.frame(),
         enclos = baseenv()
    )
    
    
}

#' @export
#' @rdname vars
vars_range_list = function(start, end){
    .Deprecated(msg = 
                    paste0("'vars_range_list' is deprecated. Use 'vars_list(from('", start,"') & to('", end, "'))' instead."))
    eval(substitute(expss::vars_list(from(start) & to(end))),
         envir = parent.frame(),
         enclos = baseenv()
    )
    
}

#' @export
#' @rdname vars
vars_pattern = function(pattern){
    .Deprecated(msg = 
                    paste0("'vars_pattern' is deprecated. Use 'vars(perl(", pattern,"))' instead."))
    eval(substitute(expss::vars(perl(pattern))),
         envir = parent.frame(),
         enclos = baseenv()
    )
} 


#' @export
#' @rdname vars
vars_pattern_list = function(pattern){
    .Deprecated(msg = 
                    paste0("'vars_pattern_list' is deprecated. Use 'vars_list(perl(", pattern,"))' instead."))
    eval(substitute(expss::vars_list(perl(pattern))),
         envir = parent.frame(),
         enclos = baseenv()
    )
    
} 


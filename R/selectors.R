#' Get variables by pattern/by name or range of variables.
#' 
#' \itemize{
#' \item{\code{vars}}{ returns all variables by their names or by criteria (see 
#' \link{criteria}). Expressions in backticks inside characters will be expanded
#' as with \link{subst}. \code{a`1:2`} will be translated to \code{'a1', 'a2'}. 
#' There is no non-standard evaluation in this function by design so use quotes 
#' for names of your variables or use \link{qc}. The only exception with 
#' non-standard evaluation is \code{\%to\%}. You can use \code{\%to\%} inside 
#' \code{vars} or independently.}
#' \item{\code{\%to\%}}{ returns range of variables between \code{e1} and 
#' \code{e2} (similar to SPSS 'to'). \link{modify}, \link{modify_if},
#' \link{calculate}, \link{keep}, \link{except} and \link{where} support
#' \code{\%to\%} but if it will be used in global environment or inside
#' \link[base]{with}, \link[base]{within} range will be taken from names of
#' variables sorted in the alphabet order.}}
#' Functions with word 'list' in name return lists of variables instead of 
#' dataframes.
#' \code{vars_pattern}, \code{vars_pattern_list}, \code{vars_range} and 
#' \code{vars_range_list} are deprecated and will be removed in the future
#' version.
#' @seealso \link{keep}
#' @param ... characters names of variables or criteria/logical functions
#' @param e1 unquoted name of start variable (e. g. a_1)
#' @param e2 unquoted name of start variable (e. g. a_5) 
#' @param start character name of start variable (e. g. a_1)
#' @param end character name of start variable (e. g. a_5)
#' @param pattern character pattern of variable(-s) name
#'
#' @return  data.frame/list with variables
#' 
#' @examples
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
#' # calculate sum of b_* variables
#' modify(dfs, {
#'     b_total = sum_row(b_1 %to% b_5)
#'     b_total2 = sum_row(vars("b_`1:5`"))
#' })
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
#' a1 %to% a5
#' vars("a`1:5`")
#' vars(perl("^a[0-9]$"))
#' 
#' # sum each row
#' sum_row(a1 %to% a5)
#' 
#' @export
vars = function(...){
    # args = substitute(list(...))
    res = eval(substitute(expss::vars_list(...)),
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
                              list("%to%" = ".internal_to_")
    )
    args = eval(args, envir = parent.frame(),
                enclos = baseenv())
    selected_names = keep_helper(var_names, args)
    mget(var_names[selected_names], envir = parent.frame(), inherits = TRUE)
}

#' @export
#' @rdname vars
'%to%' = function(e1, e2){
    # e1 = substitute(e1)
    # e2 = substitute(e2)
    eval(substitute(expss::vars(e1 %to% e2)),
         envir = parent.frame(),
         enclos = baseenv()
    )
}

#' @export
#' @rdname vars
'%to_list%' = function(e1, e2){
    # e1 = substitute(e1)
    # e2 = substitute(e2)
    eval(substitute(expss::vars_list(e1 %to% e2)),
         envir = parent.frame(),
         enclos = baseenv()
    )
}

###################################





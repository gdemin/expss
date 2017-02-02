#' Modify data.frame/conditionally modify data.frame
#' 
#' \code{modify} evaluates expression \code{expr} in the context of data.frame 
#' \code{data}. It works similar to \code{\link[base]{within}} in base R but try
#' to return new variables in order of their occurance in the expression. 
#' \code{modify_if} modifies only rows for which \code{cond} has TRUE. Other 
#' rows remain unchanged. Newly created variables also will have values only in 
#' rows for which \code{cond} has TRUE. There will be NA's in other rows. This 
#' function tries to mimic SPSS "DO IF(). ... END IF." statement. There is a 
#' special constant \code{.N} which equals to number of cases in \code{data} for
#' usage in expression inside \code{modify}. Inside \code{modify_if} \code{.N} 
#' gives number of rows which will be affected by expressions. Inside these 
#' functions you can use \code{set} function which creates variables with given 
#' name/set values to existing variables - \link{.set}. It is possible with 
#' \code{set} to assign values to multiple variables at once. \code{compute} is
#' an alias for \code{modify} and \code{do_if} is an alias for \code{modify_if}.
#'
#' @param data data.frame
#' @param expr expression(s) that should be evaluated in the context of data.frame \code{data}
#' @param cond logical vector or expression. Expression will be evaluated in the context of the data.  
#'
#' @return Both functions returns modified data.frame
#'
#' @examples
#' dfs = data.frame(
#'     test = 1:5,
#'     aa = rep(10, 5),
#'     b_ = rep(20, 5),
#'     b_1 = rep(11, 5),
#'     b_2 = rep(12, 5),
#'     b_3 = rep(13, 5),
#'     b_4 = rep(14, 5),
#'     b_5 = rep(15, 5) 
#' )
#' 
#' 
#' # calculate sum of b* variables
#' modify(dfs, {
#'     b_total = sum_row(b_, b_1 %to% b_5)
#'     var_lab(b_total) = "Sum of b"
#'     random_numbers = runif(.N) # .N usage
#' })
#' 
#' # 'set' function
#' # new variables filled with NA
#' modify(dfs, {
#'     set('new_b`1:5`')
#' })
#' 
#' # 'set' function
#' # set values to existing/new variables
#' # expression in backticks will be expanded - see ?subst
#' modify(dfs, {
#'     set('new_b`1:5`', b_1 %to% b_5)
#' })
#' 
#' 
#' # conditional modification
#' modify_if(dfs, test %in% 2:4, {
#'     aa = aa + 1    
#'     a_b = aa + b_    
#'     b_total = sum_row(b_, b_1 %to% b_5)
#'     random_numbers = runif(.N) # .N usage
#' })
#' 
#' @export
modify =  function (data, expr) {
    UseMethod("modify")
}

#' @export
modify.data.frame = function (data, expr) {
    # based on 'within' from base R by R Core team
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    prepare_env(e, n = nrow(data))
    eval(substitute(expr), envir = e, enclos = baseenv())
    clear_env(e)
    l = as.list(e, all.names = TRUE)
    l = l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    del = setdiff(names(data), names(l))
    if(length(del)){
        data[, del] = NULL
    }
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    wrong_rows = nrows!=1L & nrows!=nrow(data)
    if(any(wrong_rows)){
        er_message = utils::head(paste0("'", names(l)[wrong_rows], "' has ", nrows[wrong_rows], " rows"), 5)
        er_message = paste(er_message, collapse = ", ")
        stop(paste0("Bad number of rows: ", er_message, " instead of ", nrow(data), " rows."))
    }
    
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    nl = c(names(data), new_vars)
    data[nl] = l[nl]
    data
}


#' @export
#' @rdname modify
'%modify%' = function (data, expr) {
    # based on 'within' from base R by R Core team
    expr = substitute(expr)
    data = substitute(data)
    eval(bquote(modify(.(data), .(expr))), envir = parent.frame(), enclos = baseenv())
}

#' @export
#' @rdname modify
compute = modify

#' @export
#' @rdname modify
'%compute%' = `%modify%`

#' @export
#' @rdname modify
modify_if = function (data, cond, expr){
    UseMethod("modify_if")
}


#' @export
#' @rdname modify
do_if = modify_if

#' @export
modify_if.data.frame = function (data, cond, expr) {
    # based on 'within' from base R by R Core team
    data_expr = substitute(data)
    cond = substitute(cond)
    expr = substitute(expr)
    e = evalq(environment(), data, parent.frame())
    prepare_env(e, n = NROW(data))
    cond = calc_cond(cond, envir = e)
    
    new_data = eval(bquote(modify(.(data_expr)[.(cond),, drop = FALSE], .(expr))),
                    envir = parent.frame(),
                    enclos = baseenv()
                    )
    del = setdiff(names(data), names(new_data))
    if(length(del)){
        data[, del] = NULL
    }
    new_vars = names(new_data)[!(names(new_data) %in% names(data))]
    data[cond, names(data)] = new_data[names(data)]
    data[, new_vars] = NA
    data[cond, new_vars] = new_data[new_vars]
    data
}




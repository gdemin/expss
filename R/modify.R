#' Modify data.frame/modify subset of the data.frame
#' 
#' \itemize{
#' \item{{\code{modify}}{ evaluates expression \code{expr} in the context of data.frame 
#' \code{data} and return original data possibly modified. It works similar to
#' \code{\link[base]{within}} in base R but try to return new variables in order
#' of their occurrence in the expression and make available
#' full-featured \code{\%to\%} and \code{.N} in the expressions. See \link{vars}.}}
#' \item{{\code{calculate}}{ evaluates expression \code{expr} in the context of
#' data.frame \code{data} and return value of the evaluated expression. It works
#' similar to \code{\link[base]{with}} in base R but make available
#' full-featured \code{\%to\%} and \code{.N} in the expressions. See \link{vars}.}}
#' \item{{\code{modify_if}}{ modifies only rows for which \code{cond} equals to
#' TRUE. Other rows remain unchanged. Newly created variables also will have
#' values only in rows for which \code{cond} have TRUE. There will be NA's in
#' other rows. This function tries to mimic SPSS "DO IF(). ... END IF."
#' statement.}}
#' }
#' There is a special constant \code{.N} which equals to number of cases in
#' \code{data} for usage in expression inside \code{modify}/\code{calculate}.
#' Inside \code{modify_if} \code{.N} gives number of rows which will be affected
#' by expressions. \code{compute} is an alias for \code{modify}, \code{do_if}
#' is an alias for \code{modify_if} and \code{calc} is an alias for 
#' \code{calculate}.
#' 
#' @param data data.frame/list of data.frames. If \code{data} is list of
#'   data.frames then expression \code{expr} will be evaluated inside each
#'   data.frame separately.
#' @param expr expression that should be evaluated in the context of data.frame \code{data}
#' @param cond logical vector or expression. Expression will be evaluated in the context of the data.  
#'
#' @return \code{modify} and \code{modify_if} functions return modified 
#'   data.frame/list of modified data.frames, \code{calculate} returns value of
#'   the evaluated expression/list of values.
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
#' # compute sum of b* variables and attach it to 'dfs'
#' modify(dfs, {
#'     b_total = sum_row(b_, b_1 %to% b_5)
#'     var_lab(b_total) = "Sum of b"
#'     random_numbers = runif(.N) # .N usage
#' })
#' 
#' # calculate sum of b* variables and return it
#' calculate(dfs, sum_row(b_, b_1 %to% b_5))
#' 
#' 
#' # set values to existing/new variables
#' modify(dfs, {
#'     (b_1 %to% b_5) %into% subst('new_b`1:5`')
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
    parent = parent.frame()
    expr = substitute(expr)
    modify_internal(data, expr, parent = parent)
}


modify_internal =  function (data, expr, parent) {
    UseMethod("modify_internal")
}

#' @export
modify_internal.data.frame = function (data, expr, parent) {
    # based on 'within' from base R by R Core team
    e = evalq(environment(), data, parent)
    prepare_env(e, n = nrow(data), column_names = colnames(data))
    eval(expr, envir = e, enclos = baseenv())
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
modify_internal.list = function (data, expr, parent) {
    for(each in seq_along(data)){
        data[[each]] = modify_internal(data[[each]], expr, parent = parent)
    }
    data
}



#' @export
#' @rdname modify
compute = modify


#' @export
#' @rdname modify
modify_if = function (data, cond, expr){
    cond = substitute(cond)
    expr = substitute(expr)
    parent = parent.frame()
    modify_if_internal(data, cond, expr, parent = parent)
}


#' @export
#' @rdname modify
do_if = modify_if

modify_if_internal = function (data, cond, expr, parent){
    UseMethod("modify_if_internal")
} 

#' @export
modify_if_internal.data.frame = function (data, cond, expr, parent) {
    # based on 'within' from base R by R Core team
    e = evalq(environment(), data, parent)
    prepare_env(e, n = NROW(data), column_names = colnames(data))
    cond = calc_cond(cond, envir = e)
    
    new_data = modify_internal(data[cond,, drop = FALSE], expr, parent)
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

#' @export
modify_if_internal.list = function (data, cond, expr, parent) {
    for(each in seq_along(data)){
        data[[each]] = modify_if_internal(data[[each]], cond, expr, parent = parent)
    }
    data
}

########

#' @export
#' @rdname modify
calculate =  function (data, expr) {
    expr = substitute(expr)
    parent = parent.frame()
    calculate_internal(data, expr, parent)
}


calculate_internal =  function (data, expr, parent) {
    UseMethod("calculate_internal")
}

#' @export
calculate_internal.data.frame = function (data, expr, parent) {
    # based on 'within' from base R by R Core team
    e = evalq(environment(), data, parent)
    prepare_env(e, n = nrow(data), column_names = colnames(data))
    eval(expr, envir = e, enclos = baseenv())
}

#' @export
calculate_internal.list = function (data, expr, parent) {
    for(each in seq_along(data)){
        data[[each]] = calculate_internal(data[[each]], expr, parent)
    }
    data
}


#' @export
#' @rdname modify
calc = calculate






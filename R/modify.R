#' Modify data.frame/conditionally modify data.frame
#' 
#' \code{modify} evaluates expression \code{expr} in the context of data.frame 
#' \code{data}. It works similar to \code{\link[base]{within}} in base R but try
#' to return new variables in order of their appearance in the expression.
#' \code{modify_if} modifies only rows for which \code{cond} has TRUE. Other 
#' rows remain unchanged. Newly created variables also will have values only in 
#' rows for which \code{cond} has TRUE. There will be NA's in other rows. This 
#' function tries to mimic SPSS "DO IF(). ... END IF." statement. There is a 
#' special constant \code{.n} which equals to number of cases in \code{data} for
#' usage in expression inside \code{modify}. Inside \code{modify_if} \code{.n} 
#' gives number of rows which will be affected by expressions. Inside this 
#' functions you can use \code{set} function which creates variables with given
#' name/set values to existing variables - \link{.set}. It is possible with
#' \code{set} to assign values to multiple variables at once.
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
#'     random_numbers = runif(.n) # .n usage
#' })
#' 
#' # 'set' function
#' # new variables filled with NA
#' modify(dfs, {
#'     set(subst('new_b`1:5`'))
#' })
#' 
#' # 'set' function
#' # set values to existing/new variables
#' modify(dfs, {
#'     set(subst('new_b`1:5`'), b_1 %to% b_5)
#' })
#' 
#' 
#' # conditional modification
#' modify_if(dfs, test %in% 2:4, {
#'     aa = aa + 1    
#'     a_b = aa + b_    
#'     b_total = sum_row(b_, b_1 %to% b_5)
#'     random_numbers = runif(.n) # .n usage
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
    e$.n = nrow(data)
    e$set = set_generator(e$.n)
    eval(substitute(expr), e)
    rm(".n", "set", envir = e)
    l = as.list(e, all.names = TRUE)
    
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    stopif(any(nrows!=1L & nrows!=nrow(data)),"Bad number of rows")
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    nl = c(names(data), new_vars)
    data[nl] = l[nl]
    data
}



#' @export
#' @rdname modify
modify_if = function (data, cond, expr){
    UseMethod("modify_if")
}



#' @export
modify_if = function (data, cond, expr) {
    # based on 'within' from base R by R Core team
    parent = parent.frame()
    cond = substitute(cond)
    cond = eval(cond, data, parent.frame())
    if (!is.logical(cond)) 
        stop("'cond' must be logical")
    cond = cond & !is.na(cond)
    new_data = data[cond,, drop = FALSE]
    e = evalq(environment(), new_data, parent)
    e$.n = nrow(new_data)
    e$set = set_generator(e$.n)
    eval(substitute(expr), e)
    rm(".n", "set", envir = e)
    l = as.list(e, all.names = TRUE)
    
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    stopif(any(nrows!=1L & nrows!=nrow(new_data)),"Bad number of rows")
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    data[cond, names(data)] = l[names(data)]
    data[, new_vars] = NA
    data[cond, new_vars] = l[new_vars]
    data
}




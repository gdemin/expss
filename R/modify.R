#' Modify data.frame
#'
#' @param data 
#' @param expr 
#' @param ... 
#'
#' @return
#'
#' @examples
#' a = 1
#' @export
modify =  function (data, expr, ...) {
    UseMethod("modify")
}

#' @export
modify.data.frame = function (data, expr, ...) {
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    eval(substitute(expr), e)
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
modify_if = function (data, cond, expr, ...){
    UseMethod("modify_if")
}


 
#' @export
modify_if = function (data, cond, expr, ...) {
    parent = parent.frame()
    cond = substitute(cond)
    cond = eval(cond, data, parent.frame())
    if (!is.logical(cond)) 
        stop("'cond' must be logical")
    cond = cond & !is.na(cond)
    new_data = data[cond,, drop = FALSE]
    e = evalq(environment(), new_data, parent)
    eval(substitute(expr), e)
    l = as.list(e, all.names = TRUE)
    
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    stopif(any(nrows!=1L & nrows!=nrow(new_data)),"Bad number of rows")
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    data[cond, names(data)] = l[names(data)]
    data[, new_vars] = NA
    data[cond, new_vars] = l[new_vars]
    data
}




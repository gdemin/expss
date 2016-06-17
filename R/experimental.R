# experimental functions for working with default dataset


# aaa = mtcars

# default_dataset(aaa)

".val_lab<-" = function(x, value){
    
    
}

bbb = function(x, expr, ...) within(x, expr)

# bbb(aaa, {new = 5})

temp = function(x) {
    res = match.call()
    res
}

'temp<-' = function(x, value) {
    res = match.call()
    print(res)
    x
}



#' @export
.modify = function (expr) {
    # based on 'within' from base R by R Core team
    reference = suppressMessages(default_dataset())
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    e$.n = nrow(data)
    eval(substitute(expr), e)
    rm(".n", envir = e)
    l = as.list(e, all.names = TRUE)
    
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    stopif(any(nrows!=1L & nrows!=nrow(data)),"Bad number of rows")
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    nl = c(names(data), new_vars)
    data[nl] = l[nl]
    ref(reference) = data
    invisible(NULL)
}



#' @export
.modify_if = function (cond, expr) {
    # based on 'within' from base R by R Core team
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    cond = substitute(cond)
    cond = eval(cond, data, parent.frame())
    if (!is.logical(cond)) 
        stop("'cond' must be logical")
    cond = cond & !is.na(cond)
    new_data = data[cond,, drop = FALSE]
    e = evalq(environment(), new_data, parent)
    e$.n = nrow(new_data)
    eval(substitute(expr), e)
    rm(".n", envir = e)
    l = as.list(e, all.names = TRUE)
    
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    stopif(any(nrows!=1L & nrows!=nrow(new_data)),"Bad number of rows")
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    data[cond, names(data)] = l[names(data)]
    data[, new_vars] = NA
    data[cond, new_vars] = l[new_vars]
    ref(reference) = data
    invisible(NULL)
}


#' @export
.do_if = .modify_if


#' @export
.compute = .modify


#' @export
.set_val_lab = function(x, value){
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    ref(reference) = modify(data, {
        
        list2env(set_val_lab(x, value))   
    })
    invisible(NULL)
}
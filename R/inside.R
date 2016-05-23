# early draft
# 'within' with correct order of new variables 
inside = function (data, expr, ...) {
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    eval(substitute(expr), e)
    l = as.list(e, all.names = TRUE)
    
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    # stopifnot(all(nrows==1L | nrows==nrow(data)),"Bad number of rows")
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    nl = c(names(data), new_vars)
    data[nl] = l[nl]
    data
}
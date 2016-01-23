#' @export
#' @rdname count_if
crit = function(cond,...){
    UseMethod("crit")
}

crit.default = function(cond,...){
    cond = match.fun(cond)
    res = function(x) cond(x,...)
    class(res) = union("criterion",class(res))
    res
}

#' @export
crit.logical = function(cond, ...){
    
}

#' @export
'!.criterion' = function(a) {
    res = function(x) !a(x)
    class(res) = union("criterion",class(res))
    res
}


#' @export
'|.criterion' = function(e1,e2) {
    # one or both e1, e2 is criterion and criterion can be only logical or function
    if (is.function(e1)) {
        f1 = e1
    } else {
        f1 = function(x) x %in% e1
    }
    if (is.function(e2)) {
        f2 = e2
    } else {
        f2 = function(x) x %in% e2
    }
    res = function(x) f1(x) | f2(x)
    class(res) = union("criterion",class(res))
    res
}


#' @export
'&.criterion' = function(e1,e2) {
    # one or both e1, e2 is criterion and criterion can be only logical or function
    if (is.function(e1)) {
        f1 = e1
    } else {
        f1 = function(x) x %in% e1
    }
    if (is.function(e2)) {
        f2 = e2
    } else {
        f2 = function(x) x %in% e2
    }
    res = function(x) f1(x) & f2(x)
    class(res) = union("criterion",class(res))
    res
}


# TODO Удалить
#' @export
build_criterion = function(criterion,dfs){
    if (is.null(criterion)){
        cond = !is.na(dfs)
        return(cond)
    }
    UseMethod("build_criterion")
}

# TODO Удалить
# @export
build_criterion.function = function(criterion,dfs){
    res = lapply(dfs,function(colmn){
        cond = criterion(colmn)
        stopif(length(cond)!=length(colmn),"Cells number of criterion doesn't equal cells number of argument.")
        as.logical(cond)
    })
    do.call(cbind,res)
}

# TODO Удалить
# @export
build_criterion.default = function(criterion,dfs){
    build_criterion.function(function(x) x %in% criterion,dfs)
}

# TODO Удалить
# @export
build_criterion.criterion = function(criterion,dfs){
    if(is.logical(criterion)){
        build_criterion_logical(criterion,dfs)
    } else {
        build_criterion.function(criterion,dfs) 
    }
}

build_criterion_logical = function(criterion,dfs){
    
}





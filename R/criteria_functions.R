#' @export
#' @rdname count_if
eq = function(x){
    force(x)
    res = function(y) {
        cond = y == x
        cond
    }
    class(res) = union("criterion",class(res))
    res
}

#' @export
#' @rdname count_if
neq = function(x){
    force(x)
    res = function(y) {
        cond = y != x
        cond   
    }    
    class(res) = union("criterion",class(res))
    res
    
}

#' @export
#' @rdname count_if
lt = function(x){
    
    build_compare(x,"<")    
}

#' @export
#' @rdname count_if
gt = function(x){
    
    build_compare(x,">")    
    
}

#' @export
#' @rdname count_if
lte = function(x){
    build_compare(x,"<=")    

}

#' @export
#' @rdname count_if
gte = function(x){
    build_compare(x,">=")       
}

build_compare = function(x, compare){
    UseMethod("build_compare")
    
}

build_compare.default = function(x, compare){
    force(x)
    force(compare)
    FUN = match.fun(compare)
    res = function(y){
       FUN(y,x)
    }
    class(res) = union("criterion",class(res))
    res
}

build_compare.numeric = function(x, compare){
    force(x)
    force(compare)
    FUN = match.fun(compare)
    res = function(y){
        if(is.numeric(y)){
            FUN(y,x)
        } else {
            matrix(FALSE, nrow=NROW(y), ncol=NCOL(y))
        }
    }
    class(res) = union("criterion",class(res))
    res
    
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











#' Infix operations on vectors - append, diff, intersection, union, replication
#' 
#' \itemize{
#' \item{\code{\%a\%}}{ a(ppends) second argument to first argument. See also
#' \link[base]{append}.}
#' \item{\code{\%u\%} and \code{v_union}}{ u(nite) first and second arguments.
#' Remove elements from second argument which exist in first argument. }
#' \item{\code{\%d\%} and \code{v_diff}}{ d(iff) second argument from first
#' argument. Second argument could be a function which returns logical value. In
#' this case elements of first argument which give TRUE will be removed. }
#' \item{\code{\%i\%} and \code{v_intersect}}{ i(ntersect) first argument and
#' second argument. Second argument could be a function which returns logical
#' value. In this case elements of first argument which give FALSE will be
#' removed. }
#' \item{\code{\%e\%} and \code{v_xor}}{ e(xclusive OR). Returns elements that
#' contained only in one of arguments.}
#' \item{\code{\%r\%} }{ r(epeats) first argument second argument times. See
#' also \link[base]{rep}.}
#' \item{\code{\%n_d\%} and \code{n_diff}}{ n(ames) d(iff) - diff second argument
#' from names of first argument. Second argument could be a function which
#' returns logical value. In this case elements of first argument which names
#' give TRUE will be removed. }
#' \item{\code{\%n_i\%} and \code{n_intersect}}{ n(ames) i(ntersect) - intersect
#' names of first argument with second argument. Second argument could be a
#' function which returns logical value. In this case elements of first argument
#' which names give FALSE will be removed. }
#' } 
#' All these functions except \code{\%n_d\%}, \code{\%n_i\%} preserve names of
#' vectors and don't remove duplicates.
#' For \code{\%d\%}, \code{\%i\%}, \code{\%n_d\%}, \code{\%n_i\%} one can use
#' criteria functions. See \link{criteria} for details.
#'  
#' @param e1 vector (possibly data.frame/matrix/list for \code{\%n_d\%}, \code{\%n_i\%})
#' @param e2 vector (or function for \code{\%d\%}, \code{\%i\%})
#' 
#' @name vectors
#' @return vector (possibly data.frame/matrix/list for \code{\%n_d\%}, \code{\%n_i\%})
#' 
#' @examples 
#' 
#' 1:4 %a% 5:6   # 1:6
#' 
#' 1:4 %a% 4:5   # 1,2,3,4,4,5
#' 
#' 1:4 %u% 4:5   # 1,2,3,4,5
#' 
#' 1:6 %d% 5:6   # 1:4
#' 
#' # function as criterion
#' 1:6 %d% gt(4) # 1:4
#' 
#' 1:4 %i% 4:5   # 4
#' 
#' # function as criterion
#' letters %i% perl("[a-d]") # a,b,c,d
#' 
#' # function as criterion 
#' letters %i% (fixed("a") | fixed("z")) # a, z
#' 
#' 1:4 %e% 4:5   # 1, 2, 3, 5
#' 
#' 1:2 %r% 2     # 1, 2, 1, 2
#' 
#' # %n_i%, %n_d%
#' 
#' # remove column Species
#' iris %n_d% "Species" 
#' 
#' # leave only columns which names start with "Sepal"
#' iris %n_i% perl("^Sepal") 
#' 
#' # leave column "Species" and columns which names start with "Sepal" 
#' iris %n_i% (perl("^Sepal")|"Species") 
#'
#' @export
'%a%' = function(e1, e2){
    append(e1, e2)
}

#' @export
#' @rdname vectors
v_union = function(e1, e2){
    if(is.null(e1)) return(e2)
    c(e1, e2[!(e2 %in% e1)])
}

#' @export
#' @rdname vectors
'%u%' = v_union

#' @export
#' @rdname vectors
v_diff = function(e1, e2){
    UseMethod("v_diff")
}

#' @export
v_diff.default = function(e1, e2){
    if(is.null(e2)) return(e1)
    if (is.function(e2)){
        crit = e2(e1)
        crit = !(crit & !is.na(crit))
        e1[crit]
    } else {
        e1[!(e1 %in% e2)]
    }
}

#' @export
v_diff.list = function(e1, e2){
    if(is.null(e2)) return(e1)
    stopif(!is.function(e2), "For lists 'e2' should be function.")
    crit = vapply(e1, FUN = e2, FUN.VALUE = logical(1))
    crit = !(crit & !is.na(crit))
    if(is.data.table(e1)){
        e1[,crit, with = FALSE]
    } else {
        e1[crit]
    }
}

#' @export
v_diff.data.frame = v_diff.list

#' @export
#' @rdname vectors
'%d%' = v_diff

#' @export
#' @rdname vectors
v_intersect = function(e1, e2){
    UseMethod("v_intersect")
}

#' @export
v_intersect.default = function(e1, e2){
    if (is.function(e2)){
        crit = e2(e1)
        crit = crit & !is.na(crit)
        e1[crit]
    } else {
        e1[e1 %in% e2]
    }
}

#' @export
v_intersect.list = function(e1, e2){
    if(is.null(e2)) return(e1[FALSE])
    stopif(!is.function(e2), "For lists 'e2' should be function.")
    crit = vapply(e1, FUN = e2, FUN.VALUE = logical(1))
    crit = crit & !is.na(crit)
    if(is.data.table(e1)){
        e1[,crit, with = FALSE]
    } else {
        e1[crit]
    }
}


#' @export
v_intersect.data.frame = v_intersect.list

#' @export
#' @rdname vectors
'%i%' = v_intersect

#' @export
#' @rdname vectors
v_xor = function(e1, e2){
    if(is.null(e1)) return(e2)
    c(e1[!(e1 %in% e2)],e2[!(e2 %in% e1)])
}

#' @export
#' @rdname vectors
'%e%' = v_xor


#' @export
#' @rdname vectors
'%r%' = function(e1, e2){
    if(length(e2)==1){
        rep(e1, e2)
    } else {
        stop("Multiplicator 'e2' should be scalar quantity (vector of length 1).")
    }
}

#' @export
#' @rdname vectors
n_intersect = function(e1, e2){
    UseMethod("n_intersect")
}

#' @export
n_intersect.default = function(e1, e2){
    # names %in% .... for duplicated names
    e1[names(e1) %in% (names(e1) %i% e2)]    
}

#' @export
n_intersect.data.frame = function(e1, e2){
    e1[ , names(e1) %in% (names(e1) %i% e2), drop = FALSE]    
}

#' @export
n_intersect.data.table = function(e1, e2){
    e1[ , names(e1) %in% (names(e1) %i% e2), with = FALSE]    
}

#' @export
n_intersect.matrix = function(e1, e2){
    e1[ , colnames(e1) %in% (colnames(e1) %i% e2), drop = FALSE]    
}

#' @export
#' @rdname vectors
'%n_i%' = n_intersect

#' @export
#' @rdname vectors
n_diff = function(e1, e2){
    if(length(e2)==0) return(e1)
    UseMethod("n_diff")
}

#' @export
n_diff.default = function(e1, e2){
    
    e1[names(e1) %in% (names(e1) %d% e2)]    
}

#' @export
n_diff.data.frame = function(e1, e2){
    e1[ , names(e1) %in% (names(e1) %d% e2), drop = FALSE]    
}

#' @export
n_diff.data.table = function(e1, e2){
    e1[ , names(e1) %in% (names(e1) %d% e2), with = FALSE]    
}

#' @export
n_diff.matrix = function(e1, e2){
    e1[ , colnames(e1) %in% (colnames(e1) %d% e2), drop = FALSE]    
}

#' @export
#' @rdname vectors
'%n_d%' = n_diff






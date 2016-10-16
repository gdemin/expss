#' Infix operations on vectors - append, diff, intersection, union, replication
#' 
#' \itemize{
#' \item{\code{\%a\%}}{ a(ppends) second argument to first argument.}
#' \item{\code{\%u\%}}{ u(nites) first and second arguments. Remove elements from
#' second argument that exist in first argument. }
#' \item{\code{\%d\%}}{ d(iffs) second argument from first argument. Second
#' argument could be a function which returns logical value. In this case
#' elements of first argument which give TRUE will be removed. }
#' \item{\code{\%i\%}}{ i(ntersects) first argument and second argument. Second
#' argument could be a function which returns logical value. In this case
#' elements of first argument which give FALSE will be removed. } 
#' \item{\code{\%e\%}}{ e(xclusive OR). Returns elements that contained only in one of arguments.}
#' \item{\code{\%r\%}}{ r(epeats) first argument second argument times.}
#' \item{\code{\%n_d\%}}{n(ames) d(iff) - diffs second argument from names of first argument. Second
#' argument could be a function which returns logical value. In this case
#' elements of first argument which names give TRUE will be removed. }
#' \item{\code{\%n_i\%}}{n(ames) i(ntersect) - intersects names of first argument with second argument. Second
#' argument could be a function which returns logical value. In this case
#' elements of first argument which names give FALSE will be removed. } 
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
'%u%' = function(e1, e2){
    if(is.null(e1)) return(e2)
    c(e1, e2[!(e2 %in% e1)])
}


#' @export
#' @rdname vectors
'%d%' = function(e1, e2){
    if (is.function(e2)){
        e1[!e2(e1)]
    } else {
        e1[!(e1 %in% e2)]
    }
}

#' @export
#' @rdname vectors
'%i%' = function(e1, e2){
    if (is.function(e2)){
        e1[e2(e1)]
    } else {
        e1[e1 %in% e2]
    }
}


#' @export
#' @rdname vectors
'%e%' = function(e1, e2){
    if(is.null(e1)) return(e2)
    c(e1[!(e1 %in% e2)],e2[!(e2 %in% e1)])
}

#' @export
#' @rdname vectors
'%r%' = function(e1, e2){
    if(length(e2)==1){
        rep(e1, e2)
    } else {
        stop("Multiplicator should be scalar quantity (vector of length 1).")
    }
}

#' @export
#' @rdname vectors
'%n_d%' = function(e1, e2){
    n_d(e1, e2)
}

#' @export
#' @rdname vectors
'%n_i%' = function(e1, e2){
    n_i(e1, e2)
}


n_i = function(e1, e2){
    UseMethod("n_i")
}


# names %in% .... for duplicated names
n_i.default = function(e1, e2){
    e1[names(e1) %in% (names(e1) %i% e2)]    
}

n_i.data.frame = function(e1, e2){
    e1[ , names(e1) %in% (names(e1) %i% e2), drop = FALSE]    
}

n_i.matrix = function(e1, e2){
    e1[ , colnames(e1) %in% (colnames(e1) %i% e2), drop = FALSE]    
}

n_d = function(e1, e2){
    UseMethod("n_d")
}

n_d.default = function(e1, e2){
    e1[names(e1) %in% (names(e1) %d% e2)]    
}

n_d.data.frame = function(e1, e2){
    e1[ , names(e1) %in% (names(e1) %d% e2), drop = FALSE]    
}

n_d.matrix = function(e1, e2){
    e1[ , colnames(e1) %in% (colnames(e1) %d% e2), drop = FALSE]    
}


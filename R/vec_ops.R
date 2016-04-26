#' Infix operations on vectors - append, diff, intersection, union, replication
#' @param e1 vector
#' @param e2 vector (or function for \code{\%d\%}, \code{\%i\%})
#' 
#' @return vector
#' 
#' @details 
#' \itemize{
#' \item{\code{\%a\%}}{ a(ppends) second argument to first argument.}
#' \item{\code{\%u\%}}{ u(ites) first and second arguments. Remove elements from
#' second argument that exist in first argument. }
#' \item{\code{\%d\%}}{ d(iffs) second argument from first argument. Second
#' argument could be a function which returns logical value. In this case
#' elements of first argument which give TRUE will be removed }
#' \item{\code{\%i\%}}{ i(ntersects) first argument and second argument. Second
#' argument could be a function which returns logical value. In this case
#' elements of first argument which give FALSE will be removed } 
#' \item{\code{\%x\%}}{ x(or). Returns elements that contained only in one of argument.}
#' \item{\code{\%r\%}}{ r(epeats) first argument second argument times}
#' } 
#' 
#' All these functions preserve names of vectors and doesn't remove duplicates.
#' For \code{\%d\%} and \code{\%i\%} one can use criteria functions. See \link{count_if}
#'  for details.
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
#' 1:6 %d% gt(4) # 1:4
#' 
#' 1:4 %i% 4:5   # 4
#' 
#' letters %i% perl("[a-d]") # a,b,c,d
#'  
#' letters %i% (fixed("a") | fixed("z")) # a, z
#' 
#' 1:4 %x% 4:5   # 1, 2, 3, 5
#' 
#' 1:2 %r% 2     # 1, 2, 1, 2
#'
#' @export
'%a%' = function(e1, e2){
    append(e1, e2)
}

#' @export
#' @rdname grapes-a-grapes
'%u%' = function(e1, e2){
    c(e1, e2[!(e2 %in% e1)])
}


#' @export
#' @rdname grapes-a-grapes
'%d%' = function(e1, e2){
    if (is.function(e2)){
        e1[!e2(e1)]
    } else {
        e1[!(e1 %in% e2)]
    }
}

#' @export
#' @rdname grapes-a-grapes
'%i%' = function(e1, e2){
    if (is.function(e2)){
        e1[e2(e1)]
    } else {
        e1[e1 %in% e2]
    }
}


#' @export
#' @rdname grapes-a-grapes
'%x%' = function(e1, e2){
    c(e1[!(e1 %in% e2)],e2[!(e2 %in% e1)])
}

#' @export
#' @rdname grapes-a-grapes
'%r%' = function(e1, e2){
    if(length(e2)==1){
        rep(e1, e2)
    } else {
        stop("Multiplicator should be scalar quantity.")
    }
}






#' Criteria functions
#' 
#' These functions returns criteria functions which could be used in different 
#' situation - see \link{if_val}, \link{na_if}, \link{\%i\%}, \link{\%d\%}, 
#' \link{count_if}, \link{match_row} etc. For example, \code{gt(5)} returns
#' function which tests whether its argument greater than five. 
#' \code{fixed("apple")} return function which tests whether its argument
#' contains "apple". Logical operations (|, &, !, xor) defined for these
#' functions.
#' List of functions:
#' \itemize{
#' \item{\code{gt}}{ greater than}
#' \item{\code{gte}}{ greater than or equal}
#' \item{\code{eq}}{ equal} 
#' \item{\code{neq}}{ not equal} 
#' \item{\code{lt}}{ less than}
#' \item{\code{lte}}{ less than or equal}
#' \item{\code{thru}}{ checks whether value is inside interval.
#' \code{thru(0,1)} is equivalent of \code{x>=0 & x<=1} or \code{gte(0) &
#' lte(1)}}
#' \item{\code{\%thru\%}}{ infix version of \code{thru}, e. g. \code{0 \%thru\% 1}}
#' \item{\code{regex}}{ use POSIX 1003.2 extended regular expressions. For details see \link[base]{grepl}}
#' \item{\code{perl}}{ perl-compatible regular expressions. For details see \link[base]{grepl}}
#' \item{\code{fixed}}{ pattern is a string to be matched as is. For details see \link[base]{grepl}}
#' } 
#' 
#' @param x vector 
#' @param lower vector/single value - lower bound of interval 
#' @param upper vector/single value - upper bound of interval 
#' @param pattern character string containing a regular expression (or character
#'   string for \code{fixed}) to be matched in the given character vector.
#'   Coerced by as.character to a character string if possible.
#' @param ignore.case logical see \link[base]{grepl}
#' @param useBytes logical see \link[base]{grepl}
#'
#' @return function of class 'criterion' which tests its argument against condition and return logical value
#' 
#' @seealso \link{count_if}, \link{match_row}, \link{if_val}, \link{na_if}, \link{\%i\%}, \link{\%d\%}  
#' @examples
#' # operations on vector
#' 1:6 %d% gt(4) # 1:4
#' 
#' 1:6 %d% (1 | gt(4)) # 2:4
#' 
#' letters %i% (fixed("a") | fixed("z")) # a, z
#' 
#' # examples with count_if
#' df1 = data.frame(
#'     a=c("apples",   "oranges",     "peaches",     "apples"),
#'     b = c(32, 54, 75, 86)
#' )
#' 
#' count_if(gt(55), df1$b) # greater than 55 = 2
#' 
#' count_if(neq(75), df1$b) # not equal 75 = 3
#' 
#' count_if(gte(32), df1$b) # greater than or equal 32 = 4
#' 
#' count_if(gt(32) & lt(86), df1$b) # greater than 32 and less than 86 = 2
#' 
#' # via different kinds of 'thru'
#' count_if(thru(35, 80), df1$b) # greater than or equals to 35 and less than or equals to 80 = 2
#' # infix version
#' count_if(35 %thru% 80, df1$b) # greater than or equals to 35 and less than or equals to 80 = 2
#' 
#' # values that started on 'a'
#' count_if(regex("^a"),df1) # 2
#' 
#' # count_row_if
#' count_row_if(regex("^a"),df1) # c(1,0,0,1)
#' 
#' # if_val examples
#' # From SPSS: RECODE QVAR(1 THRU 5=1)(6 THRU 10=2)(11 THRU HI=3)(ELSE=0).
#' set.seed(123)
#' qvar = sample((-5):20, 50, replace = TRUE)
#' if_val(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, 11 %thru% Inf ~ 3, . ~ 0)
#' # the same result
#' if_val(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, gte(11) ~ 3, . ~ 0)
#' 
#' 
#' @name criteria
#' @export
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
#' @rdname criteria
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
#' @rdname criteria
lt = function(x){
    
    build_compare(x,"<")    
}

#' @export
#' @rdname criteria
gt = function(x){
    
    build_compare(x,">")    
    
}

#' @export
#' @rdname criteria
lte = function(x){
    build_compare(x,"<=")    

}

#' @export
#' @rdname criteria
gte = function(x){
    build_compare(x,">=")       
}

#' @export
#' @rdname criteria
perl = function(pattern, ignore.case = FALSE, useBytes = FALSE){
    pattern
    res = function(x){
        grepl(pattern, x, ignore.case = ignore.case, perl = TRUE, fixed = FALSE, useBytes = useBytes)
    }
    class(res) = union("criterion",class(res))
    res
}

#' @export
#' @rdname criteria
regex = function(pattern, ignore.case = FALSE, useBytes = FALSE){
    pattern
    res = function(x){
        grepl(pattern, x, ignore.case = ignore.case, perl = FALSE, fixed = FALSE, useBytes = useBytes)
    }
    class(res) = union("criterion",class(res))
    res
}

#' @export
#' @rdname criteria
fixed = function(pattern, ignore.case = FALSE, useBytes = FALSE){
    pattern
    res = function(x){
        grepl(pattern, x, ignore.case = ignore.case, perl = FALSE, fixed = TRUE, useBytes = useBytes)
    }
    class(res) = union("criterion",class(res))
    res
}

#' @export
#' @rdname criteria
thru = function(lower, upper){
    stopif(is.function(lower) | is.function(upper),
           "'thru' not defined for functions but 'lower' = ", lower, " and 'upper' = ", upper)
    force(lower)
    force(upper)
    gte(lower) & lte(upper)
}

#' @export
#' @rdname criteria
'%thru%' = function(lower, upper) thru(lower, upper)

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
    a
    res = function(x) !a(x)
    class(res) = union("criterion",class(res))
    res
}


#' @export
'|.criterion' = function(e1,e2) {
    # one or both e1, e2 is criterion and criterion can be only logical or function
    e1
    e2
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
    e1
    e2
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











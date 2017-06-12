#' Criteria functions
#' 
#' These functions returns criteria functions which could be used in different 
#' situation - see \link{keep}, \link{except}, \link{recode}, \link{na_if},
#' \link{\%i\%}, \link{\%d\%}, \link{count_if}, \link{match_row} etc. For
#' example, \code{gt(5)} returns function which tests whether its argument
#' greater than five.
#' \code{fixed("apple")} return function which tests whether its argument
#' contains "apple". Logical operations (|, &, !, xor) are defined for these
#' functions.
#' List of functions:
#' \itemize{
#' \item{\code{gt}}{ greater than}
#' \item{\code{ge}/\code{gte}}{ greater than or equal}
#' \item{\code{eq}}{ equal} 
#' \item{\code{ne}/\code{neq}}{ not equal} 
#' \item{\code{lt}}{ less than}
#' \item{\code{le}/\code{lte}}{ less than or equal}
#' \item{\code{thru}}{ checks whether value is inside interval.
#' \code{thru(0,1)} is equivalent of \code{x>=0 & x<=1} or \code{ge(0) &
#' le(1)}}
#' \item{\code{\%thru\%}}{ infix version of \code{thru}, e. g. \code{0 \%thru\% 1}}
#' \item{\code{regex}}{ use POSIX 1003.2 extended regular expressions. For details see \link[base]{grepl}}
#' \item{\code{perl}}{ perl-compatible regular expressions. For details see \link[base]{grepl}}
#' \item{\code{fixed}}{ pattern is a string to be matched as is. For details see \link[base]{grepl}}
#' \item{\code{to}}{ returns function which gives TRUE for all elements of
#' vector before the first occurrence of \code{x} and for  \code{x}.}
#' \item{\code{from}}{ returns function which gives TRUE for all elements of 
#' vector after the first occurrence of \code{x} and for \code{x}. \code{from} and
#' \code{to} are intended for usage with \link{keep} and \link{except}.}
#' \item{\code{not_na}}{ return TRUE for all non-NA elements of vector.} 
#' \item{\code{other}}{ return TRUE for all elements of vector. It is intended
#' for usage with \code{if_val}, \code{keep}, \code{except}}
#' \item{\code{items}}{ return TRUE for elements of vector with given
#' sequential number. It is intended for usage with \code{keep}, \code{except}}
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
#' @param ... numeric indexes of desired items
#' @param crit vector of values/function which returns logical or vector. It will be
#'   converted to function of class criterion.
#'
#' @return function of class 'criterion' which tests its argument against
#'   condition and return logical value
#' 
#' @seealso \link{recode}, \link{keep}, \link{except}, \link{count_if},
#'   \link{match_row}, \link{na_if}, \link{\%i\%}, \link{\%d\%}
#' @examples
#' # operations on vector
#' 1:6 %d% gt(4) # 1:4
#' 
#' 1:6 %d% (1 | gt(4)) # 2:4
#' 
#' letters %i% (fixed("a") | fixed("z")) # a, z
#' 
#' letters %i% from("w")  # w, x, y, z
#' 
#' letters %i% to("c")  # a, b, c
#' 
#' letters %i% (from("b") & to("e"))  # b, d, e
#' 
#' c(1, 2, NA, 3) %i% other # c(1, 2, 3)
#' 
#' # examples with count_if
#' df1 = data.frame(
#'     a=c("apples", "oranges", "peaches", "apples"),
#'     b = c(32, 54, 75, 86)
#' )
#' 
#' count_if(gt(55), df1$b) # greater than 55 = 2
#' 
#' count_if(ne(75), df1$b) # not equal 75 = 3
#' 
#' count_if(ge(32), df1$b) # greater than or equal 32 = 4
#' 
#' count_if(gt(32) & lt(86), df1$b) # greater than 32 and less than 86 = 2
#' 
#' # via different kinds of 'thru'
#' count_if(thru(35, 80), df1$b) # greater than or equals to 35 and less than or equals to 80 = 2
#' # infix version
#' count_if(35 %thru% 80, df1$b) # greater than or equals to 35 and less than or equals to 80 = 2
#' 
#' # values that started on 'a'
#' count_if(regex("^a"), df1) # 2
#' 
#' # count_row_if
#' count_row_if(regex("^a"), df1) # c(1,0,0,1)
#' 
#' # examples with 'keep' and 'except'
#' 
#' data(iris)
#' iris %>% keep(to("Petal.Width")) # column 'Species' will be removed 
#'  
#' # 'Sepal.Length', 'Sepal.Width' will be left 
#' iris %>% except(from("Petal.Length"))
#' 
#' # except first column
#' iris %n_d% items(1)
#' 
#' # if_val examples
#' # From SPSS: RECODE QVAR(1 THRU 5=1)(6 THRU 10=2)(11 THRU HI=3)(ELSE=0).
#' set.seed(123)
#' qvar = sample((-5):20, 50, replace = TRUE)
#' recode(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, 11 %thru% hi ~ 3, other ~ 0)
#' # the same result
#' recode(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, ge(11) ~ 3, other ~ 0)
#' 
#' 
#' @name criteria
#' @export
eq = function(x){
    force(x)
    as.criterion(function(y) {
        y == x
    })
    
}

#' @export
#' @rdname criteria
ne = function(x){
    force(x)
    as.criterion(function(y) {
        y != x
    })  
    
    
}

#' @export
#' @rdname criteria
neq = ne

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
le = function(x){
    build_compare(x,"<=")    

}

#' @export
#' @rdname criteria
lte = le

#' @export
#' @rdname criteria
ge = function(x){
    build_compare(x,">=")       
}

#' @export
#' @rdname criteria
gte = ge

#' @export
#' @rdname criteria
perl = function(pattern, ignore.case = FALSE, useBytes = FALSE){
    pattern
    ignore.case
    useBytes
    as.criterion(function(x){
        grepl(pattern, x, ignore.case = ignore.case, perl = TRUE, fixed = FALSE, useBytes = useBytes)
    })
    
}

#' @export
#' @rdname criteria
regex = function(pattern, ignore.case = FALSE, useBytes = FALSE){
    pattern
    ignore.case
    useBytes
    as.criterion(function(x){
        grepl(pattern, x, ignore.case = ignore.case, perl = FALSE, fixed = FALSE, useBytes = useBytes)
    })
}

#' @export
#' @rdname criteria
fixed = function(pattern, ignore.case = FALSE, useBytes = FALSE){
    pattern
    ignore.case
    useBytes
    as.criterion(function(x){
        grepl(pattern, x, ignore.case = ignore.case, perl = FALSE, fixed = TRUE, useBytes = useBytes)
    })
}

#' @export
#' @rdname criteria
thru = function(lower, upper){
    stopif(is.function(lower) | is.function(upper),
           "'thru' is not defined for functions but 'lower' = ", lower, " and 'upper' = ", upper)
    force(lower)
    force(upper)
    ge(lower) & le(upper)
}


#' @export
#' @rdname criteria
'%thru%' = function(lower, upper) thru(lower, upper)


#' @export
#' @rdname criteria
from = function(x){
    x
    as.criterion(function(y){
        first = match_col(x, y)[1]
        stopif(is.na(first), "'",x, "' not found." )
        positions = seq_along(y)
        positions>=first
        
    })

}

#' @export
#' @rdname criteria
to = function(x){
    x
    as.criterion(function(y){
        last = match_col(x, y)[1]
        stopif(is.na(last), "'",x, "' not found." )
        positions = seq_along(y)
        positions<=last
        
    })
}

#' @export
#' @rdname criteria
items = function(...){
    args = c(list(...), recursive = TRUE)
    as.criterion(function(x){
        numbers = seq_along(x)    
        numbers %in% args
    })

}


#' @export
#' @rdname criteria
not_na = function(x){
    if(missing(x)){
        not_na
    } else {
        !is.na(x)
    }    
}

class(not_na) = union("criterion",class(not_na))

#' @export
#' @rdname criteria
other = function(x){
    if(missing(x)){
        other    
    } else {
        rep(TRUE, NROW(x)) 
    }    
}


class(other) = union("criterion",class(other))

build_compare = function(x, compare){
    UseMethod("build_compare")
    
}

build_compare.default = function(x, compare){
    force(x)
    force(compare)
    FUN = match.fun(compare)
    as.criterion(function(y){
       FUN(y,x)
    })
}

build_compare.numeric = function(x, compare){
    force(x)
    force(compare)
    FUN = match.fun(compare)
    as.criterion(function(y){
        if(is.numeric(y)){
            FUN(y,x)
        } else {
            matrix(FALSE, nrow = NROW(y), ncol = NCOL(y))
        }
    })
    
    
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
    f1 = as.criterion(e1)
    f2 = as.criterion(e2)
    res = function(x) f1(x) | f2(x)
    class(res) = union("criterion",class(res))
    res
}


#' @export
'&.criterion' = function(e1,e2) {
    f1 = as.criterion(e1)
    f2 = as.criterion(e2)
    res = function(x) f1(x) & f2(x)
    class(res) = union("criterion",class(res))
    res
}


#' @export
#' @rdname criteria
as.criterion = function(crit){
    force(crit)
    if (is.function(crit)) {
        crit = match.fun(crit)
        res = function(x) {
            cond = crit(x)
            cond & !is.na(cond)
        }
    } else {
        if(is.logical(crit) && !(length(crit) == 1L && is.na(crit))){
            res = function(x) crit & !is.na(crit)
        } else {
            res = function(x) x %in% crit       
        }    
    }
    class(res) = union("criterion",class(res))
    res
}








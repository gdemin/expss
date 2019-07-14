#' Criteria functions
#' 
#' Produce criteria which could be used in the different situations - see
#' '\link{recode}', '\link{na_if}', '\link{count_if}', '\link{match_row}',
#' '\link{\%i\%}' and etc. For example, \code{'greater(5)'} returns function
#' which tests whether its argument greater than five. \code{'fixed("apple")'}
#' returns function which tests whether its argument contains "apple". For
#' criteria logical operations (|, &, !, xor) are defined, e. g. you can write
#' something like: \code{'greater(5) | equals(1)'}.
#' List of functions:
#' \itemize{
#' \item{comparison criteria - \code{'equals'}, \code{'greater'} and etc.}{ return
#' functions which compare its argument against value.}
#' \item{\code{'thru'}}{ checks whether a value is inside interval.
#' \code{'thru(0,1)'} is equivalent to \code{'x>=0 & x<=1'}}
#' \item{\code{'\%thru\%'}}{ is infix version of \code{'thru'}, e. g. \code{'0
#' \%thru\% 1'}}
#' \item{\code{'is_max'} and \code{'is_min'}}{ return TRUE where vector value is
#' equals to maximum or minimum.}
#' \item{\code{'contains'}}{ searches for the pattern in the strings. By default,
#' it works with fixed patterns rather than regular expressions. For details
#' about its arguments see \link[base]{grepl}}
#' \item{\code{'like'}}{ searches for the Excel-style pattern in the strings. You
#' can use wildcards: '*' means any number of symbols, '?' means single symbol.
#' Case insensitive.}
#' \item{\code{'fixed'}}{ alias for contains.}
#' \item{\code{'perl'}}{ such as \code{'contains'} but the pattern is perl-compatible
#' regular expression (\code{'perl = TRUE'}). For details see \link[base]{grepl}}
#' \item{\code{'regex'}}{ use POSIX 1003.2 extended regular expressions
#' (\code{'fixed = FALSE'}). For details see \link[base]{grepl}}
#' \item{\code{'has_label'}}{ searches values which have supplied label(-s).  We
#' can used criteria as an argument for 'has_label'.}
#' \item{\code{'to'}}{ returns function which gives TRUE for all elements of
#' vector before the first occurrence of \code{'x'} and for  \code{'x'}.}
#' \item{\code{'from'}}{ returns function which gives TRUE for all elements of 
#' vector after the first occurrence of \code{'x'} and for \code{'x'}.}
#' \item{\code{'not_na'}}{ returns TRUE for all non-NA vector elements.} 
#' \item{\code{'other'}}{ returns TRUE for all vector elements. It is intended
#' for usage with \code{'recode'}.}
#' \item{\code{'items'}}{ returns TRUE for the vector elements with the given
#' sequential numbers.}
#' \item{\code{'and'}, \code{'or'}, \code{'not'}}{ spreadsheet-style boolean functions.}
#' } 
#' Shortcuts for comparison criteria:
#' \itemize{
#' \item{'equals'}{ - \code{'eq'}}
#' \item{'not_equals'}{ - \code{'neq'}, \code{'ne'}}
#' \item{'greater'}{ - \code{'gt'}}
#' \item{'greater_or_equal'}{ - \code{'gte'}, \code{'ge'}}
#' \item{'less'}{ - \code{'lt'}}
#' \item{'less_or_equal'}{ - \code{'lte'}, \code{'le'}}
#' }
#' @param x vector 
#' @param lower vector/single value - lower bound of interval 
#' @param upper vector/single value - upper bound of interval 
#' @param pattern character string containing a regular expression (or character
#'   string for \code{'fixed'}) to be matched in the given character vector.
#'   Coerced by as.character to a character string if possible.
#' @param perl logical see \link[base]{grepl}
#' @param fixed logical see \link[base]{grepl}
#' @param ignore.case logical see \link[base]{grepl}
#' @param useBytes logical see \link[base]{grepl}
#' @param ... numeric indexes of desired items for items, logical vectors or criteria for boolean functions.
#' @param crit vector of values/function which returns logical or vector. It will be
#'   converted to function of class criterion.
#'
#' @return function of class 'criterion' which tests its argument against
#'   condition and return logical value
#' 
#' @seealso \link{recode}, \link{count_if},
#'   \link{match_row}, \link{na_if}, \link{\%i\%}
#' @examples
#' # operations on vector, '%d%' means 'diff'
#' 1:6 %d% greater(4) # 1:4
#' 1:6 %d% (1 | greater(4)) # 2:4
#' # '%i%' means 'intersect
#' 1:6 %i% (is_min() | is_max()) # 1, 6
#' # with Excel-style boolean operators
#' 1:6 %i% or(is_min(), is_max()) # 1, 6
#' 
#' letters %i% (contains("a") | contains("z")) # a, z
#' 
#' letters %i% perl("a|z") # a, z
#' 
#' letters %i% from("w")  # w, x, y, z
#' 
#' letters %i% to("c")  # a, b, c
#' 
#' letters %i% (from("b") & to("e"))  # b, d, e
#' 
#' c(1, 2, NA, 3) %i% not_na() # c(1, 2, 3)
#' 
#' # examples with count_if
#' df1 = data.frame(
#'     a=c("apples", "oranges", "peaches", "apples"),
#'     b = c(32, 54, 75, 86)
#' )
#' 
#' count_if(greater(55), df1$b) # greater than 55 = 2
#' 
#' count_if(not_equals(75), df1$b) # not equals 75 = 3
#' 
#' count_if(greater(32) & less(86), df1$b) # greater than 32 and less than 86 = 2
#' count_if(and(greater(32), less(86)), df1$b) # the same result
#' 
#' # infix version
#' count_if(35 %thru% 80, df1$b) # greater than or equals to 35 and less than or equals to 80 = 2
#' 
#' # values that started on 'a'
#' count_if(like("a*"), df1) # 2
#' 
#' # the same with Perl-style regular expression
#' count_if(perl("^a"), df1) # 2
#' 
#' # count_row_if
#' count_row_if(perl("^a"), df1) # c(1,0,0,1)
#' 
#' # examples with 'n_intersect' and 'n_diff'
#' data(iris)
#' iris %>% n_intersect(to("Petal.Width")) # all columns up to 'Species' 
#'  
#' # 'Sepal.Length', 'Sepal.Width' will be left 
#' iris %>% n_diff(from("Petal.Length"))
#' 
#' # except first column
#' iris %n_d% items(1)
#' 
#' # 'recode' examples
#' qvar = c(1:20, 97, NA, NA)
#' recode(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, 11 %thru% hi ~ 3, other ~ 0)
#' # the same result
#' recode(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, greater_or_equal(11) ~ 3, other ~ 0)
#' 
#' 
#' @name criteria
#' @export
as.criterion = function(crit){
    force(crit)
    if (is.function(crit)) {
        crit = match.fun(crit)
        res = function(x) {
            cond = crit(x)
            cond & !is.na(cond)
        }
    } else {
        if(is.list(crit) || is.data.frame(crit)){
            crit = c(crit, recursive = TRUE, use.names = FALSE)
        }
        if(is.list(crit)){
            # something we cannot convert to vector in the previous statement
            return(Reduce("|", lapply(crit, as.criterion)))
            
        } 
        if(is.logical(crit) && !(length(crit) == 1L && is.na(crit))){
            res = function(x) crit & !is.na(crit)
        } else {
            res = function(x) {
                if(inherits(x, "POSIXct") & !inherits(crit, "POSIXct")){
                    # because '%in%' doesn't coerce POSIXct in a sensible way 
                    x = as.character(x)
                } 
                if(inherits(x, "Date") & !inherits(crit, "Date")){
                    # because '%in%' doesn't coerce Date in a sensible way 
                    x = as.character(x)
                } 
                fast_in(x, crit)
            }
        }    
    }
    class(res) = union("criterion",class(res))
    res
}


#' @export
#' @rdname criteria
equals = function(x){
    force(x)
    as.criterion(function(y) {
        y == x
    })
    
}

#' @export
#' @rdname criteria
#' @usage NULL
eq = equals

#' @export
#' @rdname criteria
not_equals = function(x){
    force(x)
    as.criterion(function(y) {
        y != x
    })  
}

#' @export
#' @rdname criteria
#' @usage NULL
ne = not_equals

#' @export
#' @rdname criteria
#' @usage NULL
neq = not_equals

#' @export
#' @rdname criteria
less = function(x){
    build_compare(x,"<")    
}


#' @export
#' @rdname criteria
#' @usage NULL
lt = less


#' @export
#' @rdname criteria
less_or_equal = function(x){
    build_compare(x,"<=")    
}

#' @export
#' @rdname criteria
#' @usage NULL
le = less_or_equal 


#' @export
#' @rdname criteria
#' @usage NULL
lte = less_or_equal



#' @export
#' @rdname criteria
greater = function(x){
    build_compare(x,">")    
}

#' @export
#' @rdname criteria
#' @usage NULL
gt = greater

#' @export
#' @rdname criteria
greater_or_equal = function(x){
    build_compare(x,">=")       
}

#' @export
#' @rdname criteria
#' @usage NULL
ge = greater_or_equal

#' @export
#' @rdname criteria
#' @usage NULL
gte = greater_or_equal



#' @export
#' @rdname criteria
thru = function(lower, upper){
    !(is.function(lower) || is.function(upper)) || 
        stop("'thru' is not defined for functions but 'lower' = ", lower, " and 'upper' = ", upper)
    force(lower)
    force(upper)
    ge(lower) & le(upper)
}


#' @export
#' @rdname criteria
'%thru%' = function(lower, upper) thru(lower, upper)




#' @export
#' @rdname criteria
is_max = function(x){
    if(missing(x)){
        is_max
    } else {
        res = x == max_col(x)
        res & !is.na(res)
    } 
}
class(is_max) = union("criterion", class(is_max))

#' @export
#' @rdname criteria
is_min = function(x){
    if(missing(x)){
        is_min
    } else {
        res = x == min_col(x)
        res & !is.na(res)
    } 
}
class(is_min) = union("criterion", class(is_min))

#' @export
#' @rdname criteria
contains = function(pattern, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE){
    pattern
    ignore.case
    useBytes
    perl
    fixed
    as.criterion(function(x){
        grepl(pattern, x, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
    })
}

#' @export
#' @rdname criteria
like = function(pattern){
    pattern
    as.criterion(function(x){
        grepl(glob2rx(pattern), x, ignore.case = TRUE)
    })
}





#' @export
#' @rdname criteria
fixed = contains

#' @export
#' @rdname criteria
perl = contains
formals(perl)$fixed = FALSE
formals(perl)$perl = TRUE



#' @export
#' @rdname criteria
regex = contains
formals(regex)$fixed = FALSE


#' @export
#' @rdname criteria
has_label = function(x){
    if(!inherits(x, "criterion")){
        x = as.criterion(x)
    }
    as.criterion(function(y){
        values = n_intersect(val_lab(y), x)
        y %has% values
    })
}

#' @export
#' @rdname criteria
from = function(x){
    x
    as.criterion(function(y){
        first = match_col(x, y)
        positions = seq_along(y)
        positions>=first
        
    })

}

#' @export
#' @rdname criteria
to = function(x){
    x
    as.criterion(function(y){
        last = match_col(x, y)
        positions = seq_along(y)
        positions<=last
        
    })
}

#' @export
#' @rdname criteria
items = function(...){
    args = c(list(...), recursive = TRUE, use.names = FALSE)
    args = lapply(args, function(x) if(inherits(x, "criterion")) x else as.criterion(x))
    args = do.call(or, args)
    as.criterion(function(x){
        numbers = seq_along(x)    
        args(numbers)
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

class(not_na) = union("criterion", class(not_na))

#' @export
#' @rdname criteria
other = function(x){
    if(missing(x)){
        other    
    } else {
        rep(TRUE, NROW(x)) 
    }    
}


class(other) = union("criterion", class(other))




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

# to catch only numeric values
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
#' @rdname criteria
and = function(...){
    Reduce(`&`, list(...))
}

#' @export
#' @rdname criteria
or = function(...){
    Reduce(`|`, list(...))
}

#' @export
#' @rdname criteria
not = `!`

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










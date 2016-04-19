
#' Replace certain values with NA
#' 
#' @details 
#' There are following options for \code{value}: 
#' \itemize{ 
#' \item{vector/matrix/data.frame/list.}{ Vector of values which should be
#' replaced with \code{NA} in \code{x}. If value is matrix/vector/data.frame then for
#' each column of \code{x} will be used appropriate column of \code{value}.}
#' \item{logical vector/matrix/data.frame/list.}{ NA's will be set in places
#' where \code{value} is TRUE.}
#' \item{function.}{ NA's will be set in places where \code{value(x)} is TRUE.
#' Additionaly there are special functions for common cases of comparison with
#' value. For example \code{set_na(my_var, gt(98))} will replace all values greater
#' 98 in \code{my_var} with NA. If \code{x} has many columns(rows) and \code{value}
#' has one column(row) then it will be recycled. List of special functions see
#' below.}
#' }
#' Special functions for usage as criteria:
#' \itemize{
#' \item{\code{gt}}{ greater than}
#' \item{\code{gte}}{ greater than or equal}
#' \item{\code{eq}}{ equal} 
#' \item{\code{neq}}{ not equal} 
#' \item{\code{lt}}{ less than}
#' \item{\code{lte}}{ less than or equal}
#' } 
#' 
#' 
#' @param x vector/matrix/data.frame/list
#' @param value vector/matrix/data.frame/list/function
#'   
#' @return x with NA's instead of \code{value}
#' 
#' @seealso For reverse operation see \link{if_na}, \link{if_val} for more
#'   general recodings.
#'   
#' @examples
#' a = c(1:5,99)
#' set_na(a, 99)
#' set_na(a, gt(5)) # same result
#' 
#' set.seed(123)
#' dfs = data.frame(
#'       a = c("bad value", "bad value", "good value", "good value", "good value"),
#'       b = runif(5)
#' )
#' 
#' # rows with 'bad value' will be filled with NA
#' # logical argument and recycling by columns
#' set_na(dfs, dfs$a=="bad value")
#' 
#' a = rnorm(50)
#' # values greater than 1 or less than -1 will be set to NA
#' # special functions usage
#' set_na(a, lt(-1) | gt(1))
#' 
#' @export
set_na = function(x, value){
    if_val(x, from=list(value), to = list(NA))
}

#' @rdname set_na
#' @export
'set_na<-' = function(x, value){
    set_na(x, value)
}

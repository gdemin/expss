
#'Replace NA values in vector/data.frame/matrix/list with supplied value
#'
#'Function replaces NA values in vector/data.frame/matrix/list with supplied
#'value. If x is vector then \code{if_na(x) = 99} is equivalent to
#'\code{x[is.na(x)] = 99}. In more complex cases when x is
#'data.frame/matrix/list this function tries to replace NA recursively. If
#'replacement value is vector/data.frame/matrix/list then \code{if_na} uses for
#'replacement values from appropriate places. For example if both \code{x} and
#'\code{value} are vectors then \code{if_na(x) = value} is equivalent to
#'\code{x[is.na(x)] = value[is.na(x)]}. Single column/row value recycled to
#'conform to x. See examples. 
#'
#'@param x vector/matrix/data.frame/list
#'@param value vector/matrix/data.frame/list
#'  
#'@return x with replaced NA
#'  
#'@seealso For reverse operation see \link{na_if}.
#' @examples
#' # simple case
#' a = c(NA, 2, 3, 4, NA)
#' if_na(a, 99)
#' 
#' # the same result
#' a %if_na% 99
#' 
#' # the same result
#' if_na(a) = 99 
#' a # c(99, 2, 3, 4, 99)
#' 
#' 
#' # replacement with values from other variable
#' a = c(NA, 2, 3, 4, NA)
#' if_na(a) = 1:5
#' a # 1:5
#' 
#' # replacement with group means
#' 
#' # make data.frame 
#' set.seed(123)
#' group = sample(1:3, 30, replace = TRUE)
#' param = runif(30)
#' param[sample(30, 10)] = NA # place 10 NA's
#' df = data.frame(group, param)
#' 
#' # replace NA's with group means
#' df = compute(df, {
#'         if_na(param) = window_fun(param, group, mean_col)
#'     })
#' 
#' df
#' 
#' # replacement with column means
#' 
#' # make data.frame
#' set.seed(123)
#' x1 = runif(30)
#' x2 = runif(30)
#' x3 = runif(30)
#' x1[sample(30, 10)] = NA # place 10 NA's
#' x2[sample(30, 10)] = NA # place 10 NA's
#' x3[sample(30, 10)] = NA # place 10 NA's
#' 
#' df = data.frame(x1, x2, x3)
#' 
#' # replace NA's with column means
#' if_na(df) = t(mean_col(df))
#' 
#' df
#' 
#'@export
if_na = function(x, value){
    UseMethod("if_na")
}



#' @export
#' @rdname if_na
'if_na<-' = function(x, value){
    if_na(x, value)
}


#' @export
#' @rdname if_na
'%if_na%' = if_na

#' @export
if_na.default = function(x, value){
    check_conformance(x, value)
    if (anyNA(x)){
        if(!is.list(value) || is.data.frame(value)){
            nas = is.na(x)
            for (each_col in seq_len(NCOL(x))){
                cond = column(nas, each_col)
                if (any(cond)) column(x, each_col, cond) = column(value, each_col, cond)
            }
        } else {
            # if 'value' is list - we don't know what is inside element thats why we cannot subset it
            for (each_col in seq_len(NCOL(x))){
                if_na(column(x, each_col)) = column(value, each_col)
            }            
        }
    }
    x
}




#' @export
if_na.list = function(x, value){
    for(each in seq_along(x)){
        if_na(x[[each]]) = value
    }
    x
}





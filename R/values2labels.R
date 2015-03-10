#' Replace vector/matrix/data.frame/list values with corresponding value labels.
#' 
#' @param x vector/matrix/data.frame/list
#' @return Object of the same type as x but with value labels instead of values. If there are no labels for
#'  some values they are converted to characters in most cases.
#' @details \code{v2l} is just shortcut to \code{values2labels}.  
#' @seealso \code{\link{names2labels}}
#' @examples
#' data(mtcars)
#' mtcars = within(mtcars,{
#'                 var_lab(mpg) = NULL
#'                 val_lab(am) = c(" automatic" = 0, " manual" =  1)
#' })
#' 
#' summary(lm(mpg ~ ., data = values2labels(mtcars[,c("mpg","am")])))
#' @export
values2labels = function(x){
    UseMethod("values2labels")
}

#' @export
values2labels.default = function(x){
    vallab = val_lab(x)
    if(is.null(vallab)) return(x)
    res = names(vallab)[match(x,vallab,incomparables = NA)]
    not_na = !is.na(res)
    if(any(not_na)) x[not_na] =res[not_na]
    unvl(x)
    
}

#' @export
values2labels.data.frame = function(x){
    values2labels.list(x)
}


#' @export
values2labels.list = function(x){
    for (each in seq_along(x)){
        x[[each]] = values2labels(x[[each]])
    }
    x
}


#' @export
#' @rdname values2labels
v2l = values2labels





#'Replace NA values in vector/data.frame/matrix/list with supplied value
#'
#'In simple case of vector if_na(x) = 99 is simplified version of x[is.na(x)] = 
#'99. In more complex cases when x is data.frame/matrix/list this function tries
#'to replace NA recursively. If replacement value is 
#'vector/data.frame/matrix/list function use for replacement values from 
#'appropriate places, e. g. if both x and value are vectors if_na(x) = value is 
#'equivalent to x[is.na(x)] = value[is.na(x)]. Single column/row value recycled
#'to conform with x. See examples.
#'
#'@param x vector/matrix/data.frame/list
#'@param value vector/matrix/data.frame/list
#'  
#'@return x with replaced NA
#'  

#' @examples
#' # simple case
#' a = c(NA, 2, 3, 4, NA)
#' if_na(a) = 1
#' a # c(1, 2, 3, 4, 1)
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
#' df %<>% group_by(group) %>% 
#'     mutate(
#'         param = if_na(param, mean(param, na.rm = TRUE))
#'     )
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
#' # replaceme NA's with column means
#' if_na(df) = t(colMeans(df, na.rm = TRUE))
#' 
#' df
#' 
#'@export
if_na = function(x, value){
    UseMethod("if_na")
}

# TODO add labels possibility

#' @export
'if_na<-' = function(x, value){
    if_na(x, value)
}

#' @export
if_na.default = function(x, value){
    check_conformance(x, value)
    if(is.matrix(value)) value = value[,1]
    if(is.data.frame(value)) value = value[[1]]
    if (anyNA(x)){
        nas = is.na(x)
        if(NROW(value)>1){
            x[nas] = value[nas]
        } else {
            x[nas] = value
        }
    }
    x
}

#' @export
if_na.data.frame = function(x, value){
    check_conformance(x, value)
    if(NCOL(value)>1) {
        if(is.matrix(value)){
            for(each in seq_along(x)){
                if_na(x[[each]]) = value[,each]
            }
        } else {
            for(each in seq_along(x)){
                if_na(x[[each]]) = value[[each]]
            }    
        }
    } else {
        for(each in seq_along(x)){
            if_na(x[[each]]) = value
        } 
    }
    x
}

#' @export
if_na.matrix = function(x, value){
    check_conformance(x, value)
    if(anyNA(x)){
        nas = is.na(x)
        if(NCOL(value)>1 && NROW(value)>1) {
            value = as.matrix(value)
            x[nas] = value[nas]
        }
        if(NCOL(value)<2 && NROW(value)<2) {
            x[nas] = value
        }
        if(NCOL(value)<2 && NROW(value)>1) {
            for(each in seq_len(ncol(x))){
                x[nas[,each],each] = value[nas[,each]]
            }
        }  
        if(NCOL(value)>1 && NROW(value)<2) {
            for(each in seq_len(ncol(x))){
                x[nas[,each],each] = value[,each]
            }
        } 
    }
    x
}

#' @export
if_na.list = function(x, value){
    if(!is.list(value) || is.data.frame(value)){
    for(each in seq_along(x)){
        if_na(x[[each]]) = value
    }
    } else {
        check_conformance(x,value)
        for(each in seq_along(x)){
            if_na(x[[each]]) = value[[each]]
        }    
    }
    x
}





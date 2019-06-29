#' Replace values with NA and vice-versa
#' 
#' \itemize{
#' \item{\code{if_na}}{ replaces NA values in vector/data.frame/matrix/list with
#' supplied value. For single value argument label can be provided with
#' \code{label} argument. If replacement value is vector then \code{if_na} uses
#' for replacement values from appropriate positions.  An opposite operation is \code{na_if}.}
#' \item{\code{na_if}}{ replaces values with NA in
#' vector/data.frame/matrix/list. Another alias for this is \code{mis_val}.}
#' \item{\code{valid}}{ returns logical vector which indicate the presence of at
#' least one not-NA value in row.  For vector or single column data.frame result
#' is the same as with \link[stats]{complete.cases}. There is a special case
#' for data.frame of class \code{dichotomy}. In this case result indicate the
#' presence of at least one 1 in a row.} }
#'
#' @param x vector/matrix/data.frame/list
#' @param value single value, vector of the same length as number of rows in
#'   \code{x}, or function (\link{criteria}) for \code{na_if}. See \link{recode}
#'   for details.
#' @param label a character of length 1. Label for \code{value} which replace NA.
#' @param with_labels logical. FALSE by default. Should we also remove labels of
#'   values which we recode to NA?
#'
#'  
#' @return object of the same form and class as \code{x}. \code{valid} returns logical vector.
#'  
#' @examples
#' # simple case
#' a = c(NA, 2, 3, 4, NA)
#' if_na(a, 99)
#' 
#' # the same result
#' a %if_na% 99
#' 
#' # with label
#' a = c(NA, 2, 3, 4, NA)
#' if_na(a, 99, label = "Hard to say")
#' 
#' # in-place replacement. The same result:
#' if_na(a, label = "Hard to say") = 99 
#' a # c(99, 2, 3, 4, 99)
#' 
#' # replacement with values from other variable
#' a = c(NA, 2, 3, 4, NA)
#' b = 1:5
#' if_na(a, b)
#' 
#' # replacement with group means
#' # make data.frame 
#' set.seed(123)
#' group = sample(1:3, 30, replace = TRUE)
#' param = runif(30)
#' param[sample(30, 10)] = NA # place 10 NA's
#' df = data.frame(group, param)
#' 
#' # replace NA's with group means
#' if_na(df$param) = window_fun(df$param, df$group, mean_col)
#' df
#' 
#' ######################
#' ### na_if examples ###
#' ######################
#' 
#' a = c(1:5, 99)
#' # 99 to NA
#' na_if(a, 99)    # c(1:5, NA)
#' 
#' a %na_if% 99    # same result
#' 
#' # values which greater than 4 to NA
#' na_if(a, gt(4)) # c(1:4, NA, NA)
#' 
#' # alias 'mis_val', with_labels = TRUE
#' a = c(1, 1, 2, 2, 99)
#' val_lab(a) = c(Yes = 1, No = 2, "Hard to say" = 99)
#' mis_val(a, 99, with_labels = TRUE)
#' 
#' set.seed(123)
#' dfs = data.frame(
#'       a = c("bad value", "bad value", "good value", "good value", "good value"),
#'       b = runif(5)
#' )
#' 
#' # rows with 'bad value' will be filled with NA
#' # logical argument and recycling by columns
#' na_if(dfs, dfs$a=="bad value")
#' 
#' a = rnorm(50)
#' # values greater than 1 or less than -1 will be set to NA
#' # special functions usage
#' na_if(a, lt(-1) | gt(1))
#' 
#' # values inside [-1, 1] to NA
#' na_if(a, -1 %thru% 1)
#'@export
if_na = function(x, value, label = NULL){
    stopifnot(length(label)<2)
    if (length(label) == 0 || label == ""){ 
        recode(x) = c(NA, NaN) ~ value    
    } else {
        args = list(c(NA, NaN) ~ value)
        names(args) = label
        recode(x) = args
    }
    x
}



#' @export
#' @rdname if_na
'if_na<-' = function(x, label = NULL, value){
    fix_datatable(if_na(x, value, label = label))
}


#' @export
#' @rdname if_na
'%if_na%' = function(x, value) if_na(x, value)

#############
#' @export
#' @rdname if_na
na_if = function(x, value, with_labels = FALSE){
    if(!length(value)) return(x)
    recode(x, with_labels = with_labels) = value ~ NA
    x
}

#' @rdname if_na
#' @export
'na_if<-' = function(x, with_labels = FALSE, value){
    fix_datatable(na_if(x, value, with_labels = with_labels))
}

#' @rdname if_na
#' @export
'%na_if%' = function(x, value) na_if(x, value)


#' @rdname if_na
#' @export
mis_val = na_if

#' @rdname if_na
#' @export
'mis_val<-' = `na_if<-`

#' @rdname if_na
#' @export
'%mis_val%' = '%na_if%'



#########################################
#' @export
#' @rdname if_na
valid = function(x){
    UseMethod("valid")
}

#' @export
valid.default = function(x){
    !is.na(x)
}

#' @export
valid.data.frame = function(x){
    if (length(x)) {
        res = do.call(cbind, lapply(x, is.na))
    } else {
        res = matrix(FALSE, NROW(x), 0)
    }    
    !rowAlls(res)
}


#' @export
valid.dichotomy = function(x){
    if (length(x)) {
        res = do.call(cbind, lapply(x, function(x) x==1))
    } else {
        res = matrix(FALSE, NROW(x), 0)
    }    
    rowAnys(res, na.rm = TRUE)
}

#' @export
valid.matrix = function(x){
    !rowAlls(is.na(x))
}

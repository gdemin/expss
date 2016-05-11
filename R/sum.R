#' Sum/mean/sd/median/max/min on row/column 
#' 
#' There are two flavors of this function - one works with entire dataset/matrix/vector
#' similar to Microsoft Excel \code{COUNTIF}. The second works rowwise - e. g. 
#' similar to SPSS \code{COUNT} function. 
#'  
#' @param criterion Vector with counted values, list with conditions or
#'   function. If criterion is missing (or is NULL) non-NA's values will be
#'   counted.
#' @param ... data. Vectors, matrixes, data.frames, list. Shorter arguments
#'   will be recycled.
#' @param x Counted values or criterion for counting. Vector, matrix, data.frame,
#'   list, function. Shorter columns in list will be recycled.
#' 
#' @return 
#' \code{count} return single value (vector of length 1). 
#' \code{count_row} returns vector of counts for each row of supplied arguments.
#' \code{count_col} returns vector of counts for each column of supplied arguments.
#' \code{\%has\%} returns logical vector - presence indicator of criterion in each row.
#' 
#' @details
#' \code{count} counts values in entire dataset and return single 
#' value (vector of length 1).
#' 
#' \code{count_row} counts values in each row of supplied arguments and return
#' vector of counts for each row of supplied arguments.
#' 
#' \code{count_col} counts values in each column of supplied arguments and return
#' vector of counts for each column of supplied arguments.
#' 
#' All functions never return NA's. 
#' 
#' If criterion is list, then each element considered as single condition and
#' they will be combined with AND.
#' 
#' Function criterion should return logicals of same size and shape as its argument.
#' This function will be applied to each column of supplied data and TRUE results will be counted.
#' There is asymmetrical behavior in \code{count_row} and
#' \code{count_col}: in both cases function criterion will be applied
#' columnwise.
#' There are special functions for usage as criteria (e. g. \code{gt(5)} is
#' equivalent ">5" in spreadsheet):
#' \itemize{
#' \item{\code{gt}}{ greater than}
#' \item{\code{gte}}{ greater than or equal}
#' \item{\code{eq}}{ equal} 
#' \item{\code{neq}}{ not equal} 
#' \item{\code{lt}}{ less than}
#' \item{\code{lte}}{ less than or equal}
#' } 
#' 
#' @export
#' @examples
#' # Examples borrowed from Microsoft Excel help for COUNTIF
#' df1 = data.frame(
#'     a=c("apples",   "oranges",     "peaches",     "apples"),
#'     b = c(32, 54, 75, 86)
#' )
#' 
#' count("apples",df1$a) # 2
#' 
#' count("apples",df1) # 2
#' 
#' with(df1,count("apples",a,b)) # 2
#' 
#' count(gt(55),df1$b) # greater than 55 = 2
#' 
#' count(neq(75),df1$b) # not equal 75 = 3
#' 
#' count(gte(32),df1$b) # greater than or equal 32 = 4
#' 
#' count(list(gt(32), lt(86)),df1$b) # 2
#' 
#' count(gt(32) & lt(86),df1$b) # 2
#' 
#' count(33:85,df1$b) # 2
#' 
#' # more complex criteria
#' # values with letters
#' count(function(x) grepl("^[A-z]+$",x),df1) # 4
#' 
#' # values that started on 'a'
#' count(function(x) grepl("^a",x),df1) # 2
#' 
#' # count_row
#' count_row(function(x) grepl("^a",x),df1) # c(1,0,0,1)
#' 
#' df1 %has% 'apples' # c(TRUE,FALSE,FALSE,TRUE)
#' 
#' # example with dplyr
#' if (require(dplyr)){
#'  set.seed(123)
#'  df2 = as.data.frame(
#'         matrix(sample(c(1:10,NA),30,replace = TRUE),10)
#'  )
#'  df2  %>% mutate(exact = count_row(8, V1, V2, V3),
#'                     greater = count_row(gt(8), V1, V2, V3),
#'                     range = count_row(5:8, V1, V2, V3),
#'                     na = count_row(is.na, V1, V2, V3),
#'                     not_na = count_row(, V1, V2, V3)
#'                  ) -> result
#'  result
#' }
#' @export
sum_row=function(...){
    data = dots2data_frame(...)
    rowSums(data, na.rm=TRUE)
}


#' @export
#' @rdname sum_row
sum_col =function(...){
    data = dots2data_frame(...)
    colSums(data, na.rm=TRUE)
}


################################################

#' @export
mean.data.frame = function(x, ...) mean(as.matrix(x), ...)

#' @export
#' @rdname sum_row
mean_row=function(...){
    data = dots2data_frame(...)
    rowMeans(data, na.rm=TRUE)
}


#' @export
#' @rdname sum_row
mean_col=function(...){
    data = dots2data_frame(...)
    colMeans(data, na.rm=TRUE)
}


################################################



#' @export
#' @rdname sum_row
sd_row=function(...){
    data = dots2data_frame(...)
    apply(data, 1, sd, na.rm=TRUE)
}


#' @export
#' @rdname sum_row
sd_col=function(...){
    data = dots2data_frame(...)
    apply(data, 2, sd, na.rm=TRUE)
}

################################################

#' @export
median.data.frame = function(x, ...) median(as.matrix(x), ...)

#' @export
#' @rdname sum_row
median_row=function(...){
    data = dots2data_frame(...)
    apply(data, 1, median, na.rm=TRUE)
}


#' @export
#' @rdname sum_row
median_col=function(...){
    data = dots2data_frame(...)
    apply(data, 2, median, na.rm=TRUE)
}


###################################################

#' @export
#' @rdname sum_row
max_row=function(...){
    data = dots2data_frame(...)
    res = suppressWarnings(do.call(pmax, c(data, na.rm=TRUE)))
    res[!is.finite(res)] = NA
    res
}


#' @export
#' @rdname sum_row
max_col=function(...){
    data = dots2data_frame(...)
    res = suppressWarnings(apply(data, 2, max, na.rm=TRUE))
    res[!is.finite(res)] = NA
    res
}

##########################################################


#' @export
#' @rdname sum_row
min_row=function(...){
    data = dots2data_frame(...)
    res = suppressWarnings(do.call(pmin, c(data, na.rm=TRUE)))
    res[!is.finite(res)] = NA
    res
}


#' @export
#' @rdname sum_row
min_col=function(...){
    data = dots2data_frame(...)
    res = suppressWarnings(apply(data, 2, min, na.rm=TRUE))
    res[!is.finite(res)] = NA
    res
}


#########################################################

#' @export
#' @rdname sum_row
apply_row = function(fun, ...){
    data = dots2data_frame(...)   
    apply(data, 1, fun)
}

#' @export
#' @rdname sum_row
apply_col = function(fun, ...){
    data = dots2data_frame(...)  
    apply(data, 2, fun)
}


dots2list = function(...){
    values = as.character(substitute(c(...))[-1])
    args = list(...)
    curr_names = names(args)
    has_names = sapply(args, function(x) is.data.frame(x) || is.matrix(x) || is.list(x))
    if (is.null(curr_names)) {
        curr_names = values
        curr_names[has_names] = ""
    } else {
        
        has_names = (!is.na(curr_names) && curr_names != "") || has_names
        curr_names[!has_names] = values[!has_names]
    }
    names(args) = curr_names
    args
    
}

dots2data_frame = function(...){
    args = dots2list(...)
    zero_length = lengths(args)==0
    args[zero_length] = NA
    do.call(data.frame,c(args,stringsAsFactors=FALSE)) 
    
}
    




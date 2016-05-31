#' Compute sum/mean/sd/median/max/min/custom function on rows/columns 
#' 
#' This are convenience functions for usage inside \link{modify},
#' \link{modify_if}, \link[base]{with}, \link[base]{within} and \code{dplyr}
#' \code{mutate} functions.
#' 
#' @param ... data. Vectors, matrixes, data.frames, list. Shorter arguments
#'   will be recycled.
#'   
#' @param fun custom function that will be applied to \dots
#' 
#' @return All functions except \code{apply_*} return numeric vector of length 
#'   equals the number of argument columns/rows. Value of \code{apply_*} depends
#'   on supplied \code{fun} function.
#' 
#' @details sum/mean/sd/median/max/min always omits NA. \code{sum_*} behavior is
#'   different from base \code{sum} and similar to SPSS \code{sum} behavior. It
#'   always omits NA but result of column/row with all NA gives NA (instead of
#'   0).
#' 
#' @seealso \link{modify}, \link{modify_if}, \link{\%to\%}, \link{count_if},
#'   \link{sum_if}, \link{mean_if}, \link{median_if}, \link{sd_if},
#'   \link{min_if}, \link{max_if}
#' 
#' @export
#' @examples
#' ## Inside example
#' iris = modify(iris,{
#'   new_median = median_row(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#'   new_mean = mean_row(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#'   })
#'   
#' dfs = data.frame(
#'     test = 1:5,
#'     aa = rep(10, 5),
#'     b_ = rep(20, 5),
#'     b_1 = rep(11, 5),
#'     b_2 = rep(12, 5),
#'     b_4 = rep(14, 5),
#'     b_5 = rep(15, 5) 
#' )
#' 
#' # calculate sum of b* variables
#' modify(dfs, {
#'     b_total = sum_row(b_, b_1 %to% b_5)
#' })
#' 
#' # conditional modification
#' modify_if(dfs, test %in% 2:4, {
#'     b_total = sum_row(b_, b_1 %to% b_5)
#' })
#' 
#' # Examples from rowSums/colSums manual.
#' ## Compute row and column sums for a matrix:
#' x = cbind(x1 = 3, x2 = c(4:1, 2:5))
#' sum_row(x); sum_col(x)
#' dimnames(x)[[1]] <- letters[1:8]
#' sum_row(x); sum_col(x); mean_row(x); mean_col(x)
#' 
#' @export
sum_row=function(...){
    data = dots2data_frame(...)
    nas = is.na(rowMeans(data, na.rm=TRUE))
    res = rowSums(data, na.rm=TRUE)
    res[nas] = NA
    res
}


#' @export
#' @rdname sum_row
sum_col =function(...){
    data = dots2data_frame(...)
    nas = is.na(colMeans(data, na.rm=TRUE))
    res = colSums(data, na.rm=TRUE)
    res[nas] = NA
    res
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
    




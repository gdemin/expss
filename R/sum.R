#' Compute sum/mean/sd/median/max/min/custom function on rows/columns 
#' 
#' This are convenience functions for usage inside \code{with}, \code{within}
#' and \code{dplyr} \code{mutate} functions.
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
#' @seealso \link{count_if}, \link{sum_if}, \link{mean_if}, \link{median_if},
#'   \link{sd_if}, \link{min_if}, \link{max_if}
#' 
#' @export
#' @examples
#' # Examples from rowSums/colSums manual.
#' ## Compute row and column sums for a matrix:
#' x = cbind(x1 = 3, x2 = c(4:1, 2:5))
#' sum_row(x); sum_col(x)
#' dimnames(x)[[1]] <- letters[1:8]
#' sum_row(x); sum_col(x); mean_row(x); mean_col(x)
#' 
#' ## Inside within example
#' iris = within(iris,{
#'   new_median = median_row(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#'   new_mean = mean_row(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#'   })
#' 
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
    




#' Compute sum/mean/sd/median/max/min/custom function on rows/columns 
#' 
#' These functions are intended for usage inside \link{compute}, and
#' \link{do_if}. sum/mean/sd/median/max/min by default omits NA. \code{any_in_*}
#' checks existence of any TRUE in each row/column. It is equivalent of
#' \link[base]{any} applied to each row/column. \code{all_in_*} is equivalent of
#' \link[base]{all} applied to each row/column.
#' 
#' @param ... data. Vectors, matrixes, data.frames, list. Shorter arguments
#'   will be recycled.
#'  
#' @param na.rm logical. Contrary to the base 'sum' it is TRUE by default. Should missing values (including NaN)
#'   be removed?
#'   
#' @param fun custom function that will be applied to \dots
#' 
#' @return All functions except \code{apply_*} return numeric vector of length 
#'   equals the number of argument columns/rows. Value of \code{apply_*} depends
#'   on supplied \code{fun} function.
#' 
#' @seealso \link{compute}, \link{do_if}, \link{\%to\%}, \link{count_if},
#'   \link{sum_if}, \link{mean_if}, \link{median_if}, \link{sd_if},
#'   \link{min_if}, \link{max_if}
#' 
#' @export
#' @examples
#' iris = compute(iris, {
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
#' compute(dfs, {
#'     b_total = sum_row(b_, b_1 %to% b_5)
#' })
#' 
#' # conditional modification
#' do_if(dfs, test %in% 2:4, {
#'     b_total = sum_row(b_, b_1 %to% b_5)
#' })
#' 

#' 
#' @export
sum_row=function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    matrixStats::rowSums2(data, na.rm = na.rm)

}


#' @export
#' @rdname sum_row
sum_col =function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    res = matrixStats::colSums2(data, na.rm = na.rm)
    names(res) = colnames(data)
    res
}


################################################

#' @export
mean.data.frame = function(x, ...) mean(unlist(x, use.names = FALSE, recursive = TRUE), ...)

#' @export
#' @rdname sum_row
mean_row=function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    matrixStats::rowMeans2(data, na.rm = na.rm)
}


#' @export
#' @rdname sum_row
mean_col=function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    res = matrixStats::colMeans2(data, na.rm = na.rm)
    names(res) = colnames(data)
    res
}


################################################



#' @export
#' @rdname sum_row
sd_row=function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    matrixStats::rowSds(data, na.rm = na.rm)
}


#' @export
#' @rdname sum_row
sd_col=function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    res = matrixStats::colSds(data, na.rm = na.rm)
    names(res) = colnames(data)
    res
}

################################################

# @export
median.data.frame = function(x, ...) stats::median(as.matrix(x), ...)

#' @export
#' @rdname sum_row
median_row=function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    matrixStats::rowMedians(data, na.rm = na.rm)
}


#' @export
#' @rdname sum_row
median_col=function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    res = matrixStats::colMedians(data, na.rm = na.rm)
    names(res) = colnames(data)
    res
}


###################################################

#' @export
#' @rdname sum_row
max_row=function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    res = matrixStats::rowMaxs(data, na.rm = na.rm)
    res[!is.finite(res)] = NA
    res
}


#' @export
#' @rdname sum_row
max_col=function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    res = matrixStats::colMaxs(data, na.rm = na.rm)
    res[!is.finite(res)] = NA
    names(res) = colnames(data)
    res
}

##########################################################


#' @export
#' @rdname sum_row
min_row=function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    res = matrixStats::rowMins(data, na.rm = na.rm)
    res[!is.finite(res)] = NA
    res
}


#' @export
#' @rdname sum_row
min_col=function(..., na.rm = TRUE){
    data = dots2matrix(..., logical_as_numeric = TRUE)
    res = matrixStats::colMins(data, na.rm = na.rm)
    res[!is.finite(res)] = NA
    names(res) = colnames(data)
    res
}


#########################################################

#' @export
#' @rdname sum_row
apply_row = function(fun, ...){
    data = dots2matrix(...)
    fun = match.fun(fun)
    rows = seq_len(nrow(data))
    res = lapply(rows, function(each_row){
        fun(data[each_row, ])
    })
    if(any(lengths(res) != 1)){
        stop("'apply_col': incorrect result - function returns values with length greater than one.")    
    }
    unlist(res, use.names = FALSE, recursive = TRUE)
}

#' @export
#' @rdname sum_row
apply_col = function(fun, ...){
    data = dots2matrix(...)
    fun = match.fun(fun)
    cols = seq_len(ncol(data))
    res = lapply(cols, function(each_col){
        fun(data[, each_col])
    })
    if(any(lengths(res) != 1)){
        stop("'apply_col': incorrect result - function returns values with length greater than one.")    
    }
    res = unlist(res, use.names = FALSE, recursive = TRUE)
    names(res) = colnames(data)
    res
}


dots2matrix = function(..., logical_as_numeric = FALSE, as_logical = FALSE){
    res = flat_list(
        list(...), 
        flat_df = FALSE
    )
    res = do.call(cbind, res)
    if(!is.matrix(res)) res = as.matrix(res)
    if(logical_as_numeric && is.logical(res)) storage.mode(res) = "integer"
    if(as_logical && !is.logical(res)) storage.mode(res) = "logical"
    res
}


    

#################
#' @export
#' @rdname sum_row
any_in_row =function(..., na.rm = TRUE){
    data = dots2matrix(..., as_logical = TRUE)
    matrixStats::rowAnys(data, na.rm = na.rm)
}

#' @export
#' @rdname sum_row
any_in_col =function(..., na.rm = TRUE){
    data = dots2matrix(..., as_logical = TRUE)
    res = matrixStats::colAnys(data, na.rm = na.rm)
    names(res) = colnames(data)
    res
}

#' @export
#' @rdname sum_row
all_in_row =function(..., na.rm = TRUE){
    data = dots2matrix(..., as_logical = TRUE)
    matrixStats::rowAlls(data, na.rm = na.rm)
}

#' @export
#' @rdname sum_row
all_in_col =function(..., na.rm = TRUE){
    data = dots2matrix(..., as_logical = TRUE)
    res = matrixStats::colAlls(data, na.rm = na.rm)
    names(res) = colnames(data)
    res
}

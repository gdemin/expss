#' Compute various weighted statistics
#'
#' \itemize{
#' \item{{\code{w_mean}}{ weighted mean of a numeric vector}}
#' \item{{\code{w_sd}}{ weighted sample standard deviation of a numeric vector}}
#' \item{{\code{w_var}}{ weighted sample variance of a numeric vector}}
#' \item{{\code{w_se}}{ weighted standard error of a numeric vector}}
#' \item{{\code{w_median}}{ weighted median of a numeric vector}}
#' \item{{\code{w_mad}}{ weighted mean absolute deviation from median of a numeric vector}}
#' \item{{\code{w_sum}}{ weighted sum of a numeric vector}}
#' \item{{\code{w_n}}{ weighted number of values of a numeric vector}}
#' \item{{\code{w_cov}}{ weighted covariance matrix of a numeric matrix/data.frame}}
#' \item{{\code{w_cor}}{ weighted Pearson correlation matrix of a numeric matrix/data.frame}}
#' \item{{\code{w_pearson}}{ shortcut for \code{w_cor}. Weighted Pearson
#' correlation matrix of a numeric matrix/data.frame}}
#' \item{{\code{w_spearman}}{ weighted Spearman correlation matrix of a numeric matrix/data.frame}}
#' }
#'
#' @details
#' If argument of correlation functions is data.frame with variable labels then
#' variables names will be replaced with labels. If this is undesirable behavior
#' use \link{unvr} function: \code{w_cor(unvr(x))}. Weighted spearman
#' correlation coefficients are calculated with rounded to nearest integer
#' weights. It gives the same result as in SPSS Statistics software. By now this
#' algorithm is not memory efficient.
#'
#' @param x a numeric vector (matrix/data.frame for correlations) containing the
#'   values whose weighted statistics is to be computed.
#' @param weight a vector of weights to use for each element of x. Cases with
#'   missing, zero or negative weights will be removed before calculations. If
#'   \code{weight} is missing then unweighted statistics will be computed.
#' @param na.rm a logical value indicating whether NA values should be stripped
#'   before the computation proceeds. Note that contrary to base R statistic
#'   functions the default value is TRUE (remove missing values).
#' @param use \code{"pairwise.complete.obs"} (default) or \code{"complete.obs"}.
#'   In the first case  the correlation or covariance between each pair of
#'   variables is computed using all complete pairs of observations on those
#'   variables. If \code{use} is \code{"complete.obs"} then missing values are
#'   handled by casewise deletion.
#' @return a numeric value of length one/correlation matrix
#' @export
#'
#' @examples
#' data(mtcars)
#' dfs = mtcars %>% keep(mpg, disp, hp, wt)
#'
#' with(dfs, w_mean(hp, weight = 1/wt))
#'
#' # apply labels
#' dfs = modify(dfs, {
#'          var_lab(mpg) = "Miles/(US) gallon"
#'          var_lab(disp) = "Displacement (cu.in.)"
#'          var_lab(hp) = "Gross horsepower"
#'          var_lab(wt) = "Weight (1000 lbs)"
#' })
#'
#' # weighted correlations with labels
#' w_cor(dfs, weight = 1/dfs$wt)
#'
#' # without labels
#' w_cor(unvr(dfs), weight = 1/dfs$wt)
w_mean = function(x, weight = NULL, na.rm = TRUE){
    internal_w_stat(x = x,  weight = weight, na.rm = na.rm, fun = matrixStats::weightedMean)
}

#' @export
#' @rdname w_mean
w_median = function(x, weight = NULL, na.rm = TRUE){
    internal_w_stat(x = x,  weight = weight, na.rm = na.rm, fun = function(x, w, na.rm){
        matrixStats::weightedMedian(x = x, w = w, na.rm = na.rm, interpolate = TRUE, ties = "weighted")
    })
}

#' @export
#' @rdname w_mean
w_var = function(x, weight = NULL, na.rm = TRUE){
    internal_w_stat(x = x,  weight = weight, na.rm = na.rm, check_weight_sum = TRUE, fun = matrixStats::weightedVar)
}

#' @export
#' @rdname w_mean
w_sd = function(x, weight = NULL, na.rm = TRUE){

    internal_w_stat(x = x,  weight = weight, na.rm = na.rm, check_weight_sum = TRUE, fun = matrixStats::weightedSd)
}


#' @export
#' @rdname w_mean
w_se = function(x, weight = NULL, na.rm = TRUE){
    internal_w_stat(x = x,  weight = weight, na.rm = na.rm, check_weight_sum = TRUE, fun = function(x, w, na.rm){
        if(is.null(w)){
            if(na.rm) x = x[!is.na(x)]
            matrixStats::weightedSd(x, w = w, na.rm = na.rm)/sqrt(length(x))
        } else {
            if(na.rm) {
                w = w[!is.na(x)]
                x = x[!is.na(x)]
            }
            matrixStats::weightedSd(x, w = w, na.rm = na.rm)/sqrt(sum(w, na.rm = TRUE))
        }
    })
}

#' @export
#' @rdname w_mean
w_mad = function(x, weight = NULL, na.rm = TRUE){
    internal_w_stat(x = x,  weight = weight, na.rm = na.rm, fun = function(x, w, na.rm){
        matrixStats::weightedMad(x = x, w = w, na.rm = na.rm,
                                 center = w_median(x = x, weight = w, na.rm = na.rm)
        )
    })
}

#' @export
#' @rdname w_mean
w_sum = function(x, weight = NULL, na.rm = TRUE){
    internal_w_stat(x = x,  weight = weight, na.rm = na.rm, fun = function(x, w, na.rm){
        if(is.null(w)) sum(x, na.rm = na.rm) else sum(x*w, na.rm = na.rm)
    })
}

#' @export
#' @rdname w_mean
w_n = function(x, weight = NULL, na.rm = TRUE){
    internal_w_stat(x = x,  weight = weight, na.rm = na.rm, fun = function(x, w, na.rm){
        if(is.null(w)){
            if(na.rm){
                sum(!is.na(x))
            } else {
                length(x)
            }
        }  else {
            if(na.rm){
                sum(w[!is.na(x)])
            } else {
                sum(w)
            }

        }
    })
}

#' @export
#' @rdname w_mean
unweighted_valid_n = function(x, weight = NULL) {
    res = valid(x)
    if(is.null(weight)){
        sum(res, na.rm = TRUE)    
    } else {
        weight = set_negative_and_na_to_zero(weight)
        res = recycle_if_single_row(res, NROW(weight))
        weight = recycle_if_single_row(weight, NROW(res))
        sum(res[weight>0], na.rm = TRUE)
    }
}

#' @export
#' @rdname w_mean
valid_n = function(x, weight = NULL) {
    res = valid(x)
    if(is.null(weight)){
        sum(res, na.rm = TRUE)
    } else {
        weight = set_negative_and_na_to_zero(weight)
        res = recycle_if_single_row(res, NROW(weight))
        weight = recycle_if_single_row(weight, NROW(res))
        sum(weight[res], na.rm = TRUE)
    }
}

#' @export
#' @rdname w_mean
w_max = function(x, weight = NULL, na.rm = TRUE){
    internal_w_stat(x = x,  weight = weight, na.rm = na.rm, fun = function(x, w, na.rm){
        res = max(x, na.rm = TRUE)
        res[!is.finite(res)] = NA_real_
        res
    })
}

#' @export
#' @rdname w_mean
w_min = function(x, weight = NULL, na.rm = TRUE){
    internal_w_stat(x = x,  weight = weight, na.rm = na.rm, fun = function(x, w, na.rm){
        res = min(x, na.rm = TRUE)
        res[!is.finite(res)] = NA_real_
        res
    })
}

#' @export
#' @rdname w_mean
w_cov = function(x, weight = NULL, use = c("pairwise.complete.obs", "complete.obs")){
    if (is.data.frame(x)) {
        x = as.matrix(names2labels(x))
    } else {
        stopif(!is.matrix(x), "'x' must be a matrix or a data frame")
    }
    if(NROW(x)==0){
        return(matrix_of_na(x))
    }
    use = match.arg(use)
    if(!is.null(weight)){
        if(is.logical(weight)) weight = as.numeric(weight)
        if(length(weight) == 1L){
            weight = rep(weight, nrow(x))
        }
        stopif(length(weight) != nrow(x), "length of 'weight' must equal the number of rows in 'x'")
        positive_weight = (!is.na(weight)) & (weight>0)
        x = x[positive_weight, , drop = FALSE]
        weight = weight[positive_weight]
        if(anyNA(x)){
            if(use == "complete.obs"){
                completes = stats::complete.cases(x)
                weight = weight[completes]
                x = x[completes, ,drop = FALSE]
                internal_w_cov(x = x, weight = weight)
            } else {
                seq_ncol_x = seq_len(ncol(x))
                res = matrix(NA, ncol = ncol(x), nrow = ncol(x))
                for(i in seq_ncol_x){
                    for(j in seq_ncol_x[seq_ncol_x>=i]){
                        two_col = x[, c(i, j)]
                        complete_pair = stats::complete.cases(two_col)
                        curr_cov = internal_w_cov(two_col[complete_pair,], weight = weight[complete_pair])[1,2]
                        res[i,j] = curr_cov
                        if(i!=j){
                             res[j,i] = curr_cov
                        }
                    }
                }
                colnames(res) = colnames(x)
                rownames(res) = colnames(x)
                res
           }
        } else {
            internal_w_cov(x = x, weight = weight)
        }

    } else {
        suppressWarnings(stats::cov(x = x, use = use, method = "pearson"))
    }
}

#' @export
#' @rdname w_mean
w_cor = function(x, weight = NULL, use = c("pairwise.complete.obs", "complete.obs")){
    use = match.arg(use)
    if (is.data.frame(x)) {
        x = as.matrix(names2labels(x))
    } else {
        stopif(!is.matrix(x), "'x' must be a matrix or a data frame")
    }
    if(NROW(x)==0){
        return(matrix_of_na(x))
    }
    if(!is.null(weight)){
        if(is.logical(weight)) weight = as.numeric(weight)
        if(length(weight) == 1L){
            weight = rep(weight, nrow(x))
        }
        cov_mat = w_cov(x = x, weight = weight, use = use)
        if((use == "complete.obs") || !anyNA(x)){
            if(!all(is.na(cov_mat))) {
                res = suppressWarnings(stats::cov2cor(cov_mat))
            } else {
                res = cov_mat
            }
        } else {
            seq_ncol_x = seq_len(ncol(x))
            sds = internal_pairwise_sd(x = x, weight = weight)
            for(i in seq_ncol_x){
                for(j in seq_ncol_x){
                    cov_mat[i,j] = cov_mat[i,j]/sds[i,j]/sds[j,i]
                }
            }
            res = cov_mat
        }
    } else {
        res = suppressWarnings(stats::cor(x = x, use = use, method = "pearson"))
    }
    diag(res) = 1
    res
}


#' @export
#' @rdname w_mean
w_pearson = w_cor

#' @export
#' @rdname w_mean
w_spearman = function(x, weight = NULL, use = c("pairwise.complete.obs", "complete.obs")){
    use = match.arg(use)
    if (is.data.frame(x)) {
        x = as.matrix(names2labels(x))
    } else {
        stopif(!is.matrix(x), "'x' must be a matrix or a data frame")
    }
    if(NROW(x)==0){
        return(matrix_of_na(x))
    }
    if(!is.null(weight)){
        if(is.logical(weight)) weight = as.numeric(weight)
        if(length(weight) == 1L){
            weight = rep(weight, nrow(x))
        }
        posititive_weight =  (!is.na(weight)) & (weight>0)
        x = x[posititive_weight, , drop = FALSE]
        weight = trunc(weight[posititive_weight]+0.5)
        if(sum(weight)<1){
            # warning("Sum of weights is less than one. NA will be returned.")
            res = matrix_of_na(x)
        } else {
            x = x[rep(seq_len(nrow(x)), times = weight), ]
            res = suppressWarnings(stats::cor(x = x, use = use, method = "spearman"))
        }
    } else {
        res = suppressWarnings(stats::cor(x = x, use = use, method = "spearman"))
    }
    diag(res) = 1
    res
}

internal_pairwise_sd = function(x, weight){
    positive_weight = (!is.na(weight)) & (weight>0)
    x = x[positive_weight, , drop = FALSE]
    weight = weight[positive_weight]
    seq_ncol_x = seq_len(ncol(x))
    res = matrix(NA, ncol = ncol(x), nrow = ncol(x))
    for(i in seq_ncol_x){
        seq_ncol_j = seq_ncol_x[seq_ncol_x>=i]
        for(j in seq_ncol_j){
            complete_pair = which(stats::complete.cases(x[, c(i, j)]))
            sds = suppressWarnings(
                matrixStats::colWeightedSds(x, w = weight, rows = complete_pair, cols = c(i,j)))
            res[i,j] = sds[1]
            if(i!=j){
                res[j,i] = sds[2]
            }
        }
    }
    colnames(res) = colnames(x)
    rownames(res) = colnames(x)
    res
}


internal_w_cov = function (x, weight){
    weights_sum = sum(weight)
    if(weights_sum < 1){
        # warning("Sum of weights is less than one. NA will be returned.")
        return(matrix_of_na(x))
    }
    weight = weight/weights_sum
    center = colSums(weight * x)
    x = sqrt(weight) * sweep(x, 2, center, check.margin = FALSE)
    res = crossprod(x)*weights_sum/(weights_sum - 1)
    res
}

matrix_of_na = function(x){
    res = matrix(NA_real_, nrow = ncol(x), ncol = ncol(x))
    colnames(res) = colnames(x)
    rownames(res) = colnames(x)
    res
}

internal_w_stat = function(x, weight, na.rm, check_weight_sum = FALSE, fun){
    stopif(NCOL(x)>1, "'x' should be vector or single column matrix.")
    if(is.data.frame(x)) x = x[[1]]
    if(is.logical(x)) x = as.numeric(x)
    if(!is.null(weight)){
        if(is.logical(weight)) weight = as.numeric(weight)
        if(length(weight) == 1L){
            weight = rep(weight, length(x))
        }
        stopif(length(weight) != length(x),
               "length of 'weight' must equal to the length of 'x' but length(x) == ", length(x),
               " and length(weight) == ", length(weight))

        positive_weight = (!is.na(weight)) & (weight>0)
        x = x[positive_weight]
        weight = weight[positive_weight]
        if(check_weight_sum && (sum(weight[!is.na(x)])<1)) {
            # warning("Sum of weights is less than one. NA will be returned.")
            return(NA_real_) # for data.table - it cannot combine logical and numeric in single column
        }
    }
    fun(x = x, w = weight, na.rm = na.rm)
}


## fun is one of the functions from matrixStats with fun(x, w, na.rm)
weight_helper = function(fun){
    return(
        function(x, weight = NULL, na.rm = TRUE){
            res = suppressWarnings(fun(x, w = weight, na.rm = na.rm))
            if(is.nan(res)){
                # for data.table - it cannot combine logical and numeric in single column
                NA_real_
            } else {
                res
            }
        }
    )
}
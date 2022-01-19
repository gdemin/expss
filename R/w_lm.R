#' Fitting linear models with frequency weights
#'
#' \code{w_lm} is used to fit linear models with frequency weights, also known
#' as SPSS-style weights.  'w_lm' has 'data' as a first argument for usage with
#' 'magrittr' pipes. Additionally, it supports variable labels. 'w_lm' uses
#' standard R 'lm' internally so for detailed help see \link[stats]{lm} and
#' \link[stats]{summary.lm}.
#'
#' @param data an data frame, list or environment (or object coercible by
#'   as.data.frame to a data frame) containing the variables in the model. If
#'   not found in data, the variables are taken from environment(formula),
#'   typically the environment from which w_lm is called.
#' @param formula an object of class \link[stats]{formula} (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#'   The details of model specification are given in \link[stats]{lm}.
#' @param weight an optional vector of non-negative weights to be used in the
#'   fitting process. Should be NULL or a numeric vector. If non-NULL, weighted
#'   least squares is used with weights weights (that is, minimizing
#'   sum(w*e^2));
#' @param weight_is_frequency a logical. TRUE by default. Should we treat weight
#'   as SPSS-style frequency weights?
#' @param subset an optional vector specifying a subset of observations to be
#'   used in the fitting process.
#' @param correlation logical; if TRUE, the correlation matrix of the estimated
#'   parameters is returned and printed.
#' @param symbolic.cor logical. If TRUE, print the correlations in a symbolic
#'   form (see symnum) rather than as numbers.
#' @param ... Further arguments to pass to \link[stats]{lm}.
#'
#' @return an object of class "w_lm". For details see \link[stats]{lm} and
#'   \link[stats]{summary.lm}.
#' @export
#'
#' @examples
#' 1
w_lm = function(data, formula, weight = NULL, weight_is_frequency = TRUE, subset = NULL, ...){
    labels = extract_all_var_labs(data, as_symbols = FALSE)
    # if(!is.null(weight)) ifelse(is.na(weight) | weight<0, 0, weight) else weight,  
    res = eval.parent(substitute(stats::lm(
        formula = query(data, formula),
        data = data,
        subset = subset, 
        weights = weight,
        ... 
    )))
    res[["var_labs"]] = labels
    res[["weight_is_frequency"]] = weight_is_frequency
    if(!is.null(res$weights) && weight_is_frequency){
        res$df.residual = sum(res$weights, na.rm = TRUE) - length(res$coefficients)
    }
    class(res) = union("w_lm", class(res))
    res
}

#' @export
#' @rdname w_lm
summary.w_lm = function (object, correlation = FALSE, symbolic.cor = FALSE, ...) {
    # Slightly modified version of stats::summary.lm All code written by
    # (C) R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical
    # Computing, Vienna, Austria. URL https://www.R-project.org/.
    z <- object
    p <- z$rank
    rdf <- z$df.residual
    weight_is_frequency = !is.null(z$weights) && isTRUE(z$weight_is_frequency) 
    res_class = c("summary.w_lm", "summary.lm")
    if (p == 0) {
        r <- z$residuals
        w <- z$weights
        n <- if(weight_is_frequency) sum(w, na.rm = TRUE) else length(r)
        if (is.null(w)) {
            rss <- sum(r^2)
        }
        else {
            rss <- sum(w * r^2)
            r <- sqrt(w) * r
        }
        resvar <- rss/rdf
        ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
        class(ans) <- res_class
        ans$aliased <- is.na(coef(object))
        ans$residuals <- r
        ans$df <- c(0L, n, length(ans$aliased))
        ans$coefficients <- matrix(NA_real_, 0L, 4L, dimnames = list(NULL, 
                                                                     c("Estimate", "Std. Error", "t value", "Pr(>|t|)")))
        ans$sigma <- sqrt(resvar)
        ans$r.squared <- ans$adj.r.squared <- 0
        ans$cov.unscaled <- matrix(NA_real_, 0L, 0L)
        if (correlation) 
            ans$correlation <- ans$cov.unscaled
        ans$weight_is_frequency = weight_is_frequency
        return(match_labels(ans, z$var_labs))
    }
    if (is.null(z$terms)) 
        stop("invalid 'lm' object:  no 'terms' component")
    if (!inherits(object, "lm")) 
        warning("calling summary.lm(<fake-lm-object>) ...")
    r <- z$residuals
    f <- z$fitted.values
    w <- z$weights
    Qr <- if_null(object$qr, 
                  stop("lm object does not have a proper 'qr' component.\n Rank zero or should not have used lm(.., qr=FALSE).")
                  )
    n <- if(weight_is_frequency) sum(w, na.rm = TRUE) else NROW(Qr$qr)
    if (is.na(z$df.residual) || n - p != z$df.residual) 
        warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
    if (is.null(w)) {
        mss <- if (attr(z$terms, "intercept")) 
            sum((f - mean(f))^2)
        else sum(f^2)
        rss <- sum(r^2)
    }
    else {
        mss <- if (attr(z$terms, "intercept")) {
            m <- sum(w * f/sum(w))
            sum(w * (f - m)^2)
        }
        else sum(w * f^2)
        rss <- sum(w * r^2)
        r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    if (is.finite(resvar) && resvar < (mean(f)^2 + var(c(f))) * 
        1e-30) 
        warning("essentially perfect fit: summary may be unreliable")
    p1 <- 1L:p
    R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    se <- sqrt(diag(R) * resvar)
    est <- z$coefficients[Qr$pivot[p1]]
    tval <- est/se
    ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
    ans$residuals <- r
    ans$coefficients <- cbind(Estimate = est, `Std. Error` = se, 
                              `t value` = tval, `Pr(>|t|)` = 2 * pt(abs(tval), rdf, 
                                                                    lower.tail = FALSE))
    ans$aliased <- is.na(z$coefficients)
    ans$sigma <- sqrt(resvar)
    ans$df <- c(p, rdf, NCOL(Qr$qr))
    if (p != attr(z$terms, "intercept")) {
        df.int <- if (attr(z$terms, "intercept")) 
            1L
        else 0L
        ans$r.squared <- mss/(mss + rss)
        ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - 
                                                             df.int)/rdf)
        ans$fstatistic <- c(value = (mss/(p - df.int))/resvar, 
                            numdf = p - df.int, dendf = rdf)
    }
    else ans$r.squared <- ans$adj.r.squared <- 0
    ans$cov.unscaled <- R
    dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1, 
                                                               1)]
    if (correlation) {
        ans$correlation <- (R * resvar)/outer(se, se)
        dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
        ans$symbolic.cor <- symbolic.cor
    }
    if (!is.null(z$na.action)) 
        ans$na.action <- z$na.action
    ans$weight_is_frequency = weight_is_frequency
    class(ans) <- res_class
    match_labels(ans, z$var_labs)
}

match_labels = function(summary_res, labels){
    if(length(labels) == 0) return(summary_res)
    var_names = rownames(summary_res$coefficients) 
    if(length(var_names)==0) return(summary_res)
    var_names = ifelse(var_names %in% names(labels), labels[var_names], var_names)
    rownames(summary_res$coefficients)  = var_names
    summary_res
}
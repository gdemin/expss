#' Calculate significance (p-values) of differences between proportions/means
#'
#' @param prop1 fsdfdf
#' @param prop2 sfs
#' @param mean1 fsdfdf
#' @param mean2 sfs
#' @param sd1 fsdfdf
#' @param sd2 sfs
#' @param base1 sfsf
#' @param base2 sfsf
#' @param var_equal sfsf
#' @param common_base sfsf
#'
#' @return numeric vector with p-values
#' @export
#'
#' @examples
#' 1
compare_proportions = function(prop1, prop2, base1, base2, common_base = 0){
    # ftp://public.dhe.ibm.com/software/analytics/spss/documentation/statistics/20.0/en/client/Manuals/IBM_SPSS_Statistics_Algorithms.pdf
    # IBM SPSS Statistics Algorithms v20, p. 263
    pooled_prop = (prop1*base1 + prop2*base2)/(base1 + base2)
    z_statistic = (prop1 - prop2)/
        sqrt(pooled_prop*(1 - pooled_prop)*(1/base1 + 1/base2 - 2*common_base/base1/base2))
    2*(1 - pnorm(abs(z_statistic)))
} 

########################

#' @rdname compare_proportions
#' @export
compare_means = function(mean1, mean2, sd1, sd2, base1, base2, common_base = 0, var_equal = FALSE){
    # ftp://public.dhe.ibm.com/software/analytics/spss/documentation/statistics/20.0/en/client/Manuals/IBM_SPSS_Statistics_Algorithms.pdf
    # IBM SPSS Statistics Algorithms v20, p. 267
    if(common_base>0 || var_equal){
        pooled_sd = sqrt((sd1*sd1*(base1 - 1) + sd2*sd2*(base2 - 1))/(base1 + base2 - 2))
        t_statistic = (mean1 - mean2)/
            pooled_sd/sqrt(1/base1 + 1/base2 - 2*common_base/base1/base2)
        2*pt(-abs(t_statistic), df = base1 + base2 - common_base - 2)
    } else {
        # R Core Team (2017). R: A language and environment for statistical computing. R Foundation for
        # Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
        # t.test(..., var.equal = FALSE)
        stderr1 = sd1/sqrt(base1)
        stderr2 = sd2/sqrt(base2)
        stderr = sqrt(stderr1^2 + stderr2^2)
        df = stderr^4/(stderr1^4/(base1 - 1) + stderr2^4/(base2 - 1))
        t_statistic = (mean1 - mean2)/stderr
        2 * pt(-abs(t_statistic), df)
    }
} 
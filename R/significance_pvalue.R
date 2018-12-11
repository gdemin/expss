#' Calculate significance (p-values) of differences between proportions/means
#' 
#' \code{compare_proportions} calculates p-values (via z-test) for comparison
#' between each proportion in the \code{prop1} and \code{prop2}. Results are calculated
#' with the same formula as in \link[stats]{prop.test} without continuity
#' correction.
#' \code{compare_means} calculates p-values (via t-test) for comparison between 
#' each mean in the \code{mean1} and \code{mean2}. Results are calculated on the
#' aggregated statistics (means, std. devs, N) with the same formula as in
#' \link[stats]{t.test}.
#' These functions mainly intended for usage inside \link{significance_cpct} and
#' \link{significance_means}.
#' 
#' @param prop1 a numeric vector of proportions in the group 1. Values should be
#'   between 0 and 1
#' @param prop2 a numeric vector of proportions in the group 2. Values should be
#'   between 0 and 1
#' @param mean1 a numeric vector of the means in the first group.
#' @param mean2 a numeric vector of the means in the second group. 
#' @param sd1 a numeric vector of the standard deviations in the first group.
#'   Values should be non-negative.
#' @param sd2 a numeric vector of the standard deviations in the second group.
#'   Values should be non-negative.
#' @param base1 a numeric vector for \code{compare_means} and single number for 
#'   \code{compare_proportions}. Number of valid cases for each mean in the first group for 
#'   \code{compare_means} and number of cases for \code{compare_proportions}.
#' @param base2 a numeric vector for \code{compare_means} and single number for 
#'   \code{compare_proportions}. Number of valid cases for each mean in the
#'   second group for \code{compare_means} and number of cases for
#'   \code{compare_proportions}.
#' @param var_equal a logical variable indicating whether to treat the variances
#'   in the groups as being equal. For details see \link[stats]{t.test}.
#' @param common_base numeric. Number of cases that belong to both values in the
#'   first and the second argument. It can occur in the case of overlapping
#'   samples. Calculations are made according to algorithm in IBM SPSS Statistics
#'   Algorithms v20, p. 263. Note that with these adjustments t-tests between
#'   means are made with equal variance assumed (as with \code{var_equal =
#'   TRUE}).
#'   
#' @seealso \link{significance_cpct}, \link{significance_means}, 
#'   \link[stats]{prop.test}, \link[stats]{t.test}
#' @return numeric vector with p-values
#' @export
#'
#' @examples
#' # proportions
#' data(mtcars)
#' counts = table(mtcars$am, mtcars$vs)
#' props = prop.table(counts)
#' compare_proportions(props[,1], props[,2], 
#'                     colSums(counts)[1], colSums(counts)[1])
#'                     
#' # means
#' t.test(mpg ~ am, data = mtcars)$p.value 
#' # the same result
#' calculate(mtcars, 
#'           compare_means(
#'               mean(mpg[am==0]), mean(mpg[am==1]), 
#'               sd(mpg[am==0]),  sd(mpg[am==1]),
#'               length(mpg[am==0]), length(mpg[am==1])
#'           ))
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
    if_na(common_base) = 0
    if(any(common_base>0) || var_equal){
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
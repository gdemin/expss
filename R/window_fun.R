#' Function over grouping variables (window function)
#'
#' This is faster version of \link[stats]{ave}. \code{window_fun} applies function
#' to every subset of \code{x} and return vector of the same length as \code{x}.
#'
#' @param x A vector
#' @param ... Grouping variables all of the same length as x or length 1 and
#'   function as last argument.
#'
#' @return vector of the same length as \code{x}
#' @export
#' @examples
#' window_fun(1:3, mean)  # no grouping -> grand mean
#'
#' attach(warpbreaks)
#'
#' window_fun(breaks, wool, mean)
#' window_fun(breaks, tension, function(x) mean(x, trim = 0.1))
#'
#' detach(warpbreaks)
window_fun = function(x, ...){
    UseMethod("window_fun")
}

#' @export
window_fun.default = function(x, ...){
    args = list(...)
    stopif(length(args)==0, "'window_fun' - insufficient number of arguments.")
    fun = match.fun(args[[length(args)]])
    grouping_variables = flat_list(args[-length(args)], flat_df = TRUE)
    res = NULL # to bypass R CMD check note
    if(length(grouping_variables)){
        expr = parse(text = "fun(x)")
        grouping_names = paste0("gr", seq_along(grouping_variables))
        stopif(!all(unique(lengths(grouping_variables)) %in% c(1, NROW(x))),
               "`window_fun` - all variables should be of the same length or length 1.")
        dt_table = as.data.table(c(list(x), grouping_variables))
        setnames(dt_table, c("x", grouping_names))
        dt_table[, res:=eval(expr), by = grouping_names]
        dt_table[["res"]]
    } else {
        rep(fun(x), length(x))
    }
}


#' Function over grouping variables (window function)
#'
#' This is faster version of \link[stats]{ave}. \code{window_fun} applies function
#' to every subset of \code{x} and return vector of the same length as \code{x}.
#'
#' @param x A vector
#' @param ... Grouping variables all of the same length as x or length 1 and one function
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
    args = list(...)
    is_function = vapply(args, is.function, FUN.VALUE = logical(1))
    functions = args[is_function]
    stopif(length(functions)==0, "`window_fun` - no function among the arguments.")
    stopif(length(functions)>1, "`window_fun` - only one function should be provided.")
    fun = functions[[1]]
    grouping_variables = args[!is_function]
    res = NULL # to bypass R CMD check note
    if(length(grouping_variables)){
        expr = parse(text = "fun(x)")
        grouping_names = paste0("gr", seq_along(grouping_variables))
        stopif(!all(unique(lengths(grouping_variables)) %in% c(1, NROW(x))),
               "`window_fun` - all variables should be of the same length or length 1.")
        dt_table = as.data.table(c(list(x), grouping_variables))
        setnames(dt_table, c("x", grouping_names))
        by_string = paste(grouping_names, collapse = ",")
        dt_table[, res:=eval(expr), by = by_string]
        dt_table[["res"]]
    } else {
        rep(fun(x), length(x))
    }
}

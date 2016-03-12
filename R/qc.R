#' Create vector of characters from unquoted strings (variable names)
#' 
#' In many cases one need to address variables in list/data.frame in such 
#' manner: \code{dfs[ , c("var1", "var2", "var3")]}. \code{qc} ("quoted c") is a
#' shortcut for the such cases to reduce keystrokes. With \code{qc} you can write:
#' \code{dfs[ , qc(var1, var2, var3)]}.
#' 
#' @param ... unquoted names of variables
#'   
#' @return Vector of characters
#' @examples
#' qc(a, b, c)
#' identical(qc(a, b, c), c("a", "b", "c"))
#' 
#' mtcars[, qc(am, mpg, gear)]
#' 
#' @export
qc = function(...){
    as.character(substitute(c(...))[-1])
}
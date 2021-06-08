#' Create vector of characters from unquoted strings (variable names)
#' 
#' \itemize{
#' \item{\code{qc} }{It is often needed to address variables in the data.frame in the such 
#' manner: \code{dfs[ , c("var1", "var2", "var3")]}. \code{qc} ("quoted c") is a
#' shortcut for the such cases to reduce keystrokes. With \code{qc} you can write:
#' \code{dfs[ , qc(var1, var2, var3)]}.}
#' \item{\code{qe} }{returns list of expression.}}
#' @param ... unquoted names of variables in
#'   \code{qc} or unquoted expressions in \code{qe}.
#' @return Vector of characters or expressions
#' @examples
#' 
#' ## qc
#' qc(a, b, c)
#' identical(qc(a, b, c), c("a", "b", "c"))
#' 
#' mtcars[, qc(am, mpg, gear)]
#' 
#' ## qe
#' qe(mrset(a1 %to% a6), mrset(b1 %to% b6), mrset(c1 %to% c6))
#' @export
qc = function(...){
    as.character(substitute(c(...))[-1])
}


#' @export
#' @rdname qc
qe = function(...){
    expr = substitute(list(...))
    as.list(expr)[-1]
}



#' Create vector of characters from unquoted strings (variable names)
#' 
#' \itemize{
#' \item{\code{qc} }{It is often needed to address variables in the data.frame in the such 
#' manner: \code{dfs[ , c("var1", "var2", "var3")]}. \code{qc} ("quoted c") is a
#' shortcut for the such cases to reduce keystrokes. With \code{qc} you can write:
#' \code{dfs[ , qc(var1, var2, var3)]}.}
#' \item{\code{qe} }{returns list of expression. It is useful to create substitution list for \code{..$arg}.}
#' \item{\code{text_expand} }{is simple string interpolation function. It searches in its
#' arguments expressions in curly brackets \code{{expr}}, evaluate them and substitute with
#' the result of evaluation. See examples.}
#' }
#' @param ... characters in \code{text_expand}, unquoted names of variables in
#'   \code{qc} or unquoted expressions in \code{qe}.
#' @param delim character vector of length 2 - pair of opening and closing
#'   delimiters for the templating tags. By default it is curly brackets. Note
#'   that \code{delim} will be used in the perl-style regular expression so you
#'   need to escape special characters, e. g. use \code{"\\\{"} instead of
#'   \code{"\{"}.
#' @return Vector of characters
#' @examples
#' 
#' ## qc
#' qc(a, b, c)
#' identical(qc(a, b, c), c("a", "b", "c"))
#' 
#' mtcars[, qc(am, mpg, gear)]
#' 
#' ## text_expand
#' i = 1:5
#' text_expand("q{i}")
#' 
#' i = 1:3
#' j = 1:3
#' text_expand("q1_{i}_{j}")
#' 
#' data(iris)
#' text_expand("'iris' has {nrow(iris)} rows.")
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


#' @export
#' @rdname qc
text_expand = function(..., delim = c("\\{", "\\}")){
    if(length(delim)!=2){
        stop("'text_expand': 'delim' should be vector of length two.")
    }
    left = delim[[1]]
    right = delim[[2]]
    pattern = paste0(left, "(.+?)", right)
    all_vars= c(list(...), recursive = TRUE)
    res = vector(mode = "list", length = length(all_vars))
    for(each_var in seq_along(all_vars)){
        x = all_vars[each_var]
        if(any(grepl(pattern, x, perl = TRUE))){
            positions = gregexpr(pattern, x, perl = TRUE)    
            matches = rev(unique(unlist(regmatches(x, positions))))
            var_names = gsub(right, "", gsub(left, "", matches, perl = TRUE), perl = TRUE)
            
            for(i in seq_along(var_names)){
                evaluated_item = eval(parse(text = var_names[i]),
                                      envir = parent.frame(),
                                      enclos = baseenv())
                x = unlist(lapply(evaluated_item, function(item){
                    gsub(matches[i], item, x, fixed = TRUE)
                    
                }))
                
            }
            
        } 
        res[[each_var]] = x
    }
    c(res, recursive = TRUE)
}


#' @export
#' @rdname qc
subst = function(...){
    expr = substitute(text_expand(..., delim = c("`", "`")))
    str_expr = expr_to_character(expr)
    .Deprecated(new = str_expr)
    eval.parent(expr)
}


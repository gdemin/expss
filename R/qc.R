#' Create vector of characters from unquoted strings (variable names)
#' 
#' In many cases one need to address variables in list/data.frame in such 
#' manner: \code{dfs[ , c("var1", "var2", "var3")]}. \code{qc} ("quoted c") is a
#' shortcut for the such cases to reduce keystrokes. With \code{qc} you can write:
#' \code{dfs[ , qc(var1, var2, var3)]}.
#' \code{subst} is simple string interpolation function. It searches in its
#' arguments expressions in backticks (`), evaluate it and substitute it with
#' result of evaluation. See examples.
#' 
#' 
#' @param ... characters in \code{subst}/unquoted names of variables in \code{qc}
#'   
#' @return Vector of characters
#' @examples
#' 
#' ## qc
#' qc(a, b, c)
#' identical(qc(a, b, c), c("a", "b", "c"))
#' 
#' mtcars[, qc(am, mpg, gear)]
#' 
#' ## subst
#' i = 1:5
#' subst("q`i`")
#' 
#' i = 1:3
#' j = 1:3
#' subst("q1_`i`_`j`")
#' 
#' data(iris)
#' subst("'iris' has `nrow(iris)` rows.")
#' @export
qc = function(...){
    as.character(substitute(c(...))[-1])
}

#' @export
#' @rdname qc
subst = function(...){
    all_vars= c(list(...), recursive = TRUE)
    res = vector(mode = "list", length = length(all_vars))
    for(each_var in seq_along(all_vars)){
        x = all_vars[each_var]
        if(any(grepl("`(.+?)`", x, perl = TRUE))){
            positions = gregexpr("`(.+?)`", x, perl = TRUE)    
            matches = unique(unlist(regmatches(x, positions)))
            var_names = rev(gsub("`", "", matches))
            
            for(each_item in seq_along(var_names)){
                evaluated_item = eval(parse(text = var_names[each_item]),
                                      envir = parent.frame(),
                                      enclos = baseenv())
                curr = paste0("`",var_names[each_item],"`")
                x = unlist(lapply(evaluated_item, function(item){
                    gsub(curr, item, x, fixed = TRUE)
                    
                }))
                
            }
            
        } 
        res[[each_var]] = x
    }
    c(res, recursive = TRUE)
}

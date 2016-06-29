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
    sprintf2 = function(fmt, replacement){
        number_of_s = vapply(regmatches(fmt, gregexpr("%s", fmt, fixed = TRUE)), 
                             length,
                             FUN.VALUE = numeric(1))
        if(number_of_s<2){
            sprintf(fmt, replacement)
        } else {
            replacement = list(replacement) %r% number_of_s
            replacement = c(fmt = fmt, replacement)
            do.call(sprintf, replacement)
        }
    }
    .all_vars_ = c(list(...), recursive = TRUE)
    res = vector(mode = "list", length = length(.all_vars_))
    for(each in seq_along(.all_vars_ )){
        x = .all_vars_ [each]
        stopif(any(grepl("%s",x)), "%s inside 'x' is not allowed.")
        if(any(grepl("`(.+?)`", x, perl = TRUE))){
            .positions_ = gregexpr("`(.+?)`", x, perl = TRUE)    
            .matches_ = unique(unlist(regmatches(x, .positions_)))
            .var_names_ = gsub("`", "", .matches_)
            #.vars_ = mget(.var_names_, inherits = TRUE)
            for(.each_ in seq_along(.var_names_)){
                .curr_ = paste0("`",.var_names_[.each_],"`")
                x = gsub(.curr_, "%s", x, fixed = TRUE)
                .res_ = eval(parse(text = .var_names_[.each_]),
                             envir = parent.frame(), 
                             enclos = globalenv())
                if(length(x)>1 && length(.res_)>1){
                    x = unlist(lapply(x, function(.x_){
                        sprintf2(.x_, .res_)
                    }))
                } else {
                    x  = sprintf2(x, .res_)    
                }
                
            }
            
        } 
        res[[each]] = x
    }
    c(res, recursive = TRUE)
}


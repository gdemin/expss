#' Function for checking arbitrary error conditions.
#' 
#' This function is designed for working with piping operator \code{\link[magrittr]{\%>\%}}
#'  from 'magrittr' package.  
#'  
#' @param .data Dataset that will be checked.
#' @param expr Expression that returns logical vector. TRUE is incorrect value (error), 
#' all other values are considered as valid cases.
#' @param show Additional variables (such as 'id') that should be shown with report about 
#' errors.
#' @return original data.frame .data with attribute with
#'   results of checking.  
#' @details
#'  Supposed usage is: \code{checked_data.frame  \%>\% error_if(expression)}.
#'  It is possible to use this function with conditional operators \code{\link{check_if}}
#'  and \code{\link{check_subset}}. Both conditional operators will have the same effect:
#'  expression will be checked only for TRUE values in conditions. 
#' @seealso \code{\link{check_if}}, \code{\link{check_subset}}, 
#' \code{\link[magrittr]{\%>\%}}, \code{\link{move}}
#' @export
#' @examples
#' 
#' library(dplyr)
#' data(ProductTestRaw)
#' 
#' # Age category and age should be consistent. 4 errors
#' ProductTestRaw  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2))  %>% report
#' # No errors
#' ProductTestRaw  %>% error_if((s2a %in% 27:35) & !(s2b %in% 3))  %>% report
#' 
#' # Show 'id' and 'cell'
#' ProductTestRaw  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2),show=c(id,cell))  %>% report
#' 
#' # Conditional checking. There  are no errors in cell 2.
#' 
#' ProductTestRaw  %>% check_if(cell==2)  %>% error_if((s2a %in% 18:26) & !(s2b %in% 2))  %>% report
#' 
#' # Usage in programming (e. g. in cycle 'for')
#' # Similar to Example 4 for sngl function.
#' checked_vars = c("a3","a22","b3","b23")
#' 
#' # there is one error in a22
#' 
#' for (each_var in checked_vars){
#'      cat("Question:",each_var,"\n")
#'      ProductTestRaw  %>% 
#'          error_if(!(ProductTestRaw[,each_var] %in% 1:7),show=c(id,cell))  %>% 
#'          report     
#' }
error_if = function(.data,expr,show=NULL){
    error_if_ (.data,
           expr=lazyeval::lazy(expr),
           show=lazyeval::lazy(show)
           )    
    
}


#' @export
#' @rdname error_if
.error_if = function(expr,show=NULL){
    .data = default_dataset()
    error_if_ (.data,
               expr=lazyeval::lazy(expr),
               show=lazyeval::lazy(show)
    )    
    
}


error_if_ = function(.data,expr,show=NULL){
    # TODO Should be rewritten 

    if ("chk_if" %in% class(.data)){
        cond=.data$cond
        subset=.data$subset
        .data=.data$.data
        stopif(is.null(.data),"Incorrect 'chk_if' object. No data.")
    } else {
        cond= NULL
        subset= NULL
    }
    dat = ref(.data)
    err_if = lazyeval::lazy_eval(expr,dat)
    var_names = all.vars(expr)
    if ("lazy" %in% class(expr)) {
        var_names = all.vars(expr$expr) 
        if (any(var_names %in% colnames(dat))) {
            vars = select_(dat,.dots = var_names[var_names %in% colnames(dat)])
        } else {
            vars = NULL
        }
    } else {
        vars = NULL
    }
    if (!is.null(cond)) err_if[!(cond %in% TRUE)] = FALSE 
    if (!is.null(subset)) err_if[!(subset %in% TRUE)] = FALSE 
    if ("lazy" %in% class(show)) {
        if (!is.null(show$expr)) show_var = select_(dat,.dots=lazyeval::all_dots(show)) else show_var = NULL
    } else {
        if (!is.null(show)) show_var = select_(dat,.dots=show) else show_var = NULL
        
    }
    chk_err = ifelse(err_if %in% TRUE,.ERROR_IF,NA)
    fin = setNames(data.frame(chk_err,stringsAsFactors = FALSE),.CHK_ERR)
    if (!is.null(cond)) fin[,.CHK_COND] = cond
    if (!is.null(vars)) fin = data.frame(fin,vars,stringsAsFactors = FALSE)
    if (!is.null(show_var)) fin = data.frame(fin,show_var,stringsAsFactors = FALSE)
    class(fin) = unique(c("check",class(fin)))
    check_result(dat) = fin
    ref(.data) = dat
    invisible(.data)

    
}





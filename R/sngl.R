#' Functions for checking multiple/single-response questions for valid values 
#' 
#' These functions are designed for working with piping operator \code{\link[magrittr]{\%>\%}}
#'  from 'magrittr' package.  
#'  
#' @param ... Comma separated list of unquoted variable names. You can use special 
#' functions such as 'starts_with', 'ends_with', 'contains' and etc. For details see
#' \code{\link[dplyr]{select}} in package 'dplyr'.
#' @param .dots  Use mult_()/sngl_() to do standard evaluation. 
#' See vignette("nse") in dplyr package for details   
#' @param no_dup Logical. Should we check for absence of duplicated values in each row?
#' @param show Additional variables (such as 'id') that should be shown with report about 
#' errors.
#' @param .data data.frame
#' @return \code{mult}/\code{sngl} functions returns another function 
#' value(.data,...,exclusive). That function accepts data.frame and valid values.
#'  See examples. 'value' function return data.frame with attribute with
#'   results of checking.
#' \code{report} print results of checking and invisibly returns checked data.frame.   
#' @details
#'  Supposed usage is: \code{checked_data.frame  \%>\% mult(var_names)(valid_values,exclusive)}
#'  or \code{checked_data.frame  \%>\% mult(var_names)(valid_values)}
#'  Only valid values should be exists in var_names. All other values will be considered
#'   incorrect. For multiple response questions (\code{mult}) it is only necessary
#'  that each row will have at least one valid non-NA values.
#'  If any of exclusive values exists in row all other values should be NA . 
#'  \code{mult} function checks multiple response questions only with categorical coding.
#'  For checking multiple response questions with dichotomous coding see \code{\link{dmult}}. 
#'  If 'values' is missing than all values considered valid except NA.
#'  Only first error in the row is reported. If there are other errors for 
#'  this case they will be reported only after correction of first error.
#' @seealso \code{\link{check_internal}}, \code{\link[dplyr]{select}}, 
#' \code{\link[magrittr]{\%>\%}}, \code{\link{dmult}}, \code{\link{test}}, 
#' \code{\link{move}}
#' @export
#' @examples
#' 
#' library(dplyr)
#' data(ProductTestRaw)
#' 
#' ## Example 1 ##
#' 
#' # 4 errors: 2 missing, 2 invalid codes
#' ProductTestRaw  %>% 
#'          sngl(s2b)(2:3)  %>% 
#'          report
#' 
#' ## Example 2 ##
#' 
#' data(codeframe)
#' valid_a1 = make_labels(codeframe$likes)
#' 
#' # Exclusive values
#' # 1 Liked everything
#' # 2 Disliked everything
#' # 99 Hard to say
#' 
#' # 5 errors: 1 missing value, 1 invalid code, 1 code duplication, 
#' # 2 non-exclusive values.
#' # Additional variable 'id' also is shown.
#' ProductTestRaw  %>% 
#'          mult(a1_1:a1_6,show = id)(valid_a1,exclusive=c(1,2,99))  %>% 
#'          report
#' 
#' ## Example 3 ##
#' 
#' valid_a4 = make_labels(codeframe$dislikes_in_appearance)
#' 
#' # question a4 was asked only if codes 1-4 marked in a3
#' # 3 errors: 1 missing value, 1 invalid code, 1 code in case of a3 in 5-7.
#' ProductTestRaw  %>% 
#'          check_if(a3 %in% 1:4)  %>% 
#'          mult(a4_1:a4_6)(valid_a4,exclusive=99)  %>%
#'          report 
#' 
#' 
#' ## Example 4 ##
#' 
#' Usage in programming (e. g. in cycle 'for')
#' 
#' checked_vars = c("a3","a22","b3","b23")
#' # there is one error in a22
#' 
#' for (each_var in checked_vars){
#'      ProductTestRaw  %>% 
#'          sngl_(each_var)(1:7)  %>% 
#'          report     
#' }
#' 
sngl = function(...,no_dup=FALSE,show=NULL){
    
    sngl_ (.dots=lazyeval::lazy_dots(...),
           no_dup=no_dup,
           show=lazyeval::lazy(show))    
    
}


#' @export
#' @rdname sngl
sngl_ = function(...,.dots,no_dup=FALSE,show=NULL){
    
    vars = lazyeval::all_dots(.dots,...)

    values_fun = function(.data,..., exclusive=NULL){
        if ("chk_if" %in% class(.data)){
            cond=.data$cond
            subset=.data$subset
            .data=.data$.data
            stopif(is.null(.data),"Incorrect 'chk_if' object. No data.")
        } else {
            cond= NULL
            subset= NULL
        }
        
        dfs = dplyr::select_(.data,.dots=vars)
        values = list(...)
        
        ## for case when function/criteria supplied as valid value
        funcs = sapply(values,is.function)
        if (any(funcs)){
            first_fun_index = which(funcs)[1]
            new_values = crit(values[[first_fun_index]])
            values = values[-first_fun_index]
            for (i in seq_along(values)){
                new_values = new_values | values[[i]]
            }
            values = new_values
        } else {
            values=unlist(values)
        }
        res = check_internal(dfs,values=values,exclusive = exclusive,mult = FALSE,no_dup = no_dup,cond=cond,subset=subset)
        check_result(.data) = res
        invisible(.data)    
    }
    invisible(values_fun)
    
}


    
#### TODO аргументы со степенью детальности вывода информации... соответсвенно, их и в метод print надо добавить
#### TODO тоже в print - таблицу с частотками правильных значений
#' @export
#' @rdname sngl
report = function(.data){
    chk = check_result(.data)
    if (is.null(chk)){
        warning("No result of checking")
        return(invisible(.data))
    } 
    print (chk)
    invisible(.data)
}



###############
always = function(dfs){
    UseMethod("always")
}

always.data.frame = function(dfs){
    res = attr(dfs,"always",exact = TRUE)
    if (!is.null(res)) {
        res = intersect(res,colnames(dfs))
        if (length(res)==0) res = NULL
    }
    res
}

"always<-" = function(x,value){
    
    UseMethod("always<-")
}

"always<-.data.frame" = function(x,value){
    stopif (!all(value %in% colnames(x)),"'",paste(setdiff(value,colnames(x)),collapse=","),"' doesn't exist in data.frame")
    attr(x,"always") = value
    x
    
}


###########################

check_result = function(dfs){
    UseMethod("check_result")
}

check_result.data.frame = function(dfs){
    res = attr(dfs,"check_result",exact = TRUE)
    res
}

"check_result<-" = function(x,value){
    
    UseMethod("check_result<-")
}

"check_result<-.data.frame" = function(x,value){
    attr(x,"check_result") = value
    x   
}
#' Functions for checking multiple/single-response questions for valid values 
#' 
#' These functions are designed for working with piping operator \code{\link[magrittr]{`%>%`}}
#'  from 'magrittr' package.  
#'  
#' @param ... List of variables.
#' @param no_dup Logical. Should we check for absence of duplicated values in each row?
#' @param cond Logical vector. TRUE indicated rows in dfs that should contain valid
#'  values. In other rows all dfs values should be NA. It used for questions that
#'   were asked by condition on answer on previous questions.
#' @param subset Logical vector. TRUE indicated rows in dfs that should be checked. 
#' Other rows will be ignored.
#' @param show
#' @param .data Check object for printing.
#' 
#' @return 
#' \code{check_internal} return object of class 'check'. It is data.frame that contains 
#' check result for each row of dfs and description for each error if any of them
#'  exists.
#' \code{print.check} invisibly returns its argument x. 
#' \code{summary.check} returns list with summary check.
#' 
#' @details
#' values Valid values. All other values will be considered incorrect.
#' exclusive Numeric/character values. These values should be exclusive. 
#' All other values should be NA if any of exclusive values exists in row. 
#' 'mult=TRUE' for multiple response questions means that it is allowed to have NA
#'  values in row. It is only necessary for multiple response questions that each
#'  row will have at least one non-NA values.
#'  This function checks multiple response questions only with categorical coding.
#'  For checking multiple response questions with dichotomous coding see \code{\link{dmult}}. 
#'  If 'mult=FALSE' all values should be non-NA. However one can put NA in 'values' argument.
#'  Then NA will be considered valid.
#'  By default if 'mult=TRUE' then no_dup also is TRUE.
#'  If 'values' is missing than all values considered valid except NA.
#'  'check' report only for first error in row. If there are other errors for 
#'  this case they will be reported only after correction of first error.
#'
#' @seealso \code{\link{mult}}, \code{\link{mult_}}, \code{\link{sngl}}, 
#' \code{\link{sngl_}}, \code{\link{dmult}}
#' @export
#' @examples
#' 
#' library(dplyr)
#' data(ProductTestRaw)
#' 
#' ## Example 1 ##
#' 
#' # 4 errors: 2 missing, 2 invalid codes
#' check_internal(ProductTestRaw$s2b,values=2:3)
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
#' # 2 non-exclusive values
#' check_internal(select(ProductTestRaw,a1_1:a1_6),values=valid_a1,
#'      mult = TRUE, exclusive=c(1,2,99))
#' 
#' ## Example 3 ##
#' 
#' valid_a4 = make_labels(codeframe$dislikes_in_appearance)
#' # question a4 was asked only if codes 1-4 marked in a3
#' # 3 errors: 1 missing value, 1 invalid code, 1 code in case of a3 in 5-7.
#' check_internal(select(ProductTestRaw,a4_1:a4_6),
#'      values=valid_a4,mult = TRUE, exclusive=99,
#'       cond = ProductTestRaw$a3 %in%  1:4)
#' 
sngl = function(...,cond=NULL,subset=NULL,no_dup=FALSE,show=NULL){
    
    sngl_ (.dots=lazyeval::lazy_dots(...),
           cond=lazyeval::lazy(cond),
           subset=lazyeval::lazy(subset),
           no_dup=no_dup,
           show=lazyeval::lazy(show))    
    
}

#' @export
#' @rdname sngl
mult = function(...,cond=NULL,subset=NULL,no_dup=TRUE,show=NULL){
    
    mult_ (.dots=lazyeval::lazy_dots(...),
           cond=lazyeval::lazy(cond),
           subset=lazyeval::lazy(subset),
           no_dup=no_dup,
           show=lazyeval::lazy(show))    
    
}

#' @export
#' @rdname sngl
sngl_ = function(...,.dots,cond=~NULL,subset=~NULL,no_dup=FALSE,show=NULL){
    
    vars = lazyeval::all_dots(.dots,...)
    
    function(.data,...,exclusive=NULL){
        dfs = dplyr::select_(.data,.dots=vars)
        cond = lazyeval::lazy_eval(cond,.data)
        subset = lazyeval::lazy_eval(subset,.data)
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
        
        res = check_internal(dfs,values=values,exclusive = exclusive,mult = FALSE,no_dup = no_dup,cond = cond,subset = subset)
        check_result(.data) = res
        invisible(.data)
    }
    
}

#' @export
#' @rdname sngl
mult_ = function(...,.dots,cond=~NULL,subset=~NULL,no_dup=TRUE,show=NULL){
    
    vars = lazyeval::all_dots(.dots,...)
    
    function(.data,...,exclusive=NULL){
        dfs = dplyr::select_(.data,.dots=vars)
        #         browser()
        cond = lazyeval::lazy_eval(cond,.data)
        subset = lazyeval::lazy_eval(subset,.data)
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
        
        res = check_internal(dfs,values=values,exclusive = exclusive,mult = TRUE,no_dup = no_dup,cond = cond,subset = subset)
        check_result(.data) = res
        invisible(.data)
    }
    
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
#' Functions for checking conditional questions/subset of data.
#' 
#' These functions are designed for working with piping operator 
#' \code{\link[magrittr]{\%>\%}} from 'magrittr' package.  
#' 
#' @param .data data.frame for checking 
#' @param expr Logical vector/expression. 
#' 
#' @details TRUE in expr vector indicated rows in .data that should contain valid
#'  values. For \code{check_if} in other rows all .data values should be NA. It used for questions that
#'   were asked by condition on answer from previous questions. For \code{check_subset} values in 
#'   other rows will be ignored - they are all will be considered as valid.
#' 
#' @return These functions returns object of class 'chk_if'. It is suited for consumption by 
#' functions: \code{\link{sngl}},\code{\link{mult}},\code{\link{dmult}}, \code{\link{error_if}} and
#' \code{\link{move}}. Also it can be accepted by next \code{\link{check_if}} and/or 
#' \code{\link{check_subset}}.
#' 
#' @seealso \code{\link{sngl}}, \code{\link{mult}},  \code{\link{dmult}}, 
#' \code{\link{error_if}} and \code{\link{move}}
#' @export
#' @examples
#' 
#' library(dplyr)
#' data(ProductTestRaw)
#' data(codeframe)
#' 
#' ## Example 1 ##
#' 
#' valid_a4 = make_labels(codeframe$dislikes_in_appearance)
#' # question a4 was asked only if codes 1-4 marked in a3
#' # 3 errors: 1 missing value, 1 invalid code, 1 code in case of a3 in 5-7.
#' ProductTestRaw  %>% 
#'      check_if(a3 %in% 1:4)  %>% 
#'      mult(a4_1:a4_6)(valid_a4,exclusive=99)  %>%
#'      report 
#' 
#' ## Example 2 ##
#' 
#' # There are no errors in question s2b in cell 2.
#' ProductTestRaw  %>% 
#'      check_subset(cell %in% 2) %>% 
#'      sngl(s2b)(2,3)  %>% 
#'      report
#' 
#' ## Example 3 ##
#' 
#' # There are no errors in question a4 in cell 2.
#' ProductTestRaw  %>% 
#'      check_subset(cell %in% 2)  %>% 
#'      check_if(a3 %in% 1:4)  %>% 
#'      mult(a4_1:a4_6)(valid_a4,exclusive=99)  %>%
#'      report 
#' 
check_if = function(.data,expr){
    if (missing(.data)){
        .data = default_dataset() 
        expr=lazyeval::lazy(expr)
    } else if (missing(expr)) {
        expr=lazyeval::lazy(.data)
        .data = default_dataset()
    } else {
        expr=lazyeval::lazy(expr)
    }


    check_if_(.data,expr)
}

#' @export
check_if_ = function(.data,expr){
    UseMethod("check_if_")
    
}

#' @export
check_if_.default = function(.data,expr){
    dat = ref(.data) 
    expr = lazyeval::lazy_eval(expr,dat)
    res= list(.data=.data,cond=expr)
    class(res) = c("chk_if",class(res))
    invisible(res)
    
}

#' @export
check_if_.chk_if = function(.data,expr){
    subset = .data$subset
    old_cond = .data$cond
    .data = .data$.data
    stopif(is.null(.data),"Incorrect 'chk_if' object. No data.")
    dat = ref(.data) 
    expr = lazyeval::lazy_eval(expr,dat)
    if (!is.null(old_cond)) expr = expr & old_cond
    res= list(.data=.data,cond=expr, subset=subset)
    class(res) = c("chk_if",class(res))
    invisible(res)
    
}

#' @export
check_subset = function(.data,expr){
    if (missing(.data)){
        .data = default_dataset() 
        expr=lazyeval::lazy(expr)
    } else if (missing(expr)) {
        expr=lazyeval::lazy(.data)
        .data = default_dataset()
    } else {
        expr=lazyeval::lazy(expr)
    }
    check_subset_(.data,expr)
}

#' @export
check_subset_ = function(.data,expr){
    UseMethod("check_subset_")
    
}


#' @export
check_subset_.default = function(.data,expr){
    dat = ref(.data) 
    expr = lazyeval::lazy_eval(expr,dat)
    res= list(.data=.data,subset=expr)
    class(res) = c("chk_if",class(res))
    invisible(res)
    
}

#' @export
check_subset_.chk_if = function(.data,expr){
    old_subset = .data$subset
    cond = .data$cond
    .data = .data$.data
    stopif(is.null(.data),"Incorrect 'chk_if' object. No data.")
    dat = ref(.data) 
    expr = lazyeval::lazy_eval(expr,dat)
    if (!is.null(old_subset)) expr = expr & old_subset
    res= list(.data=.data,cond=cond, subset=expr)
    class(res) = c("chk_if",class(res))
    invisible(res)
    
}
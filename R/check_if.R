#' Functions for checking conditional questions/subset of data.
#' 
#' These functions are designed for working with piping operator 
#' \code{\link[magrittr]{\%>\%}} from 'magrittr' package.  
#' 
#' @param .data data.frame for checking 
#' @param cond Logical vector/expression. TRUE indicated rows in .data that should contain valid
#'  values. In other rows all .data values should be NA. It used for questions that
#'   were asked by condition on answer on previous questions.
#' @param subset Logical vector/expression. TRUE indicated rows in .data that should be checked. 
#' Other rows will be ignored.

#' 
#' @return These functions returns object of class 'chk_if'. It is suited for consumption by 
#' functions: \code{\link{sngl}},\code{\link{mult}},\code{\link{dmult}}, \code{\link{test}} and
#' \code{\link{move}}. Also it can be accepted by next \code{\link{check_if}} and/or 
#' \code{\link{check_subset}}.
#' 
#' @seealso \code{\link{mult}}, \code{\link{mult_}}, \code{\link{sngl}}, 
#' \code{\link{sngl_}}, \code{\link{dmult}}, \code{\link{test}} and
#' \code{\link{move}}
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
check_if = function(.data,cond){
    cond=lazyeval::lazy(cond)
    check_if_(.data,cond)
}

#' @export
check_if_ = function(.data,cond){
    UseMethod("check_if_")
    
}

#' @export
check_if_.default = function(.data,cond){
    cond = lazyeval::lazy_eval(cond,.data)
    res= list(.data=.data,cond=cond)
    class(res) = c("chk_if",class(res))
    invisible(res)
    
}

#' @export
check_if_.chk_if = function(.data,cond){
    subset = .data$subset
    old_cond = .data$cond
    .data = .data$.data
    stopif(is.null(.data),"Incorrect 'chk_if' object. No data.")
    cond = lazyeval::lazy_eval(cond,.data)
    if (!is.null(old_cond)) cond = cond & old_cond
    res= list(.data=.data,cond=cond, subset=subset)
    class(res) = c("chk_if",class(res))
    invisible(res)
    
}

#' @export
check_subset = function(.data,subset){
    subset=lazyeval::lazy(subset)
    check_subset_(.data,subset)
}

#' @export
check_subset_ = function(.data,subset){
    UseMethod("check_subset_")
    
}


#' @export
check_subset_.default = function(.data,subset){
    subset = lazyeval::lazy_eval(subset,.data)
    res= list(.data=.data,subset=subset)
    class(res) = c("chk_if",class(res))
    invisible(res)
    
}

#' @export
check_subset_.chk_if = function(.data,subset){
    old_subset = .data$subset
    cond = .data$cond
    .data = .data$.data
    stopif(is.null(.data),"Incorrect 'chk_if' object. No data.")
    subset = lazyeval::lazy_eval(subset,.data)
    if (!is.null(old_subset)) subset = subset & old_subset
    res= list(.data=.data,cond=cond, subset=subset)
    class(res) = c("chk_if",class(res))
    invisible(res)
    
}
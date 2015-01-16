#' @export
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
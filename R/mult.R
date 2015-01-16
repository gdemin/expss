#' @export
#' @rdname sngl
mult = function(...,no_dup=TRUE,show=NULL){
    
    mult_ (.dots=lazyeval::lazy_dots(...),
           no_dup=no_dup,
           show=lazyeval::lazy(show))    
    
}


#' @export
#' @rdname sngl
mult_ = function(...,.dots,no_dup=TRUE,show=NULL){
    
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
        
        res = check_internal(dfs,values=values,exclusive = exclusive,mult = TRUE,no_dup = no_dup,cond = cond,subset = subset)
        check_result(.data) = res
        invisible(.data)
    }
    invisible(values_fun)
    
}
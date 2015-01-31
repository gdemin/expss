#' @export
error_if = function(.data,expr,show=NULL){
    
    error_if_ (.data,
           expr=lazyeval::lazy(expr),
           show=lazyeval::lazy(show)
           )    
    
}


#' @export
error_if_ = function(.data,expr,show=NULL){
    
    if ("chk_if" %in% class(.data)){
        cond=.data$cond
        subset=.data$subset
        .data=.data$.data
        stopif(is.null(.data),"Incorrect 'chk_if' object. No data.")
    } else {
        cond= NULL
        subset= NULL
    }
    var_names = all.vars(expr$expr)
    vars = select_(.data,.dots = var_names)
    err_if = lazyeval::lazy_eval(expr,.data)
    if (!is.null(cond)) err_if[!(cond %in% TRUE)] = FALSE 
    if (!is.null(subset)) err_if[!(subset %in% TRUE)] = FALSE 
    if (!is.null(show$expr)) show_var = select_(.data,show$expr) else show_var = NULL

    chk_err = ifelse(err_if %in% TRUE,.ERROR_IF,NA)
    fin = setNames(data.frame(chk_err,stringsAsFactors = FALSE),.CHK_ERR)
    if (!is.null(cond)) fin[,.CHK_COND] = cond
    fin = data.frame(fin,vars,stringsAsFactors = FALSE)
    if (!is.null(show_var)) fin = data.frame(fin,show_var,stringsAsFactors = FALSE)
    class(fin) = unique(c("check",class(fin)))
    check_result(.data) = fin
    invisible(.data)    
    
    
}





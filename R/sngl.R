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


sngl = function(...,cond=NULL,subset=NULL,no_dup=FALSE,show=NULL){
    
    sngl_ (.dots=lazyeval::lazy_dots(...),
           cond=lazyeval::lazy(cond),
           subset=lazyeval::lazy(subset),
           no_dup=no_dup,
           show=lazyeval::lazy(show))    
    
}


mult = function(...,cond=NULL,subset=NULL,no_dup=TRUE,show=NULL){
    
    mult_ (.dots=lazyeval::lazy_dots(...),
           cond=lazyeval::lazy(cond),
           subset=lazyeval::lazy(subset),
           no_dup=no_dup,
           show=lazyeval::lazy(show))    
    
}

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
report = function(.data){
    chk = check_result(.data)
    if (is.null(chk)){
        warning("No result of checking")
        return(invisible(.data))
    } 
    print (chk)
    invisible(.data)
}

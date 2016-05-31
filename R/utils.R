## stop if condition with message


stopif = function(cond,...){
    if (isTRUE(cond)) {
        stop(do.call(paste0,c(list(...))),call. = FALSE)
    }
    invisible()
}


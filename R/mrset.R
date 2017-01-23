mrset = function(...){
    args = list(...)
    stopif(!length(args), "`mrset` - you should provide at least one argument.")
    if(length(args)==1){
        res = args[[1]]
        if(!is.data.frame(res)){
            res = as.dtfrm(res)
        }
    } else {
        res = as.dtfrm(args)
    }
    class(res) = union("category", class(res))
    res

}
#' @export
info=function(x, frequencies = TRUE, max_levels= 30){
    UseMethod("info")
}

#' @export
info.default=function(x, frequencies = TRUE, max_levels= 30){
    max_levels = min(1,max_levels)
    varlab=var_lab(x)
    if (length(varlab)==0) varlab=NA
    vallab=val_lab(x)
    if (length(vallab)==0) {
        vallab=NA }
    else {
        vallab=paste(paste(names(vallab),vallab,sep="="),collapse=", ")
    }
    res = list("Mode"=mode(x),Length=length(x),Valid=sum(!is.na(x)),"Label"=varlab,"Value labels"=vallab)
    if (frequencies){
        values=table(x,useNA="ifany")
        if (NA %in% names(values)) values=c(tail(values,1),head(values,-1))
        if (length(values)>max_levels) {
            other=sum(values[-(1:max_levels)])
            values=values[1:max_levels]
            values["Other values"]=other
        }
        values=paste(paste(names(values),values,sep="="),collapse=", ")
        res = c(res, Frequency =values)
    }
    res
}

#' @export
info.data.frame=function(x, frequencies = TRUE, max_levels= 30){
    info.list(x)
}

#' @export
info.list=function(x, frequencies = TRUE, max_levels= 30){
    var_names = names(x)
    res = lapply(var_names,function(y) c(y,unlist(info(x[,y]))))
    res=as.data.frame(do.call(rbind,res))
    rownames(res) = seq_len(nrow(res))                  
    res[,2]=as.numeric(res[,2])
    res[,3]=as.numeric(res[,3])
    res
}
#' @export
info=function(x, frequencies = TRUE, max_levels= 30){
    UseMethod("info")
}

#' @export
info.default=function(x, frequencies = TRUE, max_levels= 30){
    max_levels = max(1,max_levels)
    varlab=var_lab(x)
    if (length(varlab)==0) varlab=NA
    vallab=val_lab(x)
    if (length(vallab)==0) {
        vallab=NA }
    else {
        vallab=paste(paste(names(vallab),vallab,sep="="),collapse=", ")
    }
    res = list("Class"=paste(class(x),collapse=","),
               "Length"=length(x),
               "NotNA"=sum(!is.na(x)),
               "Label"=varlab,
               "ValueLabels"=vallab)
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
    curr_name = paste(as.character(substitute(x)),collapse = "")
    as.data.frame(c(Name=curr_name,res),stringsAsFactors = FALSE)
}

#' @export
info.data.frame=function(x, frequencies = TRUE, max_levels= 30){
    info.list(x,frequencies,max_levels)
}

info.matrix=function(x, frequencies = TRUE, max_levels= 30){
    info.list(as.data.frame(x,stringsAsFactors = FALSE),frequencies,max_levels)
}

#' @export
info.list=function(x, frequencies = TRUE, max_levels= 30){
    res = lapply(x,function(y) info(y,frequencies,max_levels))
    res = do.call(rbind,res)
    rownames(res) = seq_len(nrow(res))                  
    res$Name = names(x)
    res
}
#' Title
#'
#' @param ... vectors/data.frames/lists
#'
#' @return ... vector/data.frame/list
#' @export
#'
#' @examples
#' 1
nest=function(...){
    arg = list(...)
    if (length(arg)<2) return(arg)
    x = arg[[1]]
    y = arg[[2]]
    if (!is_list(x)) x = list(x) else x = flist(x)
    if (!is_list(y)) {
        res = lapply(x, function(first) {
            nest.xy(first, y)
        })
    } else {
        y=flist(y)
        res=lapply(x, function(first) {
            nest.xlist(first,y)
        })
    }
    if (length(arg)>2) res=do.call(nest,c(res,arg[-(1:2)]))
    if (length(res)==1) res=res[[1]]
    res
}

nest.xlist=function(x,y)
    # x - vector or data.frame, y - list
    # чего-то не нравится оно мне всё, слишком вычурно и громоздко... :(
{

    labs=val.lab(x)
    vlab=var.lab(x)
    uniqs=sort(unique(c(uniq_elements(x),labs)))
    xlist=lapply(uniqs,function(item){
        ((x == item) | NA)*item

    })
    res=lapply(seq_along(uniqs), function(i){
        item=uniqs[i]
        # browser()
        newlab=labs[labs == item]
        if (length(newlab)>0) val.lab(xlist[[i]])=newlab else val.lab(xlist[[i]])=NULL
        var.lab(xlist[[i]])=vlab
        lapply(y, function(item2){
            nest.xy(xlist[[i]],item2)
        })

    })
    unlist(res,recursive=FALSE)
}



nest.xy=function(x,y){
    vlabs.x=var.lab(x)
    vlabs.y=var.lab(y)
    labs.x=val.lab(x)
    labs.y=val.lab(y)
    uniq.x=sort(unique(c(uniq_elements(x),labs.x)))
    uniq.y=sort(unique(c(uniq_elements(y),labs.y)))
    encoded.x=integer.encoding(x,uniq.x)
    encoded.y=integer.encoding(y,uniq.y)
    encoded.labs.x=integer.encoding(labs.x,uniq.x)
    encoded.labs.y=integer.encoding(labs.y,uniq.y)
    max.x=length(uniq.x)
    max.y=length(uniq.y)
    aligned=align.length(list(encoded.x,encoded.y))
    x=(aligned[[1]]-1)*max.y
    y=aligned[[2]]
    values.with.labs=c(outer((encoded.labs.x-1)*max.x,encoded.labs.x,"+"))
    res=matrix(NA,nrow(x),ncol=ncol(x)*ncol(y))
    for (i in seq_len(ncol(x))) for (j in seq_len(ncol(y))){
        res[,j*i]=x[,i]+y[,j]
    }
    res=res[,colSums(is.na(res))<nrow(res)]
    new.lab.x=val_to_lab(uniq.x,labs.x)
    new.lab.y=val_to_lab(uniq.y,labs.y)
    if (!is.null(vlabs.x)) new.lab.x=paste(vlabs.x,new.lab.x,sep="|")
    if (!is.null(vlabs.y)) new.lab.y=paste(vlabs.y,new.lab.y,sep="|")
    # new.uniq=unique(c(values.with.labs,uniq_elements(res)))
    res.lab=seq_len(max.y*max.x)
    names(res.lab)=outer(new.lab.y,new.lab.x,function(x,y) paste(y,x,sep="|"))
    # res.lab=res.lab[res.lab %in% new.uniq]
    val.lab(res)=res.lab
    res
}

'%nest%'=function(x,y){
    nest(x,y)
}

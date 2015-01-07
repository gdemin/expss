#' @export
c.with_labels=function(..., recursive = FALSE)
    ### concatenate vectors of class 'with_labels' and preserve labels
{
    y = NextMethod("c")
    vectors=list(...)
    dummy= lapply(vectors,var_lab)
    dummy=dummy[!sapply(dummy,is.null)]
    if (length(dummy)>0) var_lab(y)=dummy[[1]]
    
    dummy= lapply(vectors,val_lab)
    val_lab(y)=do.call(combine_labels,dummy)
    class(y) = unique(c("with_labels",class(y)))
    y
}



#' @export
rep.with_labels=function (x, ...){
    y=NextMethod("rep")
    var_attr(y)=var_attr(x)
    class(y) = class(x)
    y	
}

#' @export
'[.with_labels'=function (x, ...){
    y = NextMethod("[")
    var_attr(y)=var_attr(x)
    class(y) = class(x)
    y
}


var_attr=function(x){
    list(label=var_lab(x),value_labels=val_lab(x))
}

'var_attr<-'=function(x,value){
    var_lab(x)=value$label
    val_lab(x)=value$value_labels
    x
}
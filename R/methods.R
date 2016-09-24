#' @export
c.labelled = function(..., recursive = FALSE)
    ### concatenate vectors of class 'labelled' and preserve labels
{
    y = NextMethod("c")
    vectors=list(...)
    dummy= lapply(vectors,var_lab)
    dummy=dummy[lengths(dummy)>0]
    if (length(dummy)>0) var_lab(y)=dummy[[1]]
    
    dummy= lapply(vectors,val_lab)
    val_lab(y)=do.call(combine_labels,dummy)
    class(y) = union("labelled",class(y))
    y
}



#' @export
rep.labelled = function (x, ...){
    y=NextMethod("rep")
    var_attr(y)=var_attr(x)
    class(y) = class(x)
    y	
}

#' @export
'[.labelled' = function (x, ...){
    y = NextMethod("[")
    var_attr(y)=var_attr(x)
    class(y) = class(x)
    y
}


var_attr = function(x){
    list(label=var_lab(x),labels=val_lab(x))
}

'var_attr<-' = function(x,value){
    var_lab(x)=value$label
    val_lab(x)=value$labels
    x
}

#' @export
"[.simple_table" = function(x, i, j, ...){
    res = `[.data.frame`(x, i, j, drop = FALSE)  
    class(res) = class(x)
    res
}




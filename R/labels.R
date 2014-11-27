
#' Set or get variable label
#' 
#' \code{var_lab} returns variable label or NULL if label doesn't 
#' exist. 
#' \code{var_lab<-} set variable label. 
#' \code{with_var_lab} returns variable with label.
#' \code{unvr} drops variable label.
#' 
#' @aliases var_lab<- with_var_lab unvr
#' @param x Variable. In the most cases it is numeric vector.
#' @param value A character scalar - label for the variable x.
#' @return \code{var_lab} return variable label. If label doesn't exist it return
#'   NULL . \code{var_lab<-} and \code{with_var_lab} return variable (vector x)
#'   of class "with_labels" with attribute "label" which equals submitted value.
#' @details Variable label is stored in attribute "label" (\code{attr(x,"label")}). For
#'   preserving from droping this attribute during some operations (such as \code{c})
#'   we set variable class to "with_labels". There are special methods of
#'   subsetting and concatenation for this class. To drop variable label use 
#'   \code{var_lab(var) <- NULL} or \code{unvr(var)}.
#' @examples
#' a = rep(1:2,5)
#' b = a
#' var_lab(a) = "Gender"
#' b = with_var_lab(b,"Gender")
#' identical(a,b) # should be TRUE
#' 
#' identical(var_lab(a),attr(a,"label")) # should be TRUE
#' 
var_lab=function(x){
    UseMethod("var_lab")
}

#'@rdname var_lab
"var_lab<-"=function(x,value){
    with_var_lab(x,value)
}

#'@rdname var_lab
with_var_lab=function(x,value){
    if (length(value)==0){
        attr(x,"label")=NULL
        return(x)
    }
    attr(x,"label")=value
    if (!isS4(x)) if (!("with_labels" %in% class(x))) class(x)=c("with_labels",class(x))
    x
}

var_lab.default=function(x){
    attr(x,"label")
}

var_lab.data.frame=function(x)
    ## Drop this function???
    ## mainly for multiple choice questions
    ## if there is no label on the data.frame itself
    ## we return label from the first variable 
{
    res=var_lab.default(x)
    if (is.null(res)){
        all_labs=lapply(x,var_lab)
        all_labs=all_labs[!sapply(all_labs,is.null)]
        if (length(all_labs)>0) res=all_labs[[1]] else res=NULL
    }
    res
}

#'@rdname var_lab
unvr=function(x){
    UseMethod("unvr")
}

unvr.default=function(x){
    with_var_lab(x,NULL)
}

unvr.data.frame=function(x){
    for (each in seq_along(x)) x[[i]] = unvr(x[[i]])
    x
}

unvr.list=function(x){
    for (each in seq_along(x)) x[[i]] = unvr(x[[i]])
    x
}


############# value labels #######################

#' Set or get value labels
#' 
#' \code{val_lab} returns value labels or NULL if labels doesn't 
#' exist. 
#' \code{val_lab<-} set value labels.
#' \code{with_val_lab} returns variable with value labels. 
#' \code{add_val_lab<-} add value labels to already existing value labels. 
#' \code{add_val_lab} return variable with added value labels to already existing value labels.
#' \code{unvl} drops value labels.
#' 
#' @aliases val_lab<- with_val_lab unvl add_val_lab add_val_lab<-
#' @param x Variable(s). Vector/data.frame/list.
#' @param value Named vector. Names of vector values are labels for the
#'   appropriate values of variable x.
#' @return \code{val_lab} return value labels (named vector). If labels doesn't
#'   exist it return NULL . \code{val_lab<-} and \code{with_val_lab} return
#'   variable (vector x) of class "with_labels" with attribute "value_labels"
#'   which contains value labels.
#' @details Value labels are stored in attribute "value_labels" 
#'   (\code{attr(x,"value_labels")}). Duplicated values are not allowed. If
#'   argument \code{x} is data.frame or list then labels applied to all elements
#'   of data.frame/list. We set variable class to "with_labels" for preserving
#'   from droping this attribute during some operations (such as \code{c}).
#'   There are special methods of subsetting and concatenation for this class.
#'   To drop value labels use \code{val_lab(var) <- NULL} or \code{unvl(var)}.
#' @examples
#' data(mtcars)
val_lab=function(x){
    UseMethod("val_lab")
}

val_lab.data.frame=function(x)
{
    res=val_lab.default(x)
    if (is.null(res)){
        all_labs=lapply(x,val_lab)
        all_labs=all_labs[!sapply(all_labs,is.null)]
        if (length(all_labs)>0) res=do.call(combine_labels,all_labs) else res=NULL
    }
    res
}

val_lab.default=function(x){
    attr(x,"value_labels")
}

#####################

"val_lab<-"=function(x,value){
    with_val_lab(x,value)
}

#####################

with_val_lab = function(x,value){
    UseMethod("with_val_lab")
}

with_val_lab.default = function(x,value){
    if (anyDuplicated(value)) stop("Duplicated values in labels: ",paste(value[duplicated(value)],collapse=" "))
    if (length(value)==0) value=NULL else value=sort(value)
    attr(x,"value_labels")=value
    if (!isS4(x)) if (!("with_labels" %in% class(x))) class(x)=c("with_labels",class(x))
    x
}


with_val_lab.data.frame = function(x,value){
    for (i in seq_along(x)) val_lab(x[[i]]) = value
    x
}

with_val_lab.list = function(x,value){
    for (i in seq_along(x)) val_lab(x[[i]]) = value
    x
}

#######

"add_val_lab<-"=function(x,value){
    add_val_lab(x,value)
}

######

add_val_lab=function(x,value){
    UseMethod("add_val_lab")
}

add_val_lab.default=function(x,value){
    if (anyDuplicated(value)) stop("Duplicated values in labels: ",paste(value[duplicated(value)],collapse=" "))
    val_lab(x) = combine_labels(value,val_lab(x))
    x
}

add_val_lab.list=function(x,value){
    for (i in seq_along(x)) add_val_lab(x[[i]])=value
    x
}

add_val_lab.data.frame=function(x,value){
    for (i in seq_along(x)) add_val_lab(x[[i]])=value
    x
}

unvl=function(x){
    with_val_lab(x,NULL)
}


#' Drop variable label and value labels
#' 
#' \code{unlab} returns variable x without variable labels and value labels
#' 
#' @param x Variable(s). Vector/data.frame/list.
#' @return \code{unlab} returns original variable x without variable label and value labels.
#' @seealso \code{link{unvr}} \code{link{unvl}}
#' @examples
#' raw_var = rep(1:2,5)
#' var_with_lab = with_var_lab(raw_var,"Income")
#' val_lab(var_with_lab) = c("Low"=1,"High"=2)
#' stopifnot(identical(raw_var,unlab(var_with_lab)))
unlab=function(x){
    UseMethod("unlab")
}

unlab.default=function(x){
    var_lab(x) = NULL
    val_lab(x) = NULL
    class(x) = class(x)[!(class(x) %in% "with_labels")]
    x
}

unlab.data.frame=function(x){
    for (each in seq_along(x)) x[[i]] = unlab(x[[i]])
    x
}

unlab.list=function(x){
    for (each in seq_along(x)) x[[i]] = unlab(x[[i]])
    x
}

########


combine_labels = function(...){
    args = list(...)
    if (length(args)==0) return(NULL) 
    new_lab = args[[1]]
    for (each in list(...)){
        if (!is.null(each)) new_lab = c(new_lab,each[!(each %in% new_lab)])
        
    }
    if (is.null(new_lab)) NULL else sort(new_lab)
}



### TODO: code.position!!!!
make_labs=function(text,code.position=c("left","right"),split="\n"){
    res=unlist(strsplit(text,split=split))
    res=res[!is.na(res)]
    res=res[res!=""]
    code=as.numeric(gsub("^([\\s\\t]*)(-*)([\\d\\.]+)([\\.\\s\\t]*)(.+)$","\\2\\3",res,perl=TRUE))
    if (!any(abs(floor(code)-code)>0)) code = as.integer(code)
    lab=gsub("^([\\s\\t]*)(-*)([\\d\\.]+)([\\.\\s\\t]*)(.+)$","\\5",res,perl=TRUE)
    structure(code,names=lab)
}


## ?????????? with_labels ? ??????

base_factor = base::factor
base_as_factor = base::as.factor

#'@export
factor = function(x = character(), levels, labels = levels, exclude = NA, ordered = is.ordered(x), nmax = NA){
    UseMethod("factor")
}

factor.default = function(...){
    base_factor(...)  
}  

factor.with_labels = function(x, exclude = NA, ordered = is.ordered(x), nmax = NA){
    vallab=val_lab(x)
    if (length(vallab)>0){
        uniqs=unique(x)
        uniqs=uniqs[!is.na(uniqs)]
        unlabeled=setdiff(uniqs,vallab)
        if (length(unlabeled)>0){
            names(unlabeled)=unlabeled
            vallab=c(vallab,unlabeled)
        }
        vallab=vallab[order(vallab)]
        res=base_factor(x,levels=vallab,labels=names(vallab),exclude=exclude,ordered=ordered,nmax=nmax)
    } else res=base_factor(x ,exclude=exclude,ordered=ordered,nmax=nmax)
    res 
    
}

## ?????????? with_labels ? ??????
as.factor.default=function(x) base_as_factor(x)

as.factor=function(x){
    UseMethod("as.factor")
}


as.factor.with_labels=function(x){
    factor.with_labels(x)
}
#' Set or get variable label
#' 
#' \code{var_lab} returns variable label or NULL if label doesn't 
#' exist. 
#' \code{var_lab<-} set variable label. 
#' \code{set_var_lab} returns variable with label.
#' \code{unvr} drops variable label.
#' 
#' @aliases var_lab<- set_var_lab unvr
#' @param x Variable. In the most cases it is numeric vector.
#' @param value A character scalar - label for the variable x.
#' @return \code{var_lab} return variable label. If label doesn't exist it return
#'   NULL . \code{var_lab<-} and \code{set_var_lab} return variable (vector x)
#'   of class "with_labels" with attribute "label" which equals submitted value.
#' @details Variable label is stored in attribute "label" (\code{attr(x,"label")}). For
#'   preserving from dropping this attribute during some operations (such as \code{c})
#'   we set variable class to "with_labels". There are special methods of
#'   subsetting and concatenation for this class. To drop variable label use 
#'   \code{var_lab(var) <- NULL} or \code{unvr(var)}.
#' @export  
#' @examples
#' data(ProductTest)
#' age_group = ProductTest$s2b
#' var_lab(ProductTest$s2b) = "Age group"
#' age_group = set_var_lab(age_group,"Age group")
#' identical(age_group,ProductTest$s2b) # should be TRUE
#' 
#' identical(var_lab(age_group),attr(age_group,"label")) # should be TRUE
#' 
var_lab=function(x){
    UseMethod("var_lab")
}

#' @rdname var_lab
#' @export
"var_lab<-"=function(x,value){
    set_var_lab(x,value)
}

#' @rdname var_lab
#' @export
set_var_lab=function(x,value){
    if (length(value)==0){
        attr(x,"label")=NULL
        return(x)
    }
    attr(x,"label")=value
    if (!isS4(x)) if (!("with_labels" %in% class(x))) class(x)=c("with_labels",class(x))
    x
}


#' @export
var_lab.default=function(x){
    attr(x,"label")
}

#' @export
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
#' @export
unvr=function(x){
    UseMethod("unvr")
}

#' @export
unvr.default=function(x){
    set_var_lab(x,NULL)
}

#' @export
unvr.data.frame=function(x){
    for (each in seq_along(x)) x[[i]] = unvr(x[[i]])
    x
}

#' @export
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
#' \code{set_val_lab} returns variable with value labels. 
#' \code{add_val_lab<-} add value labels to already existing value labels. 
#' \code{unvl} drops value labels.
#' \code{make_labels} return named vector for usage as value labels.
#' 
#' @aliases val_lab<- set_val_lab unvl add_val_lab<- make_labels
#' @param x Variable(s). Vector/data.frame/list.
#' @param value Named vector. Names of vector values are labels for the
#'   appropriate values of variable x.
#' @param add Logical. Should value labels replace old labels? Deafult is FALSE. If TRUE
#' new value lables will be combined with old value labels.
#' @param text text that should be converted to named vector
#' @param code_position Possible values "left" or "right" - position of numeric code in
#' \code{text}.
#' @return \code{val_lab} return value labels (named vector). If labels doesn't
#'   exist it return NULL . \code{val_lab<-} and \code{set_val_lab} return
#'   variable (vector x) of class "with_labels" with attribute "value_labels"
#'   which contains value labels. \code{make_labels} return named vector for usage as value labels.
#' @details Value labels are stored in attribute "value_labels" 
#'   (\code{attr(x,"value_labels")}). Duplicated values are not allowed. If
#'   argument \code{x} is data.frame or list then labels applied to all elements
#'   of data.frame/list. We set variable class to "with_labels" for preserving
#'   labels from dropping during some operations (such as \code{c} and \code{[}).
#'   There are special methods of subsetting and concatenation for this class.
#'   To drop value labels use \code{val_lab(var) <- NULL} or \code{unvl(var)}.
#'   \code{make_labels} converts text in the form that used in questionnaires 
#'   to named vector. See examples.
#' @export
#' @examples
#' data(ProductTest)
#' 
#' ### Common usage ###
#' val_lab(ProductTest$s2b) = c('18 - 26' = 2, '27 - 35' = 3)
#' 
#' head(factor(ProductTest$s2b))
#' 
#' identical(levels(factor(ProductTest$s2b)), names(val_lab(ProductTest$s2b)))
#' 
#' ProductTest = unlab(ProductTest) # drop all labels
#' 
#' ### Add labels ###
#' 
#' age_groups = c('18 - 26' = 2, '27 - 35' = 3)
#' 
#' add_val_lab(ProductTest$s2b) = age_groups[1]
#' add_val_lab(ProductTest$s2b) = age_groups[2]
#' 
#' identical(ProductTest$s2b, set_val_lab(ProductTest$s2b,age_groups))
#' 
#' ProductTest$s2b = unlab(ProductTest$s2b)
#' 
#' ## make labels from text copied from questionnaire
#' 
#' val_lab(ProductTest$s2b) = make_labels("
#'  1. 18 - 26
#'  2. 27 - 35
#' ")
#' 
#' head(factor(ProductTest$s2b))
#' 
#' # or, if in original codes is on the right side
#' 
#' val_lab(ProductTest$s8_1) = make_labels("
#'  Chocolate bars    1
#'  Chocolate sweets (bulk)	2
#'  Slab chocolate(packed)	3
#'  Slab chocolate (bulk)	4
#'  Boxed chocolate sweets	5
#'  Marshmallow/pastilles in chocolate coating	6
#'  Marmalade in chocolate coating	7
#'  Other	8
#' ", code_position = "right")
#' 
#' head(factor(ProductTest$s8_1))
val_lab=function(x){
    UseMethod("val_lab")
}

#' @export
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

#' @export
val_lab.default=function(x){
    attr(x,"value_labels")
}

#####################

#' @export
#' @rdname val_lab 
"val_lab<-"=function(x,value){
    set_val_lab(x,value,add = FALSE)
}

#####################

#' @export
#' @rdname val_lab 
set_val_lab = function(x,value,add = FALSE){
    UseMethod("set_val_lab")
}

#' @export
set_val_lab.default = function(x,value, add = FALSE){
    stopif(anyDuplicated(value),"Duplicated values in labels: ",paste(value[duplicated(value)],collapse=" "))
    if (add) value = combine_labels(value,val_lab(x))
    if (length(value)==0) value=NULL else value=sort(value)
    attr(x,"value_labels")=value
    if (!isS4(x)) if (!("with_labels" %in% class(x))) class(x)=c("with_labels",class(x))
    x
}

#' @export
set_val_lab.data.frame = function(x,value, add = FALSE){
    for (i in seq_along(x)) val_lab(x[[i]]) = value
    x
}

#' @export
set_val_lab.list = function(x,value, add = FALSE){
    for (i in seq_along(x)) val_lab(x[[i]]) = value
    x
}

#######

#' @export
#' @rdname val_lab 
"add_val_lab<-"=function(x,value){
    set_val_lab(x,value, add = TRUE)
}

######



#' @export
unvl=function(x){
    set_val_lab(x,NULL)
}


#' Drop variable label and value labels
#' 
#' \code{unlab} returns variable x without variable labels and value labels
#' 
#' @param x Variable(s). Vector/data.frame/list.
#' @return \code{unlab} returns original variable x without variable label, value labels and class.
#' @seealso \code{\link{unvr}} \code{\link{unvl}}
#' @export 
#' @examples
#' raw_var = rep(1:2,5)
#' var_with_lab = set_var_lab(raw_var,"Income")
#' val_lab(var_with_lab) = c("Low"=1,"High"=2)
#' identical(raw_var,unlab(var_with_lab)) # should be TRUE
unlab=function(x){
    UseMethod("unlab")
}

#' @export
unlab.default=function(x){
    var_lab(x) = NULL
    val_lab(x) = NULL
    class(x) = class(x)[!(class(x) %in% "with_labels")]
    x
}

#' @export
unlab.data.frame=function(x){
    for (each in seq_along(x)) x[[each]] = unlab(x[[each]])
    x
}

#' @export
unlab.list=function(x){
    for (each in seq_along(x)) x[[each]] = unlab(x[[each]])
    x
}

########



#' @export
#' @rdname val_lab
make_labels=function(text,code_position=c("left","right")){
    split="\n"
    if (length(text)>1) text = paste(text,collapse=split) 
    res = unlist(strsplit(text,split=split))
    res = res[!is.na(res)]
    res = gsub("^([\\s\\t]+)|([\\s\\t]+)$","",res,perl = TRUE)
    res = res[res!=""]
    code_position = match.arg(code_position)
    if (code_position == "left") {
        pattern = "^(-*)([\\d\\.]+)([\\.\\s\\t]*)(.+?)$"
        code_pattern = "\\1\\2"
        label_pattern = "\\4"
    } else {
        pattern = "^(.+?)([\\s\\t]+)(-*)([\\d\\.]+)$"
        code_pattern = "\\3\\4"
        label_pattern = "\\1"
        
    }
    code=as.numeric(gsub(pattern,code_pattern,res,perl=TRUE))
#     if (!any(abs(floor(code)-code)>0)) code = as.integer(code)
    lab=gsub(pattern,label_pattern,res,perl=TRUE)
    structure(code,names=lab)
}


combine_labels = function(...){
    args = list(...)
    names(args) = NULL
    if (length(args)==0) return(NULL) 
    new_lab = do.call(c,args)
    if (length(new_lab)==0) return(NULL)
    new_lab = new_lab[!duplicated(new_lab)]
    sort(new_lab)
}

#' Set or get variable label
#' 
#' This functions set/get/drop variable labels. For utilizing labels in base R 
#' see \link{f}, \link{names2labels}, \link{values2labels}, \link{unlab}. For
#' value labels see \link{val_lab}.
#' \itemize{
#' \item{\code{var_lab}}{ returns variable label or NULL if label doesn't 
#' exist.} 
#' \item{\code{var_lab<-}}{ set variable label.} 
#' \item{\code{set_var_lab}}{ returns variable with label.}
#' \item{\code{unvr}}{ drops variable label.} 
#' }
#' @param x Variable. In the most cases it is numeric vector.
#' @param value A character scalar - label for the variable x.
#' @return \code{var_lab} return variable label. If label doesn't exist it return
#'   NULL . \code{var_lab<-} and \code{set_var_lab} return variable (vector x)
#'   of class "labelled" with attribute "label" which equals submitted value.
#' @details Variable label is stored in attribute "label" (\code{attr(x,"label")}). For
#'   preserving from dropping this attribute during some operations (such as \code{c})
#'   variable class is set to "labelled". There are special methods of
#'   subsetting and concatenation for this class. To drop variable label use 
#'   \code{var_lab(var) <- NULL} or \code{unvr(var)}.
#' @export  
#' @examples
#' data(mtcars)
#' mtcars = within(mtcars,{
#'                 var_lab(mpg) = "Miles/(US) gallon"
#'                 var_lab(cyl) = "Number of cylinders"
#'                 var_lab(disp) = "Displacement (cu.in.)"
#'                 var_lab(hp) = "Gross horsepower"
#'                 var_lab(drat) = "Rear axle ratio"
#'                 var_lab(wt) = "Weight (lb/1000)"
#'                 var_lab(qsec) = "1/4 mile time"
#'                 var_lab(vs) = "V/S"
#'                 var_lab(am) = "Transmission (0 = automatic, 1 = manual)"
#'                 var_lab(gear) = "Number of forward gears"
#'                 var_lab(carb) = "Number of carburetors"
#' })
#' 
#' # note: we exclude dependent variable 'mpg' from conversion to use its short name in formula
#' summary(lm(mpg ~ ., data = n2l(mtcars, exclude = "mpg")))
#' 
var_lab=function(x){
    UseMethod("var_lab")
}

#' @export
var_lab.default=function(x){
    attr(x,"label", exact = TRUE)
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

#' @rdname var_lab
#' @export
"var_lab<-"=function(x,value){
    set_var_lab(x,value)
}

#' @rdname var_lab
#' @export
set_var_lab=function(x,value){
    UseMethod("set_var_lab")
}

#' @export
set_var_lab.list = function(x,value){
    for (each in seq_along(x)) var_lab(x[[each]]) = value
    x
}

#' @export
set_var_lab.data.frame = function(x,value){
    for (each in seq_along(x)) var_lab(x[[each]]) = value
    x
}


#' @export
set_var_lab.default = function(x,value){
    if (length(value)==0){
        attr(x,"label")=NULL
        return(x)
    }
    attr(x,"label")=value
    class(x)=union("labelled",class(x))
    x
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
    for (each in seq_along(x)) x[[each]] = unvr(x[[each]])
    x
}

#' @export
unvr.list=function(x){
    for (each in seq_along(x)) x[[each]] = unvr(x[[each]])
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
#'   variable (vector x) of class "labelled" with attribute "labels"
#'   which contains value labels. \code{make_labels} return named vector for usage as value labels.
#' @details Value labels are stored in attribute "labels" 
#'   (\code{attr(x,"labels")}). Duplicated values are not allowed. If
#'   argument \code{x} is data.frame or list then labels applied to all elements
#'   of data.frame/list. We set variable class to "labelled" for preserving
#'   labels from dropping during some operations (such as \code{c} and \code{[}).
#'   There are special methods of subsetting and concatenation for this class.
#'   To drop value labels use \code{val_lab(var) <- NULL} or \code{unvl(var)}.
#'   \code{make_labels} converts text in the form that used in questionnaires 
#'   to named vector. See examples.
#' @export
#' @examples
#' test_ds = data.frame(total = 1, 
#'                      s2b = sample(2:3,100,replace = TRUE),
#'                      s8_1 = sample(1:8,100,replace = TRUE))
#' 
#' ### Common usage ###
#' val_lab(test_ds$s2b) = c('18 - 26' = 2, '27 - 35' = 3)
#' 
#' head(factor(test_ds$s2b))
#' 
#' identical(levels(factor(test_ds$s2b)), names(val_lab(test_ds$s2b)))
#' 
#' test_ds = unlab(test_ds) # drop all labels
#' 
#' ### Add labels ###
#' 
#' age_groups = c('18 - 26' = 2, '27 - 35' = 3)
#' 
#' add_val_lab(test_ds$s2b) = age_groups[1]
#' add_val_lab(test_ds$s2b) = age_groups[2]
#' 
#' identical(test_ds$s2b, set_val_lab(test_ds$s2b,age_groups))
#' 
#' test_ds$s2b = unlab(test_ds$s2b)
#' 
#' ## make labels from text copied from questionnaire
#' 
#' val_lab(test_ds$s2b) = make_labels("
#'  1. 18 - 26
#'  2. 27 - 35
#' ")
#' 
#' head(factor(test_ds$s2b))
#' 
#' # or, if in original codes is on the right side
#' 
#' val_lab(test_ds$s8_1) = make_labels("
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
#' head(factor(test_ds$s8_1))
val_lab=function(x){
    UseMethod("val_lab")
}

#' @export
val_lab.data.frame=function(x)
{
    
    # TODO пересмотреть - слишком много ума тут
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
    attr(x,"labels", exact = TRUE)
}

#####################

#' @export
#' @rdname val_lab 
"val_lab<-"=function(x, value){
    set_val_lab(x, value, add = FALSE)
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
    attr(x,"labels")=value
    class(x)=union("labelled",class(x))
    x
}

#' @export
set_val_lab.data.frame = function(x,value, add = FALSE){
    for (each in seq_along(x)) x[[each]] = set_val_lab(x[[each]], value, add = add)
    x
}

#' @export
set_val_lab.list = function(x,value, add = FALSE){
    for (each in seq_along(x)) x[[each]] = set_val_lab(x[[each]], value, add = add)
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
    class(x) = setdiff(class(x),"labelled")
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
    new_lab = Reduce(`%u%`, args)
    if(length(new_lab)>0) sort(new_lab) else new_lab
}

labelled_and_unlabelled = function(uniqs,vallab){
    if (length(uniqs)>0) {
        uniqs=uniqs[!is.na(uniqs)]
        names(uniqs) = uniqs
    }    
    vallab = vallab %u% uniqs
    if (length(vallab)>1) sort(vallab) else vallab
}

as.labelled = function(x){
    UseMethod("as.labelled")
}

as.labelled.default = function(x){
    x
}

as.labelled.factor = function(x){
    labels = seq_along(levels(x))
    names(labels) = levels(x)
    x = as.numeric(x)
    val_lab(x) = labels
    x
    
}

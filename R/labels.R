#' Set or get variable label
#' 
#' These functions set/get/drop variable labels. For 
#' value labels see \link{val_lab}. For working with entire data.frame see
#' \link{apply_labels}.
#' \itemize{
#' \item{\code{var_lab}}{ returns variable label or NULL if label doesn't 
#' exist.} 
#' \item{\code{var_lab<-}}{ set variable label.} 
#' \item{\code{set_var_lab}}{ returns variable with label.}
#' \item{\code{unvr}}{ drops variable label.} 
#' \item{\code{add_labelled_class}}{ Add missing 'labelled' class. This function
#' is needed when you load SPSS data with packages which in some cases don't set
#' 'labelled' class for variables with labels. For example, \code{haven} package
#' doesn't set 'labelled' class for variables which have variable label but
#' don't have value labels. Note that to use 'expss' with 'haven' you need to
#' load 'expss' strictly after 'haven' to avoid conflicts.} }
#' @param x Variable. In the most cases it is numeric vector.
#' @param value A character scalar - label for the variable x.
#' @param remove_classes A character vector of classes which should be removed
#'   from the class attribute of the \code{x}.
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
#' mtcars = modify(mtcars,{
#'                 var_lab(mpg) = "Miles/(US) gallon"
#'                 var_lab(cyl) = "Number of cylinders"
#'                 var_lab(disp) = "Displacement (cu.in.)"
#'                 var_lab(hp) = "Gross horsepower"
#'                 var_lab(drat) = "Rear axle ratio"
#'                 var_lab(wt) = "Weight (lb/1000)"
#'                 var_lab(qsec) = "1/4 mile time"
#'                 var_lab(vs) = "V/S"
#'                 var_lab(am) = "Transmission (0 = automatic, 1 = manual)"
#'                 val_lab(am) = c(automatic = 0, manual=1)
#'                 var_lab(gear) = "Number of forward gears"
#'                 var_lab(carb) = "Number of carburetors"
#' })
#' 
#' fre(mtcars$am)
#' 
#' calculate(mtcars, 
#'      cro_mean(list(mpg, disp, hp, qsec), list(total(), am))
#'      ) 
#' 
#'  
#' \dontrun{
#' # 'add_labelled_class' example doesn't work intentionally
#' if(FALSE){ # to prevent execution
#' # you need to load packages strictly in this order to avoid conflicts
#' library(haven)
#' library(expss)
#' spss_data = haven::read_spss("spss_file.sav")
#' # add missing 'labelled' class
#' spss_data = add_labelled_class(spss_data) 
#' }
#' }
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
    ## we return first label 
{
    all_labs=lapply(x,var_lab)
    all_labs=all_labs[!sapply(all_labs,is.null)]
    if (length(all_labs)>0) res=all_labs[[1]] else res=NULL
    res
}

#' @rdname var_lab
#' @export
"var_lab<-"=function(x,value){
    set_var_lab(x, value)
}

#' @rdname var_lab
#' @export
set_var_lab=function(x,value){
    UseMethod("set_var_lab")
}

#' @export
set_var_lab.list = function(x,value){
    for (each in seq_along(x)) x[[each]] = set_var_lab(x[[each]], value)
    x
}

#' @export
set_var_lab.data.frame = function(x, value){
    for (each in seq_along(x)) x[[each]] = set_var_lab(x[[each]], value)
    x
}



#' @export
set_var_lab.default = function(x, value){
    if(is.list(x)){
        return(set_var_lab.list(x, value = value))
    }
    if (length(value)==0){
        attr(x,"label")=NULL
        if(length(val_lab(x))==0){
            class(x)=setdiff(class(x), c("labelled", "spss_labelled", "haven_labelled"))
        }
        return(x)
    }
    # this conversion is needed to avoid strange bug (incorrect residuals)
    # with 'lm' with labelled integers
    # if(is.integer(x)) x[] = as.double(x)
    value = as.character(value)
    length(value)==1 || stop("'set_var_lab' - label should be vector of length 1.")
    attr(x, "label")=value
    class(x)=union("labelled", class(x))
    x
}



#'@rdname var_lab
#' @export
unvr=function(x){
    UseMethod("unvr")
}

#' @export
unvr.default=function(x){
    set_var_lab(x, NULL)
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

#'@rdname var_lab
#' @export
drop_var_labs = unvr

#### add_labelled_class
#' @rdname var_lab
#' @export
add_labelled_class = function(x, remove_classes = c("haven_labelled", "spss_labelled")){
    UseMethod("add_labelled_class")
}

#' @export
add_labelled_class.default = function(x, remove_classes = c("haven_labelled", "spss_labelled")){
    x = remove_class(x, remove_classes)
    if((!is.null(var_lab(x)) || !is.null(val_lab(x))) && !inherits(x, "labelled")){
        x = add_class(x, "labelled")
    }
    x
}

#' @export
add_labelled_class.list = function(x, remove_classes = c("haven_labelled", "spss_labelled")){
    for(i in seq_along(x)){
        x[[i]] =  add_labelled_class(x[[i]], remove_classes = remove_classes)
    }
    x
}

#' @export
add_labelled_class.data.frame = add_labelled_class.list 
############# value labels #######################

#' Set or get value labels
#' 
#' These functions set/get/drop value labels. Duplicated values are not allowed.
#' If argument \code{x} is data.frame or list then labels applied to all 
#' elements of data.frame/list. To drop value labels, use \code{val_lab(var) <- 
#' NULL} or \code{unvl(var)}. \code{make_labels} converts text from the form 
#' that usually used in questionnaires to named vector. For variable labels see
#' \link{var_lab}. For working with entire data.frame see \link{apply_labels}.
#' \itemize{
#' \item{\code{val_lab}}{ returns value labels or NULL if labels doesn't 
#' exist.} 
#' \item{\code{val_lab<-}}{ set value labels.}
#' \item{\code{set_val_lab}}{ returns variable with value labels.} 
#' \item{\code{add_val_lab<-}}{ add value labels to already existing value labels.} 
#' \item{\code{unvl}}{ drops value labels.}
#' \item{\code{make_labels}}{ makes named vector from text for usage as value labels.}
#' \item{\code{num_lab}, \code{lab_num} and  \code{autonum}}{ are shortcuts for \code{make_labels}
#' with \code{code_postion} 'left', 'right' and 'autonum' accordingly.}
#' }
#' @param x Variable(s). Vector/data.frame/list.
#' @param value Named vector. Names of vector are labels for the
#'   appropriate values of variable x.
#' @param add Logical. Should we add value labels to old labels or replace it?
#'   Deafult is FALSE - we completely replace old values. If TRUE new value
#'   labels will be combined with old value labels.
#' @param text text that should be converted to named vector
#' @param code_position Possible values "left", "right" - position of numeric code in
#' \code{text}. "autonum" - makes codes by autonumbering lines of \code{text}.
#' @return \code{val_lab} return value labels (named vector). If labels doesn't 
#'   exist it return NULL . \code{val_lab<-} and \code{set_val_lab} return 
#'   variable (vector x) of class "labelled" with attribute "labels" which
#'   contains value labels. \code{make_labels} return named vector for usage as
#'   value labels.
#' @details Value labels are stored in attribute "labels" 
#'   (\code{attr(x,"labels")}). We set variable class to "labelled" for preserving
#'   labels from dropping during some operations (such as \code{c} and \code{`[`}).
#' @export
#' @examples
#' # toy example
#' set.seed(123)
#' # score - evaluation of tested product
#' 
#' score = sample(-1:1,20,replace = TRUE)
#' var_lab(score) = "Evaluation of tested brand"
#' val_lab(score) = c("Dislike it" = -1,
#'                    "So-so" = 0,
#'                    "Like it" = 1    
#'                    )
#' 
#' # frequency of product scores                                      
#' fre(score)
#' 
#' # brands - multiple response question
#' # Which brands do you use during last three months? 
#' 
#' brands = as.sheet(t(replicate(20,sample(c(1:5,NA),4,replace = FALSE))))
#'
#' var_lab(brands) = "Used brands"
#' val_lab(brands) = make_labels("
#'                               1 Brand A
#'                               2 Brand B
#'                               3 Brand C
#'                               4 Brand D
#'                               5 Brand E
#'                               ")
#' 
#' 
#' # percentage of used brands
#' fre(brands)
#' 
#' # percentage of brands within each score
#' cro_cpct(brands, score)
#' 
#' ## make labels from text copied from questionnaire
#' 
#' age = c(1, 2, 1, 2)
#' 
#' val_lab(age) = num_lab("
#'  1. 18 - 26
#'  2. 27 - 35
#' ")
#' 
#' # note support of value lables in base R
#' table(age)
#' 
#' # or, if in original codes is on the right side
#' 
#' products = 1:8
#' 
#' val_lab(products) = lab_num("
#'  Chocolate bars    1
#'  Chocolate sweets (bulk)	2
#'  Slab chocolate(packed)	3
#'  Slab chocolate (bulk)	4
#'  Boxed chocolate sweets	5
#'  Marshmallow/pastilles in chocolate coating	6
#'  Marmalade in chocolate coating	7
#'  Other	8
#' ")
#' 
#' table(products)
val_lab=function(x){
    UseMethod("val_lab")
}

#' @export
val_lab.data.frame=function(x)
{
    
    # we consider data.frame as multiple response question
    all_labs=lapply(x, val_lab)
    all_labs=all_labs[lengths(all_labs)>0]
    if (length(all_labs)>0) res=do.call(combine_labels, all_labs) else res=NULL
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
set_val_lab = function(x, value, add = FALSE){
    UseMethod("set_val_lab")
}

#' @export
#' @rdname val_lab
add_val_lab = function(x, value) set_val_lab(x, value, add = TRUE) 

#' @export
set_val_lab.default = function(x, value, add = FALSE){
     if(is.list(x)){
        return(set_val_lab.list(x, value = value, add = add))
     }
     if (length(value)==0){
        if(!add){
            attr(x, "labels") = NULL
        }
        if(length(val_lab(x)) == 0 && is.null(var_lab(x))){
            class(x)=setdiff(class(x), c("labelled", "spss_labelled", "haven_labelled"))
        }
        return(x)
     }
    if(is.factor(x)){
        label = var_lab(x)
        x = as.character(x)
        if(!is.null(label)) var_lab(x) = label
        warning("You are trying to put value labels on factor. It can lead to unexpected results. Factor will be converted to character.")
    }
    !is.null(names(value)) || stop("'set_val_lab' - labels should be named vector.")
    !anyDuplicated(value)  || stop("'set_val_lab' - duplicated values in labels: ",paste(value[duplicated(value)],collapse=" "))
    
    # this conversion is needed to avoid strange bug (incorrect residuals)
    # with 'lm' with labelled integers 
    # if(is.integer(x)) x[] = as.double(x)
    if (add) value = combine_labels(value,val_lab(x))
 
    # Warning about dupliction was removed because it was generated too often for third party *.sav files.
    #    with_warning = "duplicated labels: "
    names(value) = make_items_unique(names(value))
    value = sort(value)
    attr(x, "labels")=value
    class(x)=union("labelled", class(x))
    x
}

#' @export
set_val_lab.data.frame = function(x, value, add = FALSE){
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
#' @rdname val_lab
unvl=function(x){
    set_val_lab(x,NULL)
}

#' @export
#' @rdname val_lab
drop_val_labs = unvl

#' @export
#' @rdname val_lab
make_labels=function(text, code_position=c("left","right", "autonum")){
    split="\n"
    if (length(text)>1) text = paste(text,collapse=split) 
    res = unlist(strsplit(text,split=split))
    res = res[!is.na(res)]
    res = gsub("^([\\s\\t]+)|([\\s\\t]+)$","",res,perl = TRUE)
    res = res[res!=""]
    code_position = match.arg(code_position)
    if(code_position %in% c("left", "right")){
        if (code_position == "left") {
            pattern = "^(-*)([\\d\\.]+)([\\.\\s\\t]*)(.*?)$"
            code_pattern = "\\1\\2"
            label_pattern = "\\4"
        } else {
            pattern = "^(.*?)([\\s\\t]*)(-*)([\\d\\.]+)$"
            code_pattern = "\\3\\4"
            label_pattern = "\\1"
            
        }
        all(grepl(pattern, res, perl=TRUE)) || stop("'make_labels' - incorrect pattern for labels:\n", paste(res[!grepl(pattern, res,perl=TRUE)], collapse = "\n"))
        code=as.numeric(gsub(pattern,code_pattern,res,perl=TRUE))
        #     if (!any(abs(floor(code)-code)>0)) code = as.integer(code)
        lab=gsub(pattern,label_pattern,res,perl=TRUE)
        code = code[!(lab %in% "")]
        lab = lab[!(lab %in% "")]
    } else {
        lab = res
        lab = lab[!(lab %in% "")]
        code = seq_along(res)
    }
    
    if(length(lab)>0){
        structure(code,names=lab)
    } else {
        NULL
    }   
}

##################

#' @export
#' @rdname val_lab
drop_unused_labels = function(x){
    UseMethod("drop_unused_labels")
}

#' @export
drop_unused_labels.default = function(x){
    curr_labs = val_lab(x)
    if(is.null(curr_labs)) return(x)
    curr_values = uniq_elements(x)
    set_val_lab(x, curr_labs[curr_labs %in% curr_values])
}

#' @export
drop_unused_labels.category = function(x){
    curr_labs = val_lab(x)
    if(is.null(curr_labs)) return(x)
    curr_values = uniq_elements(x)
    set_val_lab(x, curr_labs[curr_labs %in% curr_values])
}

#' @export
drop_unused_labels.list = function(x){
    lapply(x, drop_unused_labels)
}

#' @export
drop_unused_labels.data.frame = function(x){
    x[] = lapply(x, drop_unused_labels)
    x
}


##########################    

#' @export
#' @rdname val_lab
num_lab = function(text) make_labels(text = text, code_position = "left")
#' @export
#' @rdname val_lab
lab_num = function(text) make_labels(text = text, code_position = "right")
#' @export
#' @rdname val_lab
autonum = function(text) make_labels(text = text, code_position = "autonum")

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
    if(is.null(x)) return(x)
    if(is.list(x)){
        return(unlab.list(x))
    }
    attr(x, "label") = NULL
    attr(x, "labels") = NULL
    class(x) = setdiff(class(x), c("labelled", "labelled_spss"))
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

#' @export
#' @rdname unlab
drop_all_labels = unlab

########







#' Recode vector into numeric vector with value labels 
#'
#' @param x numeric vector/character vector/factor 
#' @param label optional variable label
#'
#' @return numeric vector with value labels
#' @export
#' @examples
#' character_vector = c("one", "two",  "two", "three")
#' as.labelled(character_vector, label = "Numbers")
#' 
#'
as.labelled = function(x, label = NULL){
    UseMethod("as.labelled")
}

#' @export
as.labelled.default = function(x, label = NULL){
    labels = sort(unique(x), na.last = NA)
    values = seq_along(labels)
    res = fast_match(x, labels)
    names(values) = as.character(labels)
    val_lab(res) = values
    var_lab(res) = label
    res
}

#' @export
as.labelled.factor = function(x, label = NULL){
    values = seq_along(levels(x))
    names(values) = levels(x)
    x = as.numeric(x)
    val_lab(x) = values
    var_lab(x) = label
    x
    
}


#' @export
as.labelled.labelled = function(x, label = NULL){
    vallab = val_lab(x)
    label = c(label, var_lab(x))[1]
    if(length(vallab)>0){
        # we need to add labels if some values don't have labels
        values = unlab(unique(x))
        values = structure(values, names = values)
        vallab =  v_union(vallab, values)
        vallab = sort(vallab, na.last = NA)
        if(!is.numeric(x)){
            # and we need to convert 'x' to numeric if it is not numeric
            x = fast_match(x, vallab) 
            vallab = structure(seq_along(vallab), names = names(vallab))
        }
        val_lab(x) = vallab
        var_lab(x) = label    
    } else {
        x = as.labelled(unlab(x), label = label)
    }
    x
}

#' @export
as.labelled.data.frame = function(x, label = NULL){
    stop("'as.labelled': labelled 'data.frame' is not implemented.")
}

#' @export
as.labelled.list = function(x, label = NULL){
    stop("'as.labelled': labelled 'list' is not implemented.")
}




#' @export
#' @rdname as.labelled
is.labelled = function(x){
    inherits(x, "labelled")
}

################

combine_labels = function(...){
    args = list(...)
    new_lab = Reduce(`%u%`, args)
    if(length(new_lab)>0) sort(new_lab) else new_lab
}

labelled_and_unlabelled = function(uniqs, vallab){
    uniqs = unlab(uniqs)
    if (length(uniqs)>0) {
        uniqs=uniqs[!is.na(uniqs)]
        names(uniqs) = uniqs
    }
    vallab = vallab %u% uniqs
    if (length(vallab)>1) sort(vallab) else vallab
}

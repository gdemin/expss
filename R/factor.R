base_factor = base::factor
base_as_factor = base::as.factor
labels_sep = "|"

# These functions override the set functions provided in base to make them
# generic so that efficient versions for data frames and other tables can be
# provided. The default methods call the base versions.

#'@export
factor = function(x = character(), levels, labels = levels, exclude = NA, ordered = is.ordered(x), nmax = NA){
    UseMethod("factor")
}

#'@export
factor.default = function(...){
    base_factor(...)  
}  

#'@export
factor.with_labels = function(x, exclude = NA, ordered = is.ordered(x), nmax = NA){
    vallab=val_lab(x)
    varlab = var_lab(x)
#     browser()
#     if (length(vallab)>0){
        uniqs=unique(x)
        uniqs=uniqs[!is.na(uniqs)]
        unlabeled=setdiff(uniqs,vallab)
        if (length(unlabeled)>0){
            names(unlabeled)=unlabeled
            vallab=c(vallab,unlabeled)
        }
        vallab=sort(vallab)

        if (!is.null(varlab) && (varlab!="")) names(vallab) = paste(varlab,names(vallab),sep = labels_sep)
        res=base_factor(x,levels=vallab,labels=names(vallab),exclude=exclude,ordered=ordered,nmax=nmax)
#     } else res=base_factor(x ,exclude=exclude,ordered=ordered,nmax=nmax)
    res 
    
}


#' @export
as.factor=function(x){
    UseMethod("as.factor")
}


#'@export
as.factor.default=function(x) base_as_factor(x)



#'@export
as.factor.with_labels=function(x){
    factor.with_labels(x)
}
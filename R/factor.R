base_factor = base::factor
base_as_factor = base::as.factor
base_ordered = base::ordered
base_as_ordered = base::as.ordered
labels_sep = "|"



#' Factors
#' 
#' These functions override the same functions provided in base to make them
#' generic so that methods for variables with labels can be
#' provided. The default methods call the base versions.
#'
#' @param x a vector of data, usually taking a small number of distinct values.
#' @param levels an optional vector of the values (as character strings) that x might have taken. The default is the unique set of values taken by as.character(x), sorted into increasing order of x. Note that this set can be specified as smaller than sort(unique(x)).
#' @param labels either an optional character vector of labels for the levels (in the same order as levels after removing those in exclude), or a character string of length 1.
#' @param exclude a vector of values to be excluded when forming the set of levels. This should be of the same type as x, and will be coerced if necessary.
#' @param ordered	logical flag to determine if the levels should be regarded as ordered (in the order given).
#' @param nmax an upper bound on the number of levels.
#' @param ...	(in ordered(.)): any of the above, apart from ordered itself.
#' 
#' @return \code{factor}, \code{as.factor}, \code{ordered}, \code{as.ordered} 
#' return an object of class factor. For details see base \code{factor} documentation.
#' @details For variables with labels arguments \code{levels}, \code{labels}, \code{exclude},
#'  \code{nmax} are ignored. Factor levels are constructed as values from values labels + variable values  
#'  without labels (so there is no information lost). Labels are constructed as "Variable_label|Value label".
#'  
#' @seealso Materials for base functions: \code{\link[base]{factor}}, \code{\link[base]{as.factor}}, 
#'  \code{\link[base]{ordered}}, \code{\link[base]{as.ordered}}
#' @export  
#' @examples
#' data(mtcars)
#' 
#' var_lab(mtcars$am) = "Transmission"
#' val_lab(mtcars$am) = c(automatic = 0, manual=1)
#' 
#' \dontrun{
#' plot(factor(mtcars$am))
#' }
#' 
#' summary(lm(mpg ~ am, data = mtcars)) # no labels  
#' summary(lm(mpg ~ factor(am), data = mtcars)) # with labels 
#' summary(lm(mpg ~ factor(unvr(am)), data = mtcars)) # without variable label 
factor = function(x = character(), levels, labels = levels, exclude = NA, ordered = is.ordered(x), nmax = NA){
    UseMethod("factor")
}

#' @export
factor.default = function(...){
    base_factor(...)  
}  

#' @export
factor.with_labels = function(x,  ordered = is.ordered(x),...){
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
        res=base_factor(x,levels=vallab,labels=names(vallab),ordered=ordered)
#     } else res=base_factor(x ,exclude=exclude,ordered=ordered,nmax=nmax)
    res 
    
}


#' @export
#' @rdname factor
as.factor=function(x){
    UseMethod("as.factor")
}


#' @export
as.factor.default=function(x) base_as_factor(x)



#' @export
as.factor.with_labels=function(x){
    factor.with_labels(x)
}

#' @export
#' @rdname factor
ordered = function (x, ...) {
    labelr::factor(x, ..., ordered = TRUE)
}    

#' @export
#' @rdname factor
as.ordered = function (x) {
    if (is.ordered(x)) x else labelr::ordered(x)
}


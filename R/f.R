base_factor = base::factor
base_as_factor = base::as.factor
base_ordered = base::ordered
base_as_ordered = base::as.ordered
labels_sep = "|"



#' Convert labelled variable to factor
#' 
#' This function provide the same functionality as original \code{\link[base]{factor}} function.
#' But it is generic so that methods for variables with labels can be
#' provided. The default method call the base 'factor' version.
#'
#' @param x a vector of data, usually taking a small number of distinct values.
#' @param levels an optional vector of the values (as character strings) that x might have taken. The default is the unique set of values taken by as.character(x), sorted into increasing order of x. Note that this set can be specified as smaller than sort(unique(x)).
#' @param labels either an optional character vector of labels for the levels (in the same order as levels after removing those in exclude), or a character string of length 1.
#' @param exclude a vector of values to be excluded when forming the set of levels. This should be of the same type as x, and will be coerced if necessary.
#' @param ordered	logical flag to determine if the levels should be regarded as ordered (in the order given).
#' @param nmax an upper bound on the number of levels.
#' @param ...	(in ordered(.)): any of the above, apart from ordered itself.
#' 
#' @return \code{f}, return an object of class factor. For details see base \code{factor} documentation.
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
#' plot(f(mtcars$am))
#' }
#' 
#' summary(lm(mpg ~ am, data = mtcars)) # no labels  
#' summary(lm(mpg ~ f(am), data = mtcars)) # with labels 
#' summary(lm(mpg ~ f(unvr(am)), data = mtcars)) # without variable label 
f = function(x = character(), levels, labels = levels, exclude = NA, ordered = is.ordered(x), nmax = NA){
    UseMethod("f")
}

#' @export
f.default = function(...){
    base_factor(...)  
}  

#' @export
f.labelled = function(x,  ordered = is.ordered(x),...){
    vallab=val_lab(x)
    varlab = var_lab(x)
    uniqs=unique(x)
    vallab = labelled_and_unlabelled(uniqs,vallab) 
    if (!is.null(varlab) && (varlab!="")) names(vallab) = paste(varlab,names(vallab),sep = labels_sep)
    res=base_factor(x,levels=vallab,labels=names(vallab),ordered=ordered, ...)
    res 
    
}



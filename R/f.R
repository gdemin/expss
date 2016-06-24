base_factor = base::factor
base_as_factor = base::as.factor
base_ordered = base::ordered
base_as_ordered = base::as.ordered
LABELS_SEP = "|"

#' Convert labelled variable to factor
#' 
#' \code{f} converts labelled variable to factor. Factor levels are constructed
#' as values labels. If label doesn't exist for particular value then this value
#' remain as is - so there is no information lost. This levels look like as
#' "Variable_label|Value label". If variable doesn't have labels then usual
#' \code{factor} will be applied.
#' 
#' @param x a vector of data with labels.
#' @param ... optional arguments for \code{\link[base]{factor}} 
#' @return an object of class factor. For details see base \code{factor} documentation.
#'  
#' @seealso \link{values2labels}, \link{names2labels}, \link{val_lab}, 
#'   \link{var_lab}. Materials for base functions: \code{\link[base]{factor}},
#'   \code{\link[base]{as.factor}}, \code{\link[base]{ordered}},
#'   \code{\link[base]{as.ordered}}
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
#' table(f(mtcars$am))
#' 
#' summary(lm(mpg ~ am, data = mtcars)) # no labels  
#' summary(lm(mpg ~ f(am), data = mtcars)) # with labels 
#' summary(lm(mpg ~ f(unvr(am)), data = mtcars)) # without variable label 
f = function(x, ...){
    UseMethod("f")
}

#' @export
f.default = function(x, ...){
    base_factor(x = x, ...)  
}  

#' @export
f.labelled = function(x, ...){
    vallab=val_lab(x)
    varlab = var_lab(x)
    uniqs=unique(x)
    vallab = labelled_and_unlabelled(uniqs,vallab) 
    if (!is.null(varlab) && (varlab!="")) names(vallab) = paste(varlab,names(vallab),sep = LABELS_SEP)
    res=base_factor(x = x, levels=vallab, labels=names(vallab), ...)
    res 
    
}



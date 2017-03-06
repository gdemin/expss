LABELS_SEP = "|"

#' Convert labelled variable to factor
#' 
#' \code{to_fac} converts variable to factor. It force labels usage as factor
#' labels for labelled variables to factors even if
#' 'expss.enable_value_labels_support' set to 0. For other types of variables
#' base \link[base]{factor} is called. Factor levels are constructed as values
#' labels. If label doesn't exist for particular value then this value remain as
#' is - so there is no information lost. This levels look like as
#' "Variable_label|Value label" if argument \code{prepend} set to TRUE. 
#' 
#' @param x a vector of data with labels.
#' @param ... optional arguments for \code{\link[base]{factor}} 
#' @param drop_unused_labels logical. Should we drop unused value labels?
#'   Default is FALSE.
#' @param prepend_var_lab logical. Should we prepend variable label before value
#'   labels? Default is TRUE.
#' @return an object of class factor. For details see base \link[base]{factor} documentation.
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
#' plot(to_fac(mtcars$am))
#' }
#' 
#' table(to_fac(mtcars$am))
#' 
#' summary(lm(mpg ~ am, data = mtcars)) # no labels  
#' summary(lm(mpg ~ to_fac(am), data = mtcars)) # with labels 
#' summary(lm(mpg ~ to_fac(unvr(am)), data = mtcars)) # without variable label 
to_fac = function(x, ..., drop_unused_labels = FALSE, prepend_var_lab = TRUE){
    UseMethod("to_fac")
}

#' @export
to_fac.default = function(x, ..., drop_unused = FALSE, prepend_var_lab = TRUE){
    base::factor(x = x, ...)  
}  

#' @export
to_fac.factor = function(x, ..., drop_unused = FALSE, prepend_var_lab = TRUE){
    if(drop_unused) {
        base::factor(x = x, ...)  
    } else {
        x
    }    
}  

#' @export
to_fac.labelled = function(x, ..., drop_unused = FALSE, prepend_var_lab = TRUE){
    x = as.labelled(x) # if we have only variable label 
    vallab = val_lab(x)
    varlab = var_lab(x)
    x = unlab(x)
    stopif(anyDuplicated(vallab),"duplicated values in labels: ",paste(vallab[duplicated(vallab)],collapse=" "))
    names_vallab = names(vallab)
    if (anyDuplicated(names_vallab)){
        duplicates = duplicated(names_vallab)
        warning(paste0("duplicated labels: ", paste(names_vallab[duplicates], collapse = ",")))
        names(vallab)[duplicates] = paste0(names_vallab[duplicates], seq_len(sum(duplicates)))
    }
    uniqs=unique(x)
    vallab = labelled_and_unlabelled(uniqs,vallab) 
    if(drop_unused){
        vallab = v_intersect(vallab, uniqs) 
    }
    vallab = sort(vallab)
    if (!is.null(varlab) && (varlab!="") && prepend_var_lab) {
        names(vallab) = paste(varlab, names(vallab), sep = LABELS_SEP)
    }    
    ### premature optimization
    if(is.numeric(x) && !is.object(x)){
        ordered = if_null(list(...)$ordered, FALSE)
        res = match(x, vallab)
        levels(res) = names(vallab)
        class(res) = c(if (ordered) "ordered", "factor")
        res     
    } else {
        base::factor(x = x, levels = as.character(vallab), labels=names(vallab), ...)
    }
}



LABELS_SEP = "|"

#' Convert labelled variable to factor
#' 
#' \code{fctr} converts variable to factor. It force labels usage as factor 
#' labels for labelled variables even if 'expss.enable_value_labels_support' set
#' to 0. For other types of variables base \link[base]{factor} is called. Factor
#' levels are constructed as values labels. If label doesn't exist for
#' particular value then this value remain as is - so there is no information
#' lost. This levels look like as "Variable_label|Value label" if argument
#' \code{prepend} set to TRUE.
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
#' summary(lm(mpg ~ am, data = mtcars)) # no labels  
#' summary(lm(mpg ~ fctr(am), data = mtcars)) # with labels 
#' summary(lm(mpg ~ fctr(unvr(am)), data = mtcars)) # without variable label 
fctr = function(x, ..., drop_unused_labels = FALSE, prepend_var_lab = TRUE){
    UseMethod("fctr")
}

#' @export
fctr.default = function(x, ..., drop_unused_labels = FALSE, prepend_var_lab = TRUE){
    fast_factor(x = x, ...)  
}  

#' @export
fctr.factor = function(x, ..., drop_unused_labels = FALSE, prepend_var_lab = TRUE){
    if(drop_unused_labels) {
        base::factor(x = x, ...)  
    } else {
        x
    }    
}  

#' @export
fctr.labelled = function(x, ..., drop_unused_labels = FALSE, prepend_var_lab = TRUE){
    x = as.labelled(x) # if we have only variable label
    vallab = val_lab(x)
    varlab = var_lab(x)
    x = unlab(x)
    !anyDuplicated(vallab) || stop("duplicated values in labels: ",paste(vallab[duplicated(vallab)],collapse=" "))

    uniqs=unique(x)
    vallab = labelled_and_unlabelled(uniqs,vallab) 
    if(drop_unused_labels){
        vallab = v_intersect(vallab, uniqs) 
    }
    vallab = sort(vallab)
    if (!is.null(varlab) && (varlab!="") && prepend_var_lab) {
        names(vallab) = paste(varlab, names(vallab), sep = LABELS_SEP)
    }
    if(length(vallab)>1){
        names(vallab) = make_items_unique(names(vallab), with_warning = "duplicated labels: ")
    }
    ### premature optimization
    ordered = if_null(list(...)$ordered, FALSE)
    res = fast_match(x, vallab)
    levels(res) = names(vallab)
    class(res) = c(if (ordered) "ordered", "factor")
    res     

}




fast_factor = function (x = character(), levels, labels = levels, exclude = NA, 
                        ordered = is.ordered(x), nmax = NA) {
    if (is.null(x)) x = character()
    nx = names(x)
    if (missing(levels)) {
        y = unique(x, nmax = nmax)
        ind = sort.list(y)
        levels = y[ind]
    }
    force(ordered)
    # if (!is.character(x)) 
    #     x <- as.character(x)
    levels = levels[is.na(match(levels, exclude))]
    f = fast_match(x, levels)
    if (!is.null(nx)) 
        names(f) = nx
    nl = length(labels)
    nL = length(levels)
    if (!any(nl == c(1L, nL))) 
        stop(gettextf("invalid 'labels'; length %d should be 1 or %d", 
                      nl, nL), domain = NA)
    levels(f) = if (nl == nL) 
        as.character(labels)
    else paste0(labels, seq_along(levels))
    class(f) <- c(if (ordered) "ordered", "factor")
    f
}
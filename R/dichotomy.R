
#' @export
dichotomy = function(x, keep_unused = FALSE, keep_na = TRUE){
    UseMethod("dichotomy")    
}

#' @export
dichotomy.default = function(x, keep_unused = FALSE, keep_na = TRUE){
    vallab=val_lab(x)
    varlab = var_lab(x)
    uniqs=unique(c(x,recursive = TRUE))
    uniqs = uniqs[!is.na(uniqs)]
    vallab = labeled_and_unlabeled(uniqs,vallab)
    if (keep_unused) {
        uniqs = vallab
    } else {
        vallab = vallab[vallab %in% uniqs]        
    }   
    if (!is.null(varlab) && (varlab!="")) names(vallab) = paste(varlab,names(vallab),sep = labels_sep)
    res = matrix(NA,nrow = length(x),ncol=length(uniqs))
    if (keep_na) {
        for (i in seq_along(uniqs)) res[,i] = x == uniqs[i]
    } else {
        for (i in seq_along(uniqs)) res[,i] = x %in% uniqs[i]
    }
    res = res*1
    colnames(res) = names(vallab)
    class(res) = unique(c("dichotomy",class(res))) # for future usage. by now there is no methods for this class
    res
}

#' @export
dichotomy.list = function(x, keep_unused = FALSE, keep_na = TRUE){
    
    
}

#' @export
dichotomy.matrix = function(x, keep_unused = FALSE, keep_na = TRUE){
    
    
}

#' @export
dichotomy.data.frame = function(x, keep_unused = FALSE, keep_na = TRUE){
    
    
}
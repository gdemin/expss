#' Convert variable to matrix of dummy variables (dichotomy).
#' 
#' \code{dichotomy} returns matrix with 0,1 and possibly NA.
#' 
#' @param x vector/factor/matrix/data.frame.
#' @param keep_unused Logical. Should we create columns for unused labels/factor levels.
#' @param use_na Logical. Should we use NA for rows with all NA or use 0.
#' @param keep Numeric/character. Labels/levels/values that should be keeped.
#' By default all used values will be keeped.
#' @return matrix with 0,1 which column names are value labels. If label doesn't exist for 
#' particular value then this value will be used as column name.
#' @details .
#' @seealso \code{\link{category}} for reverse conversion.
#' @examples
#' test_ds = data.frame(total = 1, s2b = sample(2:3,100,replace = TRUE))
#' age_group = test_ds$s2b
#' var_lab(test_ds$s2b) = "Age group"
#' age_group = set_var_lab(age_group,"Age group")
#' identical(age_group,test_ds$s2b) # should be TRUE
#' 
#' identical(var_lab(age_group),attr(age_group,"label")) # should be TRUE
#' 
#' @export
dichotomy = function(x, keep_unused = FALSE, use_na = TRUE, keep = NULL){
    UseMethod("dichotomy")    
}

#' @export
dichotomy.default = function(x, keep_unused = FALSE, use_na = TRUE, keep = NULL){
    vallab = dichotomy_helper(x,keep_unused,keep)
    res = matrix(0,nrow = length(x),ncol=length(vallab))
    if (use_na) {
        for (i in seq_along(vallab)) res[,i] = x == vallab[i]
    } else {
        for (i in seq_along(vallab)) res[,i] = x %in% vallab[i]
    }
    res[] = res*1
    colnames(res) = names(vallab)
    class(res) = unique(c("dichotomy",class(res))) # for future usage. by now there is no methods for this class
    res
}

#' @export
dichotomy.factor = function(x, keep_unused = FALSE, use_na = TRUE, keep = NULL){
    dichotomy.default(as.with_labels(x), keep_unused, use_na, keep)
}

#' @export
dichotomy.matrix = function(x, keep_unused = FALSE, use_na = TRUE, keep = NULL){
    
    dichotomy.data.frame(x, keep_unused, use_na, keep)
}

#' @export
dichotomy.data.frame = function(x, keep_unused = FALSE, use_na = TRUE, keep = NULL){
    
    vallab = dichotomy_helper(x,keep_unused,keep)
    
    res = matrix(0,nrow = nrow(x),ncol=length(vallab))
    if (!is.matrix(x)) x = as.matrix(x)
    for (i in seq_along(vallab)) res[,i] = res[,i] + rowSums(matrix(x %in% vallab[i],nrow=nrow(x)))

    res[] = 1*(res>0)
    if(use_na){
        nas = rowSums(!is.na(x))==0
        res[nas,] = NA
    }
    colnames(res) = names(vallab)
    class(res) = unique(c("dichotomy",class(res))) # for future usage. by now there is no methods for this class
    res    
    
}

dichotomy_helper = function(x, keep_unused = FALSE, keep = NULL){
    vallab=val_lab(x)
    varlab = var_lab(x)
    uniqs=sort(unique(c(x,recursive = TRUE)))
    if (length(uniqs)>0) uniqs = uniqs[!is.na(uniqs)]
    vallab = labelled_and_unlabelled(uniqs,vallab)
    if (!keep_unused) {
        vallab = vallab[vallab %in% uniqs]        
    } 
    if (!is.null(keep)){
        if(is.numeric(keep)){
            vallab = vallab[vallab %in% keep]
        } else {
            vallab = vallab[keep]
        }
    }
    if (!is.null(varlab) && (varlab!="")) names(vallab) = paste(varlab,names(vallab),sep = labels_sep) 
    vallab    
}



#' Convert variable (possibly multiple choice question) to matrix of dummy variables.
#' 
#' \code{dichotomy} returns matrix with 0,1 and possibly NA. 
#' \code{dichotomy1} returns same result as \code{dichotomy} but without last category.
#' 
#' @param x vector/factor/matrix/data.frame.
#' @param keep_unused Logical. Should we create columns for unused value labels/factor levels.
#' @param use_na Logical. Should we use NA for rows with all NA or use 0's instead.
#' @param keep Numeric/character. Labels/levels/values that should be kept.
#' By default all values will be kept.
#' @return matrix with 0,1 which column names are value labels. If label doesn't exist for 
#' particular value then this value will be used as column name.
#' @details This function converts variable/multiple response variable(matrix/data.frame)
#'  with category encoding into matrix with dichotomy encoding (0/1) 
#' suited for most statistical analysis, e. g. clustering, factor analysis, 
#' linear regression and so on.  
#' \code{dichotomy1} drops last column in dichotomy matrix. It is useful in many cases
#' because any column of such matrix usually is linear combinations of other columns.
#' @seealso \code{\link{category}} for reverse conversion.
#' @examples
#' # toy example
#' # brands - multiple response question
#' # Which brands do you use during last three months? 
#' set.seed(123)
#' brands = t(replicate(20,sample(c(1:5,NA),4,replace = FALSE)))
#' # score - evaluation of tested product
#' score = sample(-1:1,20,replace = TRUE)
#' var_lab(brands) = "Used brands"
#' val_lab(brands) = make_labels("
#'                               1 Brand A
#'                               2 Brand B
#'                               3 Brand C
#'                               4 Brand D
#'                               5 Brand E
#'                               ")
#' 
#' var_lab(score) = "Evaluation of tested brand"
#' val_lab(score) = make_labels("
#'                              -1 Dislike it
#'                               0 So-so
#'                               1 Like it    
#'                              ")
#' 
#' # percentage of used brands
#' colMeans(dichotomy(brands))
#' 
#' # percentage of brands within each score
#' aggregate(dichotomy(brands) ~ factor(score), FUN = mean)
#' 
#' # or, same result in another form
#' by(dichotomy(brands), factor(score), FUN = colMeans)
#' 
#' # customer segmentation by used brands
#' kmeans(dichotomy(brands),3)
#' 
#' # model of influence of used brands on evaluation of tested product 
#' summary(lm(score ~ dichotomy1(brands)))
#' 
#' @export
dichotomy = function(x, keep_unused = FALSE, use_na = TRUE, keep = NULL){
    UseMethod("dichotomy")    
}

#' @export
dichotomy.default = function(x, keep_unused = FALSE, use_na = TRUE, keep = NULL){
    vallab = dichotomy_helper(x,keep_unused,keep)
    x = c(x, recursive=TRUE)
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
    if (NCOL(x)<2) {
        dichotomy.default(x, keep_unused, use_na, keep)
    } else {
        dichotomy.data.frame(x, keep_unused, use_na, keep)    
    }
    
}

#' @export
dichotomy.data.frame = function(x, keep_unused = FALSE, use_na = TRUE, keep = NULL){
    vallab = dichotomy_helper(x,keep_unused,keep)
    if (!is.matrix(x)) x = as.matrix(x) 
    res = matrix(0,nrow = nrow(x),ncol=length(vallab))
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

#' @export
#' @rdname dichotomy
dichotomy1 = function(x, keep_unused = FALSE, use_na = TRUE, keep = NULL){
    res = dichotomy(x,keep_unused,use_na,keep)
    if (ncol(res)>0){
        res = res[,-ncol(res),drop = FALSE]
    }
    res
}

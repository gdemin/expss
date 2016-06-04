#' Convert variable (possibly multiple choice question) to matrix of dummy variables.
#' 
#' This function converts variable/multiple response variable(matrix/data.frame)
#'  with category encoding into matrix with dichotomy encoding (0/1) 
#' suited for most statistical analysis, e. g. clustering, factor analysis, 
#' linear regression and so on. 
#' \itemize{ 
#' \item{\code{dichotomy}}{ returns matrix with 0, 1 and possibly NA.} 
#' \item{\code{dichotomy1}}{ drops last column in dichotomy matrix. It is useful in many cases
#' because any column of such matrix usually is linear combinations of other columns.}
#' \item{\code{dummy}}{ is another shortcut for \code{dichotomy}.}
#' }
#' @param x vector/factor/matrix/data.frame.
#' @param keep_unused Logical. Should we create columns for unused value labels/factor levels.
#' @param use_na Logical. Should we use NA for rows with all NA or use 0's instead.
#' @param keep_values Numeric/character. Values that should be kept. By default
#'   all values will be kept.
#' @param keep_labels Numeric/character. Labels/levels that should be kept. By
#'   default all labels/levels will be kept.
#' @param drop_values Numeric/character. Values that should be dropped. By default
#'   all values will be kept. Ignored if keep_values/keep_labels are provided.
#' @param drop_labels Numeric/character. Labels/levels that should be dropped. By
#'   default all labels/levels will be kept. Ignored if keep_values/keep_labels are provided.
#' @return matrix with 0,1 which column names are value labels. If label doesn't exist for 
#' particular value then this value will be used as column name.
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
#' aggregate(dichotomy(brands) ~ f(score), FUN = mean)
#' 
#' # or, same result in another form
#' by(dichotomy(brands), f(score), FUN = colMeans)
#' 
#' # customer segmentation by used brands
#' kmeans(dichotomy(brands),3)
#' 
#' # model of influence of used brands on evaluation of tested product 
#' summary(lm(score ~ dichotomy(brands)))
#' 
#' @export
dichotomy = function(x, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                     keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    UseMethod("dichotomy")    
}

#' @export
dichotomy.default = function(x, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                             keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    vallab = dichotomy_helper(x = x,
                              keep_unused = keep_unused,
                              keep_values = keep_values, 
                              keep_labels = keep_labels, 
                              drop_values = drop_values, 
                              drop_labels = drop_labels)
    x = c(x, recursive=TRUE)
    res = matrix(0,nrow = length(x),ncol=length(vallab))
    if (use_na) {
        for (i in seq_along(vallab)) res[,i] = x == vallab[i]
    } else {
        for (i in seq_along(vallab)) res[,i] = x %in% vallab[i]
    }
    res[] = res*1
    colnames(res) = names(vallab)
    class(res) = union("dichotomy",class(res)) # for future usage. by now there is no methods for this class
    res
}

#' @export
dichotomy.factor = function(x, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                            keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    dichotomy.default(as.labelled(x), 
                      keep_unused = keep_unused,
                      use_na = use_na,
                      keep_values = keep_values, 
                      keep_labels = keep_labels, 
                      drop_values = drop_values, 
                      drop_labels = drop_labels)
}

#' @export
dichotomy.matrix = function(x, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                            keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    if (NCOL(x)<2) {
        dichotomy.default(x, 
                          keep_unused = keep_unused,
                          use_na = use_na,
                          keep_values = keep_values, 
                          keep_labels = keep_labels, 
                          drop_values = drop_values, 
                          drop_labels = drop_labels)
    } else {
        dichotomy.data.frame(x,
                             keep_unused = keep_unused,
                             use_na = use_na,
                             keep_values = keep_values, 
                             keep_labels = keep_labels, 
                             drop_values = drop_values, 
                             drop_labels = drop_labels)
    }
    
}

#' @export
dichotomy.data.frame = function(x, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                                keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    vallab = dichotomy_helper(x = x,
                              keep_unused = keep_unused,
                              keep_values = keep_values, 
                              keep_labels = keep_labels, 
                              drop_values = drop_values, 
                              drop_labels = drop_labels)
    # it seems strange that we call dichotomy.data.frame from dichotomy.matrix
    # but it is because we lost all labels if we convert data.frame to matrix and then call dichotomy.matrix
    if (!is.matrix(x)) x = as.matrix(x) 
    res = matrix(0,nrow = nrow(x),ncol=length(vallab))
    for (i in seq_along(vallab)) res[,i] = res[,i] + rowSums(matrix(x %in% vallab[i],nrow=nrow(x)))

    res[] = 1*(res>0)
    if(use_na){
        nas = rowSums(!is.na(x))==0
        res[nas,] = NA
    }
    colnames(res) = names(vallab)
    class(res) = union("dichotomy",class(res)) # for future usage. by now there is no methods for this class
    res    
    
}


# returns values+labels that will be used during dichotomizing
dichotomy_helper = function(x, keep_unused = FALSE, keep_values = NULL,
                            keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    vallab=val_lab(x)
    varlab = var_lab(x)
    uniqs=sort(unique(c(x,recursive = TRUE)))
    if(!is.null(keep_values) && keep_unused){
        uniqs = sort(union(uniqs,keep_values))
    }
    if(!is.null(keep_labels) && keep_unused){
            stopif(!all(keep_labels %in% names(vallab)),"keep_unused = TRUE but some values in 'keep_labels'",
                   " doesn't exist in value labels, e. g. '", setdiff(keep_labels, names(vallab))[1],"'")
    }
    if (length(uniqs)>0) uniqs = uniqs[!is.na(uniqs)]
    vallab = labelled_and_unlabelled(uniqs,vallab)
    if (!keep_unused) {
        vallab = vallab[vallab %in% uniqs]        
    } 
    if(!is.null(keep_values)){
            vallab = vallab[vallab %in% keep_values]
    } 
    if(!is.null(keep_labels)){
            vallab = vallab[keep_labels]
    }
    if(is.null(keep_values) && is.null(keep_labels)){
        if(!is.null(drop_values)){
            vallab = vallab[!(vallab %in% drop_values)]
        }
        if(!is.null(drop_labels)){
            vallab = vallab[setdiff(names(vallab),drop_labels)]
        }
    }
    if (!is.null(varlab) && (varlab!="")) names(vallab) = paste(varlab,names(vallab),sep = labels_sep) 
    vallab    
}

#' @export
#' @rdname dichotomy
dichotomy1 = function(x, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                      keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    res = dichotomy(x,
                    keep_unused = keep_unused,
                    use_na = use_na,
                    keep_values = keep_values, 
                    keep_labels = keep_labels, 
                    drop_values = drop_values, 
                    drop_labels = drop_labels)
    if (ncol(res)>0){
        res = res[,-ncol(res),drop = FALSE]
    }
    res
}

#' @export
#' @rdname dichotomy
dummy = dichotomy




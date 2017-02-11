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
#' \item{\code{*_df}}{ are the same functions as \code{dichotomy} etc. but return
#' data.frame instead of matrix.}
#' }
#' @param x vector/factor/matrix/data.frame.
#' @param prefix character. If it is not NULL it instead of labels will be used prefix+values.
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
#' @return matrix or data.frame of class \code{dichotomy} with 0,1 which column names are value labels or
#'   values with prefix. If label doesn't exist for particular value then this
#'   value will be used as column name.
#' @seealso \code{\link{category}} for reverse conversion, \link{mrset},
#'   \link{mdset} for usage multiple-response variables with tables.
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
#' cro_mean(dichotomy(brands), score)
#' # the same as
#' cro_cpct(brands, score)
#' 
#' # percentage of brands within each score - same numbers
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
#' # prefixed data.frame 
#' dichotomy_df(brands, prefix = "brand_")
#' 
#' @export
dichotomy = function(x, prefix = NULL, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                     keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    res = dichotomy_dispatcher(x = x,
                           prefix = prefix, 
                           keep_unused = keep_unused,
                           use_na = use_na,
                           keep_values = keep_values, 
                           keep_labels = keep_labels, 
                           drop_values = drop_values, 
                           drop_labels = drop_labels)
    res = do.call(prefix_helper, res)
    class(res) = union("dichotomy",class(res)) # for future usage. by now there is no methods for this class
    res 
      
}

#' @export
is.dichotomy = function(x){
    "dichotomy" %in% class(x)
}

# so unnessasary complex because we need return dihotomy_df with labels from original data

dichotomy_dispatcher = function(x, prefix = NULL, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                     keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    UseMethod("dichotomy_dispatcher")    
}


dichotomy_dispatcher.default = function(x, prefix = NULL, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
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
    list(res = res, vallab = vallab, prefix = prefix)
}


dichotomy_dispatcher.factor = function(x, prefix = NULL, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                            keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    dichotomy_dispatcher.default(as.labelled(x),
                      prefix = prefix,
                      keep_unused = keep_unused,
                      use_na = use_na,
                      keep_values = keep_values, 
                      keep_labels = keep_labels, 
                      drop_values = drop_values, 
                      drop_labels = drop_labels)
}


dichotomy_dispatcher.matrix = function(x, prefix = NULL, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                            keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    if (NCOL(x)<2) {
        dichotomy_dispatcher.default(x, 
                          prefix = prefix,
                          keep_unused = keep_unused,
                          use_na = use_na,
                          keep_values = keep_values, 
                          keep_labels = keep_labels, 
                          drop_values = drop_values, 
                          drop_labels = drop_labels)
    } else {
        dichotomy_dispatcher.data.frame(x,
                             prefix = prefix,
                             keep_unused = keep_unused,
                             use_na = use_na,
                             keep_values = keep_values, 
                             keep_labels = keep_labels, 
                             drop_values = drop_values, 
                             drop_labels = drop_labels)
    }
    
}


dichotomy_dispatcher.data.frame = function(x, prefix = NULL, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
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
    list(res = res, vallab = vallab, prefix = prefix)   
    
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
    vallab = labelled_and_unlabelled(uniqs, vallab)
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
    if (!is.null(varlab) && (varlab!="")) {
        names(vallab) = paste(varlab,names(vallab),sep = LABELS_SEP) 
    }    

    vallab    
}

#' @export
#' @rdname dichotomy
dichotomy1 = function(x, prefix = NULL, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                      keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    res = dichotomy(x,
                    prefix = prefix,
                    keep_unused = keep_unused,
                    use_na = use_na,
                    keep_values = keep_values, 
                    keep_labels = keep_labels, 
                    drop_values = drop_values, 
                    drop_labels = drop_labels)
    if (ncol(res)>0){
        res = res[,-ncol(res),drop = FALSE]
    }
    class(res) = union("dichotomy", class(res)) 
    res
}


#' @export
#' @rdname dichotomy
dichotomy1_df = function(x, prefix = NULL, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                      keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    res = dichotomy_df(x,
                    prefix = prefix,
                    keep_unused = keep_unused,
                    use_na = use_na,
                    keep_values = keep_values, 
                    keep_labels = keep_labels, 
                    drop_values = drop_values, 
                    drop_labels = drop_labels)
    if (ncol(res)>0){
        res = res[,-ncol(res),drop = FALSE]
    }
    class(res) = union("dichotomy", class(res)) 
    res
        
}

#' @export
#' @rdname dichotomy
dichotomy_df = function(x, prefix = NULL, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                         keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    res = dichotomy_dispatcher(x = x,
                               prefix = prefix, 
                               keep_unused = keep_unused,
                               use_na = use_na,
                               keep_values = keep_values, 
                               keep_labels = keep_labels, 
                               drop_values = drop_values, 
                               drop_labels = drop_labels)
    n_vallab = names(res$vallab)
    res = do.call(prefix_helper, res)
    res = as.data.frame(res, stringsAsFactors = FALSE, check.names = FALSE)
    if(length(n_vallab)>0 && !is.null(prefix)){
        for (each in seq_along(res)){
            var_lab(res[[each]]) = n_vallab[each]
        }
    } 
    class(res) = union("dichotomy", class(res)) 
    res
}

prefix_helper = function(res, vallab, prefix){
    if(is.null(prefix)){
        colnames(res) = names(vallab)    
    } else {
        if (length(vallab)>0){
            colnames(res) = paste0(prefix, vallab)
        }
    }
    res 
}

#' @export
#' @rdname dichotomy
dummy = dichotomy

#' @export
#' @rdname dichotomy
dummy_df = dichotomy_df

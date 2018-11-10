#' Convert variable (possibly multiple choice question) to data.frame/matrix of dummy variables.
#' 
#' This function converts variable/multiple response
#' variable (vector/matrix/data.frame) with category encoding into
#' data.frame/matrix with dichotomy encoding (0/1) suited for most statistical
#' analysis, e. g. clustering, factor analysis, linear regression and so on.
#' \itemize{ 
#' \item{\code{as.dichotomy}}{ returns data.frame of class 'dichotomy' with 0, 1
#' and possibly NA.}
#' \item{\code{dummy}}{ returns matrix of class 'dichotomy' with 0, 1 and possibly NA.}
#' \item{\code{dummy1}}{ drops last column in dichotomy matrix. It is useful in many cases
#' because any column of such matrix usually is linear combinations of other columns.}
#' }
#' @param x vector/factor/matrix/data.frame.
#' @param prefix character. By default "v".
#' @param keep_unused Logical. Should we create columns for unused value
#'   labels/factor levels? FALSE by default.
#' @param use_na Logical. Should we use NA for rows with all NA or use 0's
#'   instead. TRUE by default.
#' @param keep_values Numeric/character. Values that should be kept. By default
#'   all values will be kept.
#' @param keep_labels Numeric/character. Labels/levels that should be kept. By
#'   default all labels/levels will be kept.
#' @param drop_values Numeric/character. Values that should be dropped. By default
#'   all values will be kept. Ignored if keep_values/keep_labels are provided.
#' @param drop_labels Numeric/character. Labels/levels that should be dropped. By
#'   default all labels/levels will be kept. Ignored if keep_values/keep_labels are provided.
#' @param presence numeric value which will code presence of the level. By
#'   default it is 1. Note that all tables functions need that \code{presence}
#'   and \code{absence} will be 1 and 0.
#' @param absence numeric value which will code absence of the level. By default
#'   it is 0. Note that all tables functions need that \code{presence} and
#'   \code{absence} will be 1 and 0.
#' @return  \code{as.dichotomy} returns data.frame of class \code{dichotomy} 
#'   with 0,1. Columns of this data.frame have variable labels according to
#'   value labels of original data. If label doesn't exist for particular value
#'   then this value will be used as variable label. \code{dummy} returns matrix
#'   of class \code{dichotomy}. Column names of this matrix are value labels of
#'   original data.
#' @seealso \code{\link{as.category}} for reverse conversion, \link{mrset},
#'   \link{mdset} for usage multiple-response variables with tables.
#' @examples
#' # toy example
#' # brands - multiple response question
#' # Which brands do you use during last three months? 
#' set.seed(123)
#' brands = as.sheet(t(replicate(20,sample(c(1:5,NA),4,replace = FALSE))))
#' # score - evaluation of tested product
#' score = sample(-1:1,20,replace = TRUE)
#' var_lab(brands) = "Used brands"
#' val_lab(brands) = autonum("
#'                               Brand A
#'                               Brand B
#'                               Brand C
#'                               Brand D
#'                               Brand E
#'                               ")
#' 
#' var_lab(score) = "Evaluation of tested brand"
#' val_lab(score) = make_labels("
#'                              -1 Dislike it
#'                               0 So-so
#'                               1 Like it    
#'                              ")
#' 
#' cro_cpct(as.dichotomy(brands), score)
#' # the same as
#' cro_cpct(mrset(brands), score)
#' 
#' # customer segmentation by used brands
#' kmeans(dummy(brands), 3)
#' 
#' # model of influence of used brands on evaluation of tested product 
#' summary(lm(score ~ dummy(brands)))
#' 
#' # prefixed data.frame 
#' as.dichotomy(brands, prefix = "brand_")
#' 
#' @export
as.dichotomy = function(x, prefix = "v", keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                        keep_labels = NULL, drop_values = NULL, drop_labels = NULL, 
                        presence = 1, absence = 0){
    UseMethod("as.dichotomy")
}


#' @export
as.dichotomy.default = function(x, prefix = "v", keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                                keep_labels = NULL, drop_values = NULL, drop_labels = NULL,
                                presence = 1, absence = 0){
    # if(!is.labelled(x)) x = as.labelled(x)
    vallab = get_values_for_dichotomizing(x = x,
                                          keep_unused = keep_unused,
                                          keep_values = keep_values, 
                                          keep_labels = keep_labels, 
                                          drop_values = drop_values, 
                                          drop_labels = drop_labels)
    
    res = matrix(absence, nrow = NROW(x), ncol = length(vallab))
    
    row_index = seq_len(NROW(x))
    col_index = match(unlist(x, use.names = FALSE), vallab)
    res[cbind(row_index, col_index)] = presence
    
    if(use_na & NCOL(x)>0){
        res[!valid(x), ] = NA
    }
    
    res = as.sheet(res)
    if(NCOL(res)>0){
        colnames(res) = paste0(prefix, vallab)
    }
    for (each in seq_along(res)){
        var_lab(res[[each]]) = names(vallab)[each]
    }
    if(ncol(res) == 0){
        if(NROW(res)>0) {
            res[["NA"]] = NA
        } else {
            res[["NA"]] = logical(0)
        }    
    } 
    class(res) = union("dichotomy", setdiff(class(res), "category")) 
    res  
}




########################################

#' @export
#' @rdname as.dichotomy
dummy = function(x, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                     keep_labels = NULL, drop_values = NULL, drop_labels = NULL,
                 presence = 1, absence = 0){
    UseMethod("dummy")
    
}


#' @export
dummy.default = function(x, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                 keep_labels = NULL, drop_values = NULL, drop_labels = NULL,
                 presence = 1, absence = 0){
    # if(!is.labelled(x)) x = as.labelled(x)
    vallab = get_values_for_dichotomizing(x = x,
                                          keep_unused = keep_unused,
                                          keep_values = keep_values, 
                                          keep_labels = keep_labels, 
                                          drop_values = drop_values, 
                                          drop_labels = drop_labels)
    

    res = matrix(absence, nrow = NROW(x), ncol = length(vallab))
    row_index = seq_len(NROW(x))
    col_index = match(unlist(x, use.names = FALSE), vallab)
    res[cbind(row_index, col_index)] = presence
    if(use_na & NCOL(x)>0){
        res[!valid(x),] = NA
    }
    if(NCOL(res)>0){
        colnames(res) = names(vallab)
    } else {
        if(NROW(res)>0){
            res = cbind(res, NA)
            colnames(res) = "NA"
        }
    }
    class(res) = union("dichotomy", setdiff(class(res), "category")) 
    res  
}


#' @export
#' @rdname as.dichotomy
dummy1 = function(x, keep_unused = FALSE, use_na = TRUE, keep_values = NULL,
                  keep_labels = NULL, drop_values = NULL, drop_labels = NULL,
                  presence = 1, absence = 0){
    res = dummy(x,
                keep_unused = keep_unused,
                use_na = use_na,
                keep_values = keep_values, 
                keep_labels = keep_labels, 
                drop_values = drop_values, 
                drop_labels = drop_labels,
                presence = presence,
                absence = absence)
    if (NCOL(res)>1){
        res = res[, -ncol(res), drop = FALSE]
    }
    class(res) = union("dichotomy", setdiff(class(res), "category")) 
    res
}

#' @export
#' @rdname as.dichotomy
is.dichotomy = function(x){
    inherits(x, "dichotomy")
}



# returns values+labels that will be used during dichotomizing
get_values_for_dichotomizing = function(x, keep_unused = FALSE, keep_values = NULL,
                            keep_labels = NULL, drop_values = NULL, drop_labels = NULL){
    
    stopif(is.null(x), "'as.dichotomy' - 'x' is NULL. Perhaps a variable does not exist.")
    vallab = val_lab(x)
    varlab = var_lab(x)
    x = unlab(x)
    uniqs = uniq_elements(x)
    if(length(uniqs)>0) {
        uniqs = sort(uniqs, na.last = NA)
    }    

    if(!is.null(keep_values) && keep_unused){
        uniqs = sort(union(uniqs, keep_values))
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
            vallab = vallab[setdiff(names(vallab), drop_labels)]
        }
    }
    if (!is.null(varlab) && (varlab!="")) {
        names(vallab) = paste(varlab, names(vallab), sep = LABELS_SEP) 
    }    

    vallab    
}




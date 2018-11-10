#' Convert dichotomy data.frame/matrix to data.frame with category encoding
#' 
#' @param x Dichotomy data.frame/matrix (usually with 0,1 coding).
#' @param prefix If is not NULL then column names will be added in the form
#'   prefix+column number.
#' @param counted_value Vector. Values that will be considered as indicator 
#' of category presence. By default it equals to 1.
#' @param compress Logical. Should we drop columns with all NA? FALSE by 
#'   default. TRUE significantly decreases performance of the function.
#' @return data.frame of class \code{category} with numeric values
#'   that correspond to column numbers of counted values. Column names of x or
#'   variable labels are added as value labels.
#' @seealso \code{\link{as.dichotomy}} for reverse conversion, \link{mrset},
#'   \link{mdset} for usage multiple-response variables with tables.
#' @examples
#' set.seed(123)
#' 
#' # Let's imagine it's matrix of consumed products
#' dichotomy_matrix = matrix(sample(0:1,40,replace = TRUE,prob=c(.6,.4)),nrow=10)
#' colnames(dichotomy_matrix) = c("Milk","Sugar","Tea","Coffee")
#' 
#' as.category(dichotomy_matrix, compress = TRUE) # compressed version
#' category_encoding = as.category(dichotomy_matrix)
#' 
#'  # should be TRUE
#' identical(val_lab(category_encoding), c(Milk = 1L, Sugar = 2L, Tea = 3L, Coffee = 4L))
#' all(as.dichotomy(category_encoding, use_na = FALSE) == dichotomy_matrix)
#' 
#' # with prefix
#' as.category(dichotomy_matrix, prefix = "products_")
#' 
#' # data.frame with variable labels
#' dichotomy_dataframe = as.data.frame(dichotomy_matrix)
#' colnames(dichotomy_dataframe) = paste0("product_", 1:4)
#' var_lab(dichotomy_dataframe[[1]]) = "Milk"
#' var_lab(dichotomy_dataframe[[2]]) = "Sugar"
#' var_lab(dichotomy_dataframe[[3]]) = "Tea"
#' var_lab(dichotomy_dataframe[[4]]) = "Coffee"
#' 
#' as.category(dichotomy_dataframe, prefix = "products_")
#' 
#' @export
as.category = function(x, prefix = NULL, counted_value = 1, compress = FALSE){
    UseMethod("as.category")    
}


#' @export
as.category.matrix = function(x, prefix = NULL, counted_value = 1, compress = FALSE){
    vallab = colnames(x)
    varlab_nchars = 0
    if(length(vallab)>0){
        varlab = common_label(vallab)
        varlab_nchars = nchar(varlab)
        if(varlab_nchars>0){
            # + 2 because of '|' delimiter
            vallab = substr(vallab, varlab_nchars+2, nchar(vallab))
        }
    }
    res = col(x)
    res[!(x %in% counted_value)] = NA
    res = compress_and_finish(res = as.sheet(res), vallab = vallab, prefix = prefix, compress = compress)
    if(varlab_nchars>0) {
        set_var_lab(res, varlab)
    } else {
        res
    }    
}

#' @export
as.category.data.frame = function(x, prefix = NULL, counted_value = 1, compress = FALSE){
    vallab = unlist(lapply(seq_along(x), function(i){
        varlab = var_lab(x[[i]])
        if(!is.null(varlab) && varlab!=""){
            varlab
        } else {
            colnames(x)[i]
        }
    }))
    varlab_nchars = 0
    if(length(vallab)>0){
        varlab = common_label(vallab)
        varlab_nchars = nchar(varlab)
        if(varlab_nchars>0){
            # + 2 because of '|' delimiter
            vallab = substr(vallab, varlab_nchars+2, nchar(vallab))
        }
    }
    for(i in seq_along(x)){
        x[[i]] =  ((x[[i]] %in% counted_value) | NA)*i
    }
    res = compress_and_finish(res = x, vallab = vallab, prefix = prefix, compress = compress)
    if(varlab_nchars>0) {
        set_var_lab(res, varlab)
    } else {
        res
    }   
}

#' @export
as.category.default = function(x, prefix = NULL, counted_value = 1, compress = FALSE){
    as.category(x = as.matrix(x), 
                    prefix = prefix, 
                    counted_value = counted_value, 
                    compress = compress)
}

#' @export
#' @rdname as.category
is.category = function(x){
    inherits(x, "category")
}

compress_and_finish = function(res, vallab, prefix, compress){
    if(compress && NROW(res)>0 && NCOL(res)>1){
        res[] = t(apply(res,1,function(x){
            nas = is.na(x)
            c(x[!nas], rep(NA, sum(nas)))
        }))
        columns_with_values = colSums(!is.na(res))>0
        res = res[,columns_with_values,drop = FALSE]
    }
    if(!is.null(vallab)){
        val_lab(res) = structure(seq_along(vallab), names = vallab) 
    } 
    if(!is.null(prefix) && NCOL(res)>0){
        colnames(res) = paste0(prefix, seq_len(NCOL(res)))
    }
    if(NCOL(res) == 0){
        if(NROW(res)>0){
            res[["NA"]] = NA
        } else {
            res[["NA"]] = logical(0)
        }
    }
    class(res) = union("category", setdiff(class(res), "dichotomy"))
    res
    
}

common_label = function(vallabs){
    has_var_labs = grepl("^([^\\|]+)\\|", vallabs, perl = TRUE)
    if(!all(has_var_labs)) return("")
    all_var_labs = gsub("^([^\\|]+)\\|(.*)$", "\\1", vallabs, perl = TRUE)
    all_var_labs = unique(all_var_labs)
    if(length(all_var_labs)>1) return("")
    all_var_labs
}
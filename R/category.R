#' Convert dichotomy matrix/data.frame to matrix/data.frame with category encoding
#' 
#' @param x Dichotomy matrix (usually with 0,1 coding).
#' @param prefix If not is NULL then column names will be added in the form
#'   prefix+column number.
#' @param use_var_lab logical If TRUE then we will try to use variable labels as
#'   value labels instead of column names.
#' @param counted_value Vector. Values that should be considered as indicator 
#' of category presence. By default it equals to 1.
#' @param compress Logical. Should we drop columns with all NA? FALSE by default.
#' @return Matrix or data.frame with numeric values that correspond to column
#'   numbers of counted values. Column names of x or variable labels are added as value labels.
#' @seealso \code{\link{dichotomy}} for reverse conversion.
#' @examples
#' set.seed(123)
#' 
#' # Let's imagine it's matrix of consumed products
#' dichotomy_matrix = matrix(sample(0:1,40,replace = TRUE,prob=c(.6,.4)),nrow=10)
#' colnames(dichotomy_matrix) = c("Milk","Sugar","Tea","Coffee")
#' category(dichotomy_matrix,compress=FALSE) # uncompressed version
#' category_matrix=category(dichotomy_matrix)
#' 
#'  # should be TRUE
#' identical(val_lab(category_matrix),c(Milk = 1L,Sugar = 2L,Tea = 3L,Coffee = 4L))
#' all(dichotomy(category_matrix,use_na = FALSE)==dichotomy_matrix)
#' 
#' # with prefix
#' category(dichotomy_matrix, prefix = "products_")
#' 
#' # data.frame with variable labels
#' dichotomy_dataframe = as.data.frame(dichotomy_matrix)
#' colnames(dichotomy_dataframe) = paste0("product_", 1:4)
#' var_lab(dichotomy_dataframe[[1]]) = "Milk"
#' var_lab(dichotomy_dataframe[[2]]) = "Sugar"
#' var_lab(dichotomy_dataframe[[3]]) = "Tea"
#' var_lab(dichotomy_dataframe[[4]]) = "Coffee"
#' 
#' category_df(dichotomy_dataframe, prefix = "products_")
#' 
#' @export
category = function(x, prefix = NULL, use_var_lab = TRUE, counted_value=1, compress = FALSE){
    UseMethod("category")    
}

#' @export
category.matrix = function(x, prefix = NULL, use_var_lab = TRUE, counted_value=1, compress = FALSE){
   vallab = colnames(x)
   res = col(x)
   res[!(x %in% counted_value)] = NA
   compress_and_finish(res = res, vallab = vallab, prefix = prefix, compress = compress)
}

#' @export
category.data.frame = function(x, prefix = NULL, use_var_lab = TRUE, counted_value=1, compress = FALSE){
    if (use_var_lab){
        for (i in seq_along(x)){
            varlab = var_lab(x[[i]])
            if(!is.null(varlab)){
                colnames(x)[i] = varlab
            }
        }
    }  
    vallab = colnames(x)
    # res = col(x)
    for(i in seq_along(x)){
        x[[i]] =  ((x[[i]] %in% counted_value) | NA)*i
        # res[,i][!(x[[i]] %in% counted_value)] = NA
    }
    compress_and_finish(res = x, vallab = vallab, prefix = prefix, compress = compress)
}

#' @export
category.default = function(x, prefix = NULL, use_var_lab = TRUE, counted_value=1, compress = FALSE){
    category.matrix(x = as.matrix(x), 
                    prefix = prefix, 
                    use_var_lab = use_var_lab, 
                    counted_value, 
                    compress)
}

#' @export
#' @rdname category
category_df = function(x, prefix = NULL, use_var_lab = TRUE, counted_value=1, compress = FALSE){
    res =  category(x = x, 
                    prefix = prefix, 
                    use_var_lab = use_var_lab, 
                    counted_value, 
                    compress) 
    vallab = val_lab(res)
    if (!is.data.frame(res)) { 
        res = as.dtfrm(res)
        class(res) = union("category", class(res))   
    }    
    set_val_lab(res, vallab)
}




compress_and_finish = function(res, vallab, prefix, compress){
    if(compress && length(res)>0 && ncol(res)>1){
        res = t(apply(res,1,function(x){
            nas = is.na(x)
            c(x[!nas], rep(NA, sum(nas)))
        }))
        columns_with_values = colSums(!is.na(res))>0
        res = res[,columns_with_values,drop = FALSE]
    }
    if(!is.null(vallab)){
        val_lab(res) = structure(seq_along(vallab),names = vallab) 
        
    } 
    if(!is.null(prefix) && NCOL(res)>0){
        colnames(res) = paste0(prefix, seq_len(NCOL(res)))
    }
    class(res) = union("category", setdiff(class(res),"dichotomy"))
    res
    
}
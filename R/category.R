#' Convert dichotomy matrix to matrix with category encoding.
#' 
#' @param x Dichotomy matrix (usually with 0,1 coding).
#' @param counted_value Vector. Values that should be considered as indicator 
#' of category presence. By default it is 1.
#' @param compress Logical. Should we drop columns with all NA?
#' @return Matrix with numeric values that correspond to column numbers of counted values. 
#' Column names of x are added as value labels.
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
#' @export
category = function(x, counted_value=1, compress = TRUE){
    UseMethod("category")    
}

#' @export
category.matrix = function(x, counted_value=1, compress = TRUE){
   vallab = colnames(x)
   res = col(x)
   res[!(x %in% counted_value)] = NA
   if(compress && length(res)>0 && ncol(res)>1){
       res = t(apply(res,1,sort,na.last = TRUE))
       columns_with_values = colSums(!is.na(res))>0
       res = res[,columns_with_values,drop = FALSE]
   }
   if(!is.null(vallab)){
       val_lab(res) = structure(seq_len(ncol(x)),names = vallab) 
       
   } 
   class(res) = setdiff(class(res),"dichotomy")
   res
}

#' @export
category.data.frame = function(x, counted_value=1, compress = TRUE){
    category.matrix(as.matrix(x),counted_value,compress)
}

#' @export
category.default = function(x, counted_value=1, compress = TRUE){
    category.matrix(as.matrix(x),counted_value,compress)
}
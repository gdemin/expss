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
#' examples
#' @export
category = function(x, counted_value=1, compress = TRUE){
    UseMethod("category")    
}

#' @export
category.matrix = function(x, counted_value=1, compress = TRUE){
   vallab = colnames(x)
   res = col(x)
   res[!(x %in% counted_value)] = NA
   if(compress){
       res = t(apply(res,1,sort,na.last = TRUE))
       columns_with_values = colSums(!is.na(res))>0
       res = res[,columns_with_values,drop = FALSE]
   }
   if(!is.null(vallab)){
       val_lab(res) = structure(seq_len(ncol(x)),names = vallab) 
       
   } 
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
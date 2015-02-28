#' @export
category = function(x, counted_value=1, truncate = TRUE){
    UseMethod("category")    
}

#' @export
category.matrix = function(x, counted_value=1, truncate = TRUE){
   vallab = colnames(x)
   res = col(x)
   res[!(x %in% counted_value)] = NA
   if(truncate){
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
category.data.frame = function(x, counted_value=1, truncate = TRUE){
    category.matrix(as.matrix(x),counted_value,truncate)
}

#' @export
category.default = function(x, counted_value=1, truncate = TRUE){
    category.matrix(as.matrix(x),counted_value,truncate)
}
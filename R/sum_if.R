#' Conditional sum/mean/max/min
#' 
#' There are two flavors of this function - one works with entire dataset/matrix/vector
#' similar to Microsoft Excel \code{COUNTIF}. The second works rowwise - e. g. 
#' similar to SPSS \code{COUNT} function. 
#'  
#' @param criterion Vector with counted values, list with conditions or
#'   function. If criterion is missing (or is NULL) non-NA's values will be
#'   counted.
#' @param ... data. Vectors, matrixes, data.frames, list. Shorter arguments
#'   will be recycled.
#' @param x Counted values or criterion for counting. Vector, matrix, data.frame,
#'   list, function. Shorter columns in list will be recycled.
#' 
#' @return 
#' \code{count_if} return single value (vector of length 1). 
#' \code{row_count_if} returns vector of counts for each row of supplied arguments.
#' \code{col_count_if} returns vector of counts for each column of supplied arguments.
#' \code{\%has\%} returns logical vector - presence indicator of criterion in each row.
#' 
#' @details
#' \code{count_if} counts values in entire dataset and return single 
#' value (vector of length 1).
#' 
#' \code{row_count_if} counts values in each row of supplied arguments and return
#' vector of counts for each row of supplied arguments.
#' 
#' \code{col_count_if} counts values in each column of supplied arguments and return
#' vector of counts for each column of supplied arguments.
#' 
#' All functions never return NA's. 
#' 
#' If criterion is list, then each element considered as single condition and
#' they will be combined with AND.
#' 
#' Function criterion should return logicals of same size and shape as its argument.
#' This function will be applied to each column of supplied data and TRUE results will be counted.
#' There is asymmetrical behavior in \code{row_count_if} and
#' \code{col_count_if}: in both cases function criterion will be applied
#' columnwise.
#' There are special functions for usage as criteria (e. g. \code{gt(5)} is
#' equivalent ">5" in spreadsheet):
#' \itemize{
#' \item{\code{gt}}{ greater than}
#' \item{\code{gte}}{ greater than or equal}
#' \item{\code{eq}}{ equal} 
#' \item{\code{neq}}{ not equal} 
#' \item{\code{lt}}{ less than}
#' \item{\code{lte}}{ less than or equal}
#' } 
#' 
#' \code{\%has\%} is simple wrapper for rather frequent case \code{row_count_if(criterion,x)>0}.
#' 
#' @export
#' @examples
#' # Examples borrowed from Microsoft Excel help for COUNTIF
#' df1 = data.frame(
#'     a=c("apples",   "oranges",     "peaches",     "apples"),
#'     b = c(32, 54, 75, 86)
#' )
#' 
#' count_if("apples",df1$a) # 2
#' 
#' count_if("apples",df1) # 2
#' 
#' with(df1,count_if("apples",a,b)) # 2
#' 
#' count_if(gt(55),df1$b) # greater than 55 = 2
#' 
#' count_if(neq(75),df1$b) # not equal 75 = 3
#' 
#' count_if(gte(32),df1$b) # greater than or equal 32 = 4
#' 
#' count_if(list(gt(32), lt(86)),df1$b) # 2
#' 
#' count_if(gt(32) & lt(86),df1$b) # 2
#' 
#' count_if(33:85,df1$b) # 2
#' 
#' # more complex criteria
#' # values with letters
#' count_if(function(x) grepl("^[A-z]+$",x),df1) # 4
#' 
#' # values that started on 'a'
#' count_if(function(x) grepl("^a",x),df1) # 2
#' 
#' # row_count_if
#' row_count_if(function(x) grepl("^a",x),df1) # c(1,0,0,1)
#' 
#' df1 %has% 'apples' # c(TRUE,FALSE,FALSE,TRUE)
#' 
#' # example with dplyr
#' if (require(dplyr)){
#'  set.seed(123)
#'  df2 = as.data.frame(
#'         matrix(sample(c(1:10,NA),30,replace = TRUE),10)
#'  )
#'  df2  %>% mutate(exact = row_count_if(8, V1, V2, V3),
#'                     greater = row_count_if(gt(8), V1, V2, V3),
#'                     range = row_count_if(5:8, V1, V2, V3),
#'                     na = row_count_if(is.na, V1, V2, V3),
#'                     not_na = row_count_if(, V1, V2, V3)
#'                  ) -> result
#'  result
#' }
sum_if=function(criterion=NULL, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    sum(data, na.rm = TRUE)
}

#' @export
#' @rdname sum_if
row_sum_if=function(criterion=NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    rowSums(data, na.rm=TRUE)
}


#' @export
#' @rdname sum_if
col_sum_if=function(criterion=NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    colSums(data, na.rm=TRUE)
}

#' @export
#' @rdname sum_if
mean_if=function(criterion=NULL, ..., data = NULL){
    data = as.matrix(fun_if_helper(criterion = criterion, ..., data = data))
    if(!(is.numeric(data) | is.logical(data) | is.complex(data))) {
        stop("Invalid argument type: for averagibg it should be numeric or logical")
    }
    mean(as.matrix(data), na.rm = TRUE)
}

#' @export
#' @rdname sum_if
row_mean_if=function(criterion=NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    rowMeans(data, na.rm=TRUE)
}


#' @export
#' @rdname sum_if
col_mean_if=function(criterion=NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    colMeans(data, na.rm=TRUE)
}


#' @export
fun_if_helper = function(criterion,..., data){
    dfs = do.call(data.frame,c(list(...),stringsAsFactors=FALSE)) # form data.frame 
    criterion = build_criterion(criterion, dfs)
    if(is.null(data)) {
        
        return(set_na(dfs, !criterion))
    }    

    set_na(data) = !criterion
    if(is.list(data) && !is.data.frame(data)){
        data = do.call(data.frame,c(data,stringsAsFactors=FALSE))        
    }  else {
        if (!is.data.frame(data)){
            data = as.data.frame(data, stringsAsFactors = FALSE)
        } else {
            data
        }
    }   
    
}


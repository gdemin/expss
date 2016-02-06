#' Count values that meet a criterion that you specify
#' 
#' There are two flavors of this function - one works with entire dataset/matrix/vector
#' similar to Microsoft Excel \code{COUNTIF}. The second works rowwise - e. g. 
#' similar to SPSS \code{COUNT} function. 
#'  
#' @param criterion Vector with counted values, list with conditions or function.
#'  Excel expressions such as ">5" are allowed. 
#' @param ... Counted data. Vector, matrix, data.frame, list. Shorter arguments will be recycled.
#' @param x Counted values or criteria for counting. Vector, matrix, data.frame, list. Shorter columns in list
#'  will be recycled.
#' 
#' @return 
#' \code{count_if} return single value (vector of length 1). 
#' \code{row_count_if} returns vector of counts for each row of supplied arguments.
#' \code{\%has\%} returns logical vector - presense indicator of criterion in each row.
#' 
#' @details
#' \code{count_if} counts values in entire dataset and return single 
#' value (vector of length 1).
#' 
#' \code{row_count_if} counts values in each row of supplied arguments and return
#' vector of counts for each row of supplied arguments.
#' 
#' Both functions never return NA's. If criterion is missing (or is NULL) 
#' non-NA's values will be counted. 
#' 
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
#' count_if(">55",df1$b) # 2
#' 
#' count_if("!=75",df1$b) # 3
#' 
#' count_if(">=32",df1$b) # 4
#' 
#' count_if(list(">32", "<86"),df1$b) # 2
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
#' set.seed(123)
#' df2 = as.data.frame(
#'     matrix(sample(c(1:10,NA),30,replace = TRUE),10)
#' )
#' df2  %>% mutate(exact = row_count_if(8,V1,V2,V3),
#'                 greater = row_count_if(">8",V1,V2,V3),
#'                 range = row_count_if(5:8,V1,V2,V3),
#'                 na = row_count_if(is.na,V1,V2,V3),
#'                 not_na = row_count_if(,V1,V2,V3)
#'                 ) -> result
#' result
count_if=function(criterion=NULL,...){
    dfs = do.call(data.frame,c(list(...),stringsAsFactors=FALSE)) # form data.frame  
    if (is.null(criterion)){
        cond = !is.na(dfs)
    } else {
        cond = build_criterion(criterion, dfs)
    }   
    sum(cond,na.rm=TRUE)
}

#' @export
#' @rdname count_if
row_count_if=function(criterion=NULL,...){
    dfs = do.call(data.frame,c(list(...),stringsAsFactors=FALSE)) # form data.frame 
    if (is.null(criterion)){
        cond = !is.na(dfs)
    } else {
        cond = build_criterion(criterion,dfs)
    } 
    rowSums(cond,na.rm=TRUE)
}

#' @export
#' @rdname count_if
'%has%'=function(x,criterion){
    row_count_if(criterion=criterion,x)>0
}




#' Count values that meet a criterion that you specify
#' 
#' There are two flavors of this function - one works on entire dataset/matrix/vector
#' similar to Microsoft Excel \code{COUNTIF}. The second works rowwise - e. g. 
#' similar to SPSS \code{COUNT} function. 
#'  
#' @param criterion Vector with counted values or function (possibly constructed with \code{crit}) 
#' @param ... Counted data. Vector, matrix, data.frame, list. Shorter arguments will be recycled.
#' @param FUN Function for criterion construction. In most cases it is '>','==' and so on.
#' @param x Tested data. Vector, matrix, data.frame, list. Shorter columns in list
#'  will be recycled.
#' 
#' @return 
#' \code{count_if} return single value (vector of length 1). 
#' \code{row_count_if} returns vector of counts for each row of supplied arguments.
#' \code{\%has\%} returns logical vector - presense indicator of criterion in each row.
#' \code{crit} returns function of class 'criterion' that test argument for 
#' supplied condition. 
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
#' \code{crit(FUN,...)} translate FUN in function \code{FUN(x,...)}. Functions 
#' returned by \code{crit} may be combined with logical operators: |, &, !.
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
#' count_if(crit(">",55),df1$b) # 2
#' 
#' count_if(crit("!=",75),df1$b) # 3
#' 
#' count_if(crit(">=",32),df1$b) # 4
#' 
#' count_if(crit(">",32) & crit("<",86),df1$b) # 2
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
#'                 greater = row_count_if(crit(">",8),V1,V2,V3),
#'                 range = row_count_if(5:8,V1,V2,V3),
#'                 na = row_count_if(is.na,V1,V2,V3),
#'                 not_na = row_count_if(,V1,V2,V3)
#'                 ) -> result
#' result
count_if=function(criterion=NULL,...){
    dfs = do.call(data.frame,c(list(...),stringsAsFactors=FALSE)) # form data.frame  
    cond = build_criterion(criterion,dfs)
    sum(cond,na.rm=TRUE)
}

#' @export
#' @rdname count_if
row_count_if=function(criterion=NULL,...){
    dfs = do.call(data.frame,c(list(...),stringsAsFactors=FALSE)) # form data.frame 
    cond = build_criterion(criterion,dfs)
    rowSums(cond,na.rm=TRUE)
}

#' @export
#' @rdname count_if
'%has%'=function(x,criterion){
    row_count_if(criterion=criterion,x)>0
}

#' @export
#' @rdname count_if
crit = function(FUN,...){
    FUN = match.fun(FUN)
    res = function(x) FUN(x,...)
    class(res) = unique("criterion",class(res))
    res
}

#' @export
'!.criterion' = function(a) {
    res = function(x) !a(x)
    class(res) = unique("criterion",class(res))
    res
}


#' @export
'|.criterion' = function(e1,e2) {
    
    if (is.function(e1)) {
        f1 = e1
    } else {
        f1 = function(x) x %in% e1
    }
    if (is.function(e2)) {
        f2 = e2
    } else {
        f2 = function(x) x %in% e2
    }
    res = function(x) f1(x) | f2(x)
    class(res) = unique("criterion",class(res))
    res
}


#' @export
'&.criterion' = function(e1,e2) {
    if (is.function(e1)) {
        f1 = e1
    } else {
        f1 = function(x) x %in% e1
    }
    if (is.function(e2)) {
        f2 = e2
    } else {
        f2 = function(x) x %in% e2
    }
    res = function(x) f1(x) & f2(x)
    class(res) = unique("criterion",class(res))
    res
}


# TODO Удалить
#' @export
build_criterion = function(criterion,dfs){
    if (is.null(criterion)){
        cond = !is.na(dfs)
        return(cond)
    }
    UseMethod("build_criterion")
}

# TODO Удалить
# @export
build_criterion.function = function(criterion,dfs){
    res = lapply(dfs,function(colmn){
        cond = criterion(colmn)
        stopif(length(cond)!=length(colmn),"Cells number of criterion doesn't equal cells number of argument.")
        as.logical(cond)
    })
    do.call(cbind,res)
}

# TODO Удалить
# @export
build_criterion.default = function(criterion,dfs){
    build_criterion.function(function(x) x %in% criterion,dfs)
}

# TODO Удалить
# @export
build_criterion.criterion = function(criterion,dfs){
    build_criterion.function(criterion,dfs)
}



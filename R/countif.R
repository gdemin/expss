#' Count values that meet a criterion that you specify.
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
#' \code{countif} return single value (vector of length 1). 
#' \code{row_countif} returns vector of counts for each row of supplied arguments.
#' \code{\%has\%} returns logical vector - presense indicator of criterion in each row.
#' \code{crit} returns function of class 'criterion' that test argument for 
#' supplied condition. 
#' 
#' @details
#' \code{countif} counts values in entire dataset and return single 
#' value (vector of length 1).
#' 
#' \code{row_countif} counts values in each row of supplied arguments and return
#' vector of counts for each row of supplied arguments.
#' 
#' Both functions never return NA's. If criterion is missing (or is NULL) 
#' non-NA's values will be counted. 
#' 
#' \code{crit(FUN,...)} translate FUN in function \code{FUN(x,...)}. Functions 
#' returned by \code{crit} may be combined with logical operators: |, &, !.
#' 
#' \code{\%has\%} is simple wrapper for rather frequent case \code{row_countif(criterion,x)>0}.
#' 
#' @export
#' @examples
#' # Examples borrowed from Microsoft Excel help for COUNTIF
#' df1 = data.frame(
#'     a=c("apples",   "oranges",     "peaches",     "apples"),
#'     b = c(32, 54, 75, 86)
#' )
#' 
#' countif("apples",df1$a) # 2
#' 
#' countif("apples",df1) # 2
#' 
#' with(df1,countif("apples",a,b)) # 2
#' 
#' countif(crit(">",55),df1$b) # 2
#' 
#' countif(crit("!=",75),df1$b) # 3
#' 
#' countif(crit(">=",32),df1$b) # 4
#' 
#' countif(crit(">",32) & crit("<",86),df1$b) # 2
#' 
#' countif(33:85,df1$b) # 2
#' 
#' # more complex criteria
#' # values with letters
#' countif(function(x) grepl("^[A-z]+$",x),df1) # 4
#' 
#' # values that started on 'a'
#' countif(function(x) grepl("^a",x),df1) # 2
#' 
#' # row_countif
#' row_countif(function(x) grepl("^a",x),df1) # c(1,0,0,1)
#' 
#' df1 %has% 'apples' # c(TRUE,FALSE,FALSE,TRUE)
#' 
#' # example with dplyr
#' set.seed(123)
#' df2 = as.data.frame(
#'     matrix(sample(c(1:10,NA),30,replace = TRUE),10)
#' )
#' df2  %>% mutate(exact = row_countif(8,V1,V2,V3),
#'                 greater = row_countif(crit(">",8),V1,V2,V3),
#'                 range = row_countif(5:8,V1,V2,V3),
#'                 na = row_countif(is.na,V1,V2,V3),
#'                 not_na = row_countif(,V1,V2,V3)
#'                 ) -> result
#' result
countif=function(criterion=NULL,...){
    dtfrm = do.call(data.frame,c(list(...),stringsAsFactors=FALSE)) # form data.frame  
    cond = build_criterion(criterion,dtfrm)
    sum(cond,na.rm=TRUE)
}

#' @export
#' @rdname countif
row_countif=function(criterion=NULL,...){
    dtfrm = do.call(data.frame,c(list(...),stringsAsFactors=FALSE)) # form data.frame 
    cond = build_criterion(criterion,dtfrm)
    rowSums(cond,na.rm=TRUE)
}

#' @export
#' @rdname countif
'%has%'=function(x,criterion){
    row_countif(criterion=criterion,x)>0
}

#' @export
#' @rdname countif
crit = function(FUN,...){
    FUN <- match.fun(FUN)
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



build_criterion = function(criterion,dtfrm){
    if (is.null(criterion)){
        cond = !is.na(dtfrm)
        return(cond)
    }
    UseMethod("build_criterion")
}

build_criterion.function = function(criterion,dtfrm){
    res = lapply(dtfrm,function(colmn){
        cond = criterion(colmn)
        stopifnot_message(length(cond)==length(colmn),"Cells number of criterion doesn't equal cells number of argument.")
        as.logical(cond)
    })
    do.call(cbind,res)
}

build_criterion.default = function(criterion,dtfrm){
    build_criterion.function(function(x) x %in% criterion,dtfrm)
}

build_criterion.criterion = function(criterion,dtfrm){
    build_criterion.function(criterion,dtfrm)
}




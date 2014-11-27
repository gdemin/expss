#' Count values that meet a criterion that you specify.
#' 
#' There are two flavors of this function - one works on entire dataset/matrix/vector
#' similar to Microsoft Excel \code{COUNTIF}. The second works rowwise - e. g. 
#' similar to SPSS \code{COUNT} function. 
#'  
#' @param criteria Vector with counted values or function (possibly constructed with \code{crit}) 
#' @param ... Tested data. Vector, matrix, data.frame, list. Shorter arguments will be recycled.
#' @param FUN Function for criteria construction. In most cases it is '>','==' and so on.
#' 
#' @return 
#' \code{countif} return single value (vector of length 1). 
#' \code{row_countif} returns vector of counts for each row of supplied arguments.
#' \code{crit} returns function of class 'criteria' that test argument for 
#' supplied criteria.
#' 
#' @details
#' \code{countif} counts values in entire dataset and return single 
#' value (vector of length 1).
#' \code{row_countif} counts values in each row of supplied arguments and return
#' vector of counts for each row of supplied arguments. Both functions never 
#' return NA's. If criteria is missing (or is NULL) non-NA's values will be 
#' counted. Functions returned by \code{crit} may be combined with 
#' logical operators: |,&,!.
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
#' countif("apples",df1) # 2
#' with(df1,countif("apples",a,b)) # 2
#' countif(crit(">",55),df1$b) # 2
#' countif(crit("!=",75),df1$b) # 3
#' countif(crit(">=",32),df1$b) # 4
#' countif(crit(">",32) & crit("<",86),df1$b) # 2
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
#' # example with dplyr
#' set.seed(123)
#' df2 = as.data.frame(
#'     matrix(sample(c(1:10,NA),30,replace = TRUE),10)
#' )
#' df2  %>% mutate(exact = row_countif(8,V1,V2,V3),
#'                 greater = row_countif(crit(">",8),V1,V2,V3),
#'                 range = row_countif(5:8,V1,V2,V3)
#'                 ) -> result
#' result
countif=function(criteria=NULL,...){
    dtfrm = do.call(data.frame,list(...)) # form data.frame to use rowSums 
    cond = build_criteria(criteria,dtfrm)
    sum(cond,na.rm=TRUE)
}

#' @export
#' @rdname countif
row_countif=function(criteria=NULL,...){
    dtfrm = do.call(data.frame,list(...)) # form data.frame to use rowSums 
    cond = build_criteria(criteria,dtfrm)
    rowSums(cond,na.rm=TRUE)
}

#' @export
#' @rdname countif
crit = function(FUN,...){
    FUN <- match.fun(FUN)
    res = function(x) FUN(x,...)
    class(res) = unique("criteria",class(res))
    res
}

#' @export
'!.criteria' = function(a) function(x) !a(x)


#' @export
'|.criteria' = function(e1,e2) function(x) e1(x) | e2(x)

#' @export
'&.criteria' = function(e1,e2) function(x) e1(x) & e2(x)


build_criteria = function(criteria,dtfrm){
    if (is.null(criteria)){
        cond = !is.na(dtfrm)
        return(cond)
    }
    UseMethod("build_criteria")
}

build_criteria.function = function(criteria,dtfrm){
    res = lapply(dtfrm,function(colmn){
        cond = criteria(colmn)
        stopifnot_message(length(cond)==length(colmn),"Cells number of criteria doesn't equal cells number of argument.")
        as.logical(cond)
    })
    do.call(cbind,res)
}

build_criteria.default = function(criteria,dtfrm){
    build_criteria.function(function(x) x %in% criteria,dtfrm)
}

build_criteria.criteria = function(criteria,dtfrm){
    build_criteria.function(criteria,dtfrm)
}




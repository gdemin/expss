#' Count values that meet a criterion that you specify
#' 
#' There are two flavors of this function - one works with entire dataset/matrix/vector
#' similar to Microsoft Excel \code{COUNTIF}. The second works rowwise(columnwise) - e. g. 
#' similar to SPSS \code{COUNT} function. 
#'  
#' @param criterion Vector with counted values, list with conditions or
#'   function. If criterion is missing (or is NULL) non-NA's values will be
#'   used for function.
#' @param ... Data on which criterion will be applied. Vector, matrix,
#'   data.frame, list. Shorter arguments will be recycled.
#'   
#' @param x Counted values or criterion for counting. Vector, matrix, data.frame,
#'   list, function. Shorter columns in list will be recycled.
#'   
#' @param data Data on which function will be applied. Doesn't applicable to
#'   \code{count_*_if} functions. If omitted function will be applied on the ...
#'   argument.
#'   
#' @param fun Custom function that will be applied based on criterion.
#' 
#' @return 
#' \code{count_if} return single value (vector of length 1). 
#' \code{count_row_if} returns vector of counts for each row of supplied arguments.
#' \code{count_col_if} returns vector of counts for each column of supplied arguments.
#' \code{\%in_row\%}/\code{\%in_col\%} return logical vector - presence indicator of criterion in each row/column.
#' 
#' @details
#' \code{count_if} counts values in entire dataset and return single 
#' value (vector of length 1).
#' 
#' \code{count_row_if} counts values in each row of supplied arguments and return
#' vector of counts for each row of supplied arguments.
#' 
#' \code{count_col_if} counts values in each column of supplied arguments and return
#' vector of counts for each column of supplied arguments.
#' 
#' All functions never return NA's. 
#' 
#' If criterion is list, then each element considered as single condition and
#' they will be combined with AND.
#' 
#' Function criterion should return logicals of same size and shape as its argument.
#' This function will be applied to each column of supplied data and TRUE results will be counted.
#' There is asymmetrical behavior in \code{count_row_if} and
#' \code{count_col_if}: in both cases function criterion will be applied
#' columnwise.
#' There are special functions for usage as criteria (e. g. \code{gt(5)} is
#' equivalent ">5" in spreadsheet) - see \link{eq}:
#' 
#' \code{\%has\%} is simple wrapper for rather frequent case \code{count_row_if(criterion,x)>0}.
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
#' count_if(gt(32) & lt(86),df1$b) # 2
#' 
#' # count only integer values between 33 and 85
#' count_if(33:85,df1$b) # 2
#' 
#' # values with letters
#' count_if(regex("^[A-z]+$"),df1) # 4
#' 
#' # values that started on 'a'
#' count_if(regex("^a"),df1) # 2
#' 
#' # count_row_if
#' count_row_if(regex("^a"),df1) # c(1,0,0,1)
#' 
#' 'apples' %in_row% df1  # c(TRUE,FALSE,FALSE,TRUE)
#' 
#'  set.seed(123)
#'  df2 = as.data.frame(
#'         matrix(sample(c(1:10,NA),30,replace = TRUE),10)
#'  )
#'  result  = modify(df2, {
#'                     exact = count_row_if(8, V1, V2, V3)
#'                     greater = count_row_if(gt(8), V1, V2, V3)
#'                     range = count_row_if(5:8, V1, V2, V3)
#'                     na = count_row_if(is.na, V1, V2, V3)
#'                     not_na = count_row_if(, V1, V2, V3)
#'                  })  
#'  result
count_if=function(criterion = NULL,...){
    dfs = dots2data_frame(...)   
    cond = build_criterion(criterion, dfs)
    sum(cond,na.rm=TRUE)
}

#' @export
#' @rdname count_if
count_row_if=function(criterion = NULL,...){
    dfs = dots2data_frame(...)   
    cond = build_criterion(criterion, dfs)
    rowSums(cond,na.rm=TRUE)
}


#' @export
#' @rdname count_if
count_col_if=function(criterion = NULL,...){
    dfs = dots2data_frame(...)   
    cond = build_criterion(criterion, dfs)
    colSums(cond,na.rm=TRUE)
}


#' @export
#' @rdname count_if
'%in_row%'=function(criterion, x){
    count_row_if(criterion=criterion, x)>0
}

#' @export
#' @rdname count_if
'%in_col%'=function(criterion, x){
    count_col_if(criterion=criterion, x)>0
}

#' @export
#' @rdname count_if
sum_if=function(criterion = NULL, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    sum(data, na.rm = TRUE)
}

#' @export
#' @rdname count_if
sum_row_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    rowSums(data, na.rm=TRUE)
}


#' @export
#' @rdname count_if
sum_col_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    colSums(data, na.rm=TRUE)
}


################################################

#' @export
#' @rdname count_if
mean_if=function(criterion = NULL, ..., data = NULL){
    data = as.matrix(fun_if_helper(criterion = criterion, ..., data = data))
    if(!(is.numeric(data) | is.logical(data) | is.complex(data))) {
        stop("Invalid argument type: for averaging it should be numeric or logical")
    }
    mean(data, na.rm = TRUE)
}

#' @export
#' @rdname count_if
mean_row_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    rowMeans(data, na.rm=TRUE)
}


#' @export
#' @rdname count_if
mean_col_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    colMeans(data, na.rm=TRUE)
}


################################################

#' @export
#' @rdname count_if
sd_if=function(criterion = NULL, ..., data = NULL){
    data = as.matrix(fun_if_helper(criterion = criterion, ..., data = data))
    if(!(is.numeric(data) | is.logical(data) | is.complex(data))) {
        stop("Invalid argument type: for averaging it should be numeric or logical")
    }
    stats::sd(data, na.rm = TRUE)
}

#' @export
#' @rdname count_if
sd_row_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    apply(data, 1, stats::sd, na.rm=TRUE)
}


#' @export
#' @rdname count_if
sd_col_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    apply(data, 2, stats::sd, na.rm=TRUE)
}

################################################

#' @export
#' @rdname count_if
median_if=function(criterion = NULL, ..., data = NULL){
    data = as.matrix(fun_if_helper(criterion = criterion, ..., data = data))
    if(!(is.numeric(data) | is.logical(data) | is.complex(data))) {
        stop("Invalid argument type: for averaging it should be numeric or logical")
    }
    stats::median(data, na.rm = TRUE)
}

#' @export
#' @rdname count_if
median_row_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    apply(data, 1, stats::median, na.rm=TRUE)
}


#' @export
#' @rdname count_if
median_col_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    apply(data, 2, stats::median, na.rm=TRUE)
}


###################################################

#' @export
#' @rdname count_if
max_if=function(criterion = NULL, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(max(data, na.rm = TRUE))
    if(!is.finite(res)) res = NA
    res
}

#' @export
#' @rdname count_if
max_row_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(do.call(pmax, c(data, na.rm=TRUE)))
    res[!is.finite(res)] = NA
    res
}


#' @export
#' @rdname count_if
max_col_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(apply(data, 2, max, na.rm=TRUE))
    res[!is.finite(res)] = NA
    res
}

##########################################################

#' @export
#' @rdname count_if
min_if=function(criterion = NULL, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(min(data, na.rm = TRUE))
    if(!is.finite(res)) res = NA
    res
}

#' @export
#' @rdname count_if
min_row_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(do.call(pmin, c(data, na.rm=TRUE)))
    res[!is.finite(res)] = NA
    res
}


#' @export
#' @rdname count_if
min_col_if=function(criterion = NULL,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(apply(data, 2, min, na.rm=TRUE))
    res[!is.finite(res)] = NA
    res
}




#' @export
#' @rdname count_if
apply_row_if=function(fun, criterion = NULL,..., data = NULL){
    dfs = dots2data_frame(...) # form data.frame 
    criterion = build_criterion(criterion, dfs)
    if (is.null(data)){
        data = dfs
    }
    data = as.matrix(data)
    rows = 1:nrow(dfs)
    res = lapply(rows, function(each_row){
        filtered_row = data[each_row,][criterion[each_row,]]
        if(length(filtered_row) > 0){
            res_row = fun(filtered_row)
            stopif(length(res_row)!=1, "Incorrect result from function 'fun' - length of result is not equal to 1") 
            res_row
        } else {
            NA
        }
        
    })
    unlist(res)
}




#' @export
#' @rdname count_if
apply_col_if=function(fun, criterion = NULL,..., data = NULL){
    dfs = dots2data_frame(...) # form data.frame 
    criterion = build_criterion(criterion, dfs)
    if (is.null(data)){
        data = dfs
    }
    cols = 1:ncol(dfs)
    res = lapply(cols, function(each_col){
        filtered_col = data[[each_col]][criterion[, each_col]]
        if(length(filtered_col) > 0){
            res_col = fun(filtered_col)
            stopif(length(res_col)!=1, "Incorrect result from function 'fun' - length of result is not equal to 1") 
            res_col
        } else {
            NA
        }
        
    })
    stats::setNames(unlist(res), colnames(data))
}

#########################################################
fun_if_helper = function(criterion,..., data){
    dfs = dots2data_frame(...)   
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

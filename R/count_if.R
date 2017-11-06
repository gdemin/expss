#' Count/sum/average/other functions on values that meet a criterion
#' 
#' These functions calculate count/sum/average/etc on values that meet a 
#' criterion that you specify. \code{apply_if_*} apply custom functions. There
#' are different flavors of these functions: \code{*_if} work on entire
#' dataset/matrix/vector, \code{*_row_if} works on each row and \code{*_col_if}
#' works on each column.
#'  
#' @param criterion Vector with counted values, logical vector/matrix or
#'   function. See details and examples.
#'   
#' @param ... Data on which criterion will be applied. Vector, matrix,
#'   data.frame, list. Shorter arguments will be recycled.
#'   
#' @param x Data on which criterion will be applied. Vector, matrix,
#'   data.frame, list. Shorter arguments will be recycled.
#'   
#' @param data Data on which function will be applied. Doesn't applicable to 
#'   \code{count_*_if} functions. If omitted then function will be applied on
#'   the ... argument.
#'   
#' @param fun Custom function that will be applied based on criterion.
#' 
#' @return 
#' \code{*_if} return single value (vector of length 1). 
#' \code{*_row_if} returns vector for each row of supplied arguments.
#' \code{*_col_if} returns vector for each column of supplied arguments.
#' \code{\%row_in\%}/\code{\%col_in\%} return logical vector - indicator of
#' presence of criterion in each row/column. \code{\%has\%} is an alias for
#' \code{\%row_in\%}.
#' 
#' @details
#' Possible type for criterion argument:
#' \itemize{
#' \item{vector/single value}{ All values in \code{...} which equal to elements of
#' vector in criteria will be used as function \code{fun} argument.}
#' \item{function}{ Values for which function gives TRUE will be used as 
#' function \code{fun} argument. There are some special functions for
#' convenience (e. g. \code{gt(5)} is equivalent ">5" in spreadsheet) - see
#' \link{criteria}.}
#' \item{logical vector/matrix/data.frame}{ Values for which element of 
#' criterion equals to TRUE will be used as function \code{fun} argument.
#' Logical vector will be recycled across all columns of \code{...}\code{data}.
#' If criteria is logical matrix/data.frame then column from this
#' matrix/data.frame will be used for corresponding column/element of
#' \code{...}\code{data}. Note that this kind of criterion doesn't use
#' \code{...} so \code{...} can be used instead of \code{data} argument.}}
#' 
#' \code{count*} and \code{\%in*\%} never returns NA's. Other functions remove
#' NA's before calculations (as \code{na.rm = TRUE} in base R functions).
#' 
#' Function criterion should return logical vector of same size and shape as its
#' argument. This function will be applied to each column of supplied data and
#' TRUE results will be used. There is asymmetrical behavior in \code{*_row_if}
#' and \code{*_col_if} for function criterion: in both cases function criterion
#' will be applied columnwise.
#'
#' 
#' 
#' @export
#' @examples
#' set.seed(123)
#' dfs = as.data.frame(
#'        matrix(sample(c(1:10,NA), 30, replace = TRUE), 10)
#' )
#' 
#' result  = modify(dfs, {
#'              # count 8
#'              exact = count_row_if(8, V1, V2, V3)
#'              # count values greater than 8
#'              greater = count_row_if(gt(8), V1, V2, V3)
#'              # count integer values between 5 and 8, e. g. 5, 6, 7, 8
#'              integer_range = count_row_if(5:8, V1, V2, V3)
#'              # count values between 5 and 8 
#'              range = count_row_if(5 %thru% 8, V1, V2, V3)
#'              # count NA 
#'              na = count_row_if(is.na, V1, V2, V3)
#'              # count not-NA 
#'              not_na = count_row_if(not_na, V1, V2, V3) 
#'              # are there any 5 in each row?
#'              has_five = cbind(V1, V2, V3) %row_in% 5   
#'          })  
#' result
#'  
#' mean_row_if(6, dfs$V1, data = dfs)
#' median_row_if(gt(2), dfs$V1, dfs$V2, dfs$V3) 
#' sd_row_if(5 %thru% 8, dfs$V1, dfs$V2, dfs$V3)
#'  
#' if_na(dfs) = 5 # replace NA 
#' 
#' # custom apply
#' apply_col_if(prod, gt(2), dfs$V1, data = dfs) # product of all elements by columns
#' apply_row_if(prod, gt(2), dfs$V1, data = dfs) # product of all elements by rows
#'  
#' # Examples borrowed from Microsoft Excel help for COUNTIF
#' df1 = data.frame(
#'     a = c("apples",   "oranges",     "peaches",     "apples"),
#'     b = c(32, 54, 75, 86)
#' )
#' 
#' count_if("apples", df1$a) # 2
#' 
#' count_if("apples", df1) # 2
#' 
#' with(df1, count_if("apples", a, b)) # 2
#' 
#' count_if(gt(55), df1$b) # greater than 55 = 2
#' 
#' count_if(ne(75), df1$b) # not equal 75 = 3
#' 
#' count_if(ge(32), df1$b) # greater than or equal 32 = 4
#' 
#' count_if(gt(32) & lt(86), df1$b) # 2
#' 
#' # count only integer values between 33 and 85
#' count_if(33:85, df1$b) # 2
#' 
#' # values with letters
#' count_if(regex("^[A-z]+$"), df1) # 4
#' 
#' # values that started on 'a'
#' count_if(regex("^a"), df1) # 2
#' 
#' # count_row_if
#' count_row_if(regex("^a"), df1) # c(1,0,0,1)
#' 
#' df1 %row_in% 'apples'  # c(TRUE,FALSE,FALSE,TRUE)
#' 
#' # Some of Microsoft Excel examples for SUMIF/AVERAGEIF/etc 
#' dfs = read.csv(
#'     text = "
#'     property_value,commission,data
#'     100000,7000,250000
#'     200000,14000,	
#'     300000,21000,	
#'     400000,28000,"
#' )
#' 
#' # Sum of commision for property value greater than 160000
#' with(dfs, sum_if(gt(160000), property_value, data = commission)) # 63000
#'     
#' # Sum of property value greater than 160000
#' with(dfs, sum_if(gt(160000), property_value)) # 900000
#' 
#' # Sum of commision for property value equals to 300000
#' with(dfs, sum_if(300000, property_value, data = commission)) # 21000
#'     
#' # Sum of commision for property value greater than first value of data
#' with(dfs, sum_if(gt(data[1]), property_value, data = commission)) # 49000
#'     
#' 
#' dfs = data.frame(
#'     category = c("Vegetables", "Vegetables", "Fruits", "", "Vegetables", "Fruits"),
#'     food = c("Tomatoes", "Celery", "Oranges", "Butter", "Carrots", "Apples"),
#'     sales = c(2300, 5500, 800, 400, 4200, 1200),
#'     stringsAsFactors = FALSE
#' )
#' 
#' # Sum of sales for Fruits
#' with(dfs, sum_if("Fruits", category, data = sales)) # 2000
#' 
#' # Sum of sales for Vegetables    
#' with(dfs, sum_if("Vegetables", category, data = sales)) # 12000
#' 
#' # Sum of sales for food which is ending on 'es' 
#' with(dfs, sum_if(perl("es$"), food, data = sales)) # 4300
#' 
#' # Sum of sales for empty category
#' with(dfs, sum_if("", category, data = sales))  # 400
#' 
#' 
#' dfs = read.csv(
#'     text = "
#'     property_value,commission,data
#'     100000,7000,250000
#'     200000,14000,	
#'     300000,21000,	
#'     400000,28000,"
#' )
#' 
#' # Commision average for comission less than 23000
#' with(dfs, mean_if(lt(23000), commission)) # 14000
#' 
#' 
#' # Property value average for property value less than 95000
#' with(dfs, mean_if(lt(95000), property_value)) #  NaN
#' 
#' # Commision average for property value greater than 250000
#' with(dfs, mean_if(gt(250000), property_value, data = commission)) # 24500
#' 
#' 
#' dfs = data.frame(
#'     region = c("East", "West", "North", "South (New Office)",  "MidWest"),
#'     profits = c(45678, 23789, -4789, 0, 9678),
#'     stringsAsFactors = FALSE
#' )
#' 
#' 
#' # Mean profits for 'west' regions
#' with(dfs, mean_if(fixed("West"), region, data = profits)) # 16733.5
#' 
#' 
#' # Mean profits for regions wich doesn't contain New Office
#' with(dfs, mean_if(!fixed("(New Office)"), region, data = profits))  # 18589
#' 
#' 
#' dfs = read.csv(
#'     text = '
#'     grade,weight 
#'     89,1
#'     93,2
#'     96,2
#'     85,3
#'     91,1
#'     88,1'
#'     ,stringsAsFactors = FALSE
#' )
#' 
#' # Minimum grade for weight equals to 1
#' with(dfs, min_if(1, weight, data = grade)) # 88
#' 
#' 
#' # Maximum grade for weight equals to 1
#' with(dfs, max_if(1, weight, data = grade)) #91
#' 
#' 
#' # Example with offset
#' dfs = read.csv(
#'     text = '
#'     weight,grade 
#'     10,b
#'     11,a
#'     100,a
#'     111,b
#'     1,a
#'     1,a'
#'     ,stringsAsFactors = FALSE
#' )
#' 
#' with(dfs, min_if("a", grade[2:5], data = weight[1:4])) # 10
#' 
#' 
count_if=function(criterion,...){
    dfs = dots2data_frame(...)   
    cond = build_criterion(criterion, dfs)
    sum(cond,na.rm=TRUE)
}

#' @export
#' @rdname count_if
count_row_if=function(criterion,...){
    dfs = dots2data_frame(...)   
    cond = build_criterion(criterion, dfs)
    rowSums(cond,na.rm=TRUE)
}


#' @export
#' @rdname count_if
count_col_if=function(criterion,...){
    dfs = dots2data_frame(...)   
    cond = build_criterion(criterion, dfs)
    colSums(cond,na.rm=TRUE)
}




#' @export
#' @rdname count_if
'%row_in%'=function(x, criterion){
    count_row_if(criterion=criterion, x)>0
}

#' @export
#' @rdname count_if
'%has%' = `%row_in%`

#' @export
#' @rdname count_if
'%col_in%'=function(x, criterion){
    count_col_if(criterion=criterion, x)>0
}

#' @export
#' @rdname count_if
sum_if=function(criterion, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    sum(data, na.rm = TRUE)
}

#' @export
#' @rdname count_if
sum_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    rowSums(data, na.rm=TRUE)
}


#' @export
#' @rdname count_if
sum_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    colSums(data, na.rm=TRUE)
}


################################################

#' @export
#' @rdname count_if
mean_if=function(criterion, ..., data = NULL){
    data = as.matrix(fun_if_helper(criterion = criterion, ..., data = data))
    if(!(is.numeric(data) | is.logical(data) | is.complex(data))) {
        stop("Invalid argument type: for averaging it should be numeric or logical")
    }
    mean(data, na.rm = TRUE)
}

#' @export
#' @rdname count_if
mean_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    rowMeans(data, na.rm=TRUE)
}


#' @export
#' @rdname count_if
mean_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    colMeans(data, na.rm=TRUE)
}


################################################

#' @export
#' @rdname count_if
sd_if=function(criterion, ..., data = NULL){
    data = as.matrix(fun_if_helper(criterion = criterion, ..., data = data))
    if(!(is.numeric(data) | is.logical(data) | is.complex(data))) {
        stop("Invalid argument type: for standard deviation it should be numeric or logical")
    }
    stats::sd(data, na.rm = TRUE)
}

#' @export
#' @rdname count_if
sd_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    apply(data, 1, stats::sd, na.rm=TRUE)
}


#' @export
#' @rdname count_if
sd_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    vapply(data, stats::sd, FUN.VALUE = numeric(1), na.rm=TRUE)
}

################################################

#' @export
#' @rdname count_if
median_if=function(criterion, ..., data = NULL){
    data = as.matrix(fun_if_helper(criterion = criterion, ..., data = data))
    if(!(is.numeric(data) | is.logical(data) | is.complex(data))) {
        stop("Invalid argument type: for median it should be numeric or logical")
    }
    stats::median(data, na.rm = TRUE)
}

#' @export
#' @rdname count_if
median_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    apply(data, 1, stats::median, na.rm=TRUE)
}


#' @export
#' @rdname count_if
median_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    vapply(data, FUN = stats::median, FUN.VALUE = numeric(1), na.rm=TRUE)
}


###################################################

#' @export
#' @rdname count_if
max_if=function(criterion, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(max(data, na.rm = TRUE))
    if(is.numeric(res) && !is.finite(res)) res = NA
    res
}

#' @export
#' @rdname count_if
max_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(do.call(pmax, c(data, na.rm=TRUE)))
    if(is.numeric(res)) res[!is.finite(res)] = NA
    res
}


#' @export
#' @rdname count_if
max_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(apply(data, 2, max, na.rm=TRUE))
    if(is.numeric(res)) res[!is.finite(res)] = NA
    res
}

##########################################################

#' @export
#' @rdname count_if
min_if=function(criterion, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(min(data, na.rm = TRUE))
    if(is.numeric(res) && !is.finite(res)) res = NA
    res
}

#' @export
#' @rdname count_if
min_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(do.call(pmin, c(data, na.rm=TRUE)))
    if(is.numeric(res)) res[!is.finite(res)] = NA
    res
}


#' @export
#' @rdname count_if
min_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data)
    res = suppressWarnings(apply(data, 2, min, na.rm=TRUE))
    if(is.numeric(res)) res[!is.finite(res)] = NA
    res
}




#' @export
#' @rdname count_if
apply_row_if=function(fun, criterion,..., data = NULL){
    dfs = dots2data_frame(...) # form data.frame 
    criterion = build_criterion(criterion, dfs)
    
    if (is.null(data)){
        data = as.matrix(dfs)
    } else {
        data = as.matrix(data)
        criterion = if_val(
            matrix(TRUE, ncol = NCOL(data), nrow = NROW(data)), 
            criterion ~ TRUE, 
            !criterion ~ FALSE,
            other ~ FALSE
        )
    }
    
    rows = 1:nrow(data)
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
apply_col_if=function(fun, criterion,..., data = NULL){
    dfs = dots2data_frame(...) # form data.frame 
    criterion = build_criterion(criterion, dfs)
    if (is.null(data)){
        data = dfs
    } 
    cols = 1:ncol(data)
    res = lapply(cols, function(each_col){
        filtered_col = column(data, each_col)[column(criterion, each_col)]
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
        return(na_if(dfs, !criterion))
    }    
    
    if(!is.data.frame(data)){
            data = as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
    }    
    na_if(data, !criterion) 
}

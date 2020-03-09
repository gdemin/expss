#' Count/sum/average/other functions on values that meet a criterion
#' 
#' These functions calculate count/sum/average/etc. on values that meet a 
#' criterion that you specify. \code{apply_if_*} apply custom functions. There
#' are different flavors of these functions: \code{*_if} work on entire
#' dataset/matrix/vector, \code{*_row_if} works on each row and \code{*_col_if}
#' works on each column.
#'  
#' @param criterion Vector with counted values or
#'   function. See details and examples.
#'   
#' @param ... Data on which criterion will be applied. Vector, matrix,
#'   data.frame, list. 
#'   
#' @param x Data on which criterion will be applied. Vector, matrix,
#'   data.frame, list. 
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
#' \item{vector/single value}{ All values in \code{...} which equal to the
#' elements of vector in the criteria will be used as function \code{fun}
#' argument.}
#' \item{function}{ Values for which function gives TRUE will be used as
#' function \code{fun} argument. There are some special functions for
#' convenience (e. g. \code{gt(5)} is equivalent ">5" in spreadsheet) - see
#' \link{criteria}.}}
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
#' @export
#' @examples
#' set.seed(123)
#' sheet1 = as.sheet(
#'        matrix(sample(c(1:10,NA), 30, replace = TRUE), 10)
#' )
#' 
#' result  = compute(sheet1, {
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
#' mean_row_if(6, sheet1$V1, data = sheet1)
#' median_row_if(gt(2), sheet1$V1, sheet1$V2, sheet1$V3) 
#' sd_row_if(5 %thru% 8, sheet1$V1, sheet1$V2, sheet1$V3)
#'  
#' if_na(sheet1) = 5 # replace NA 
#' 
#' # custom apply
#' apply_col_if(prod, gt(2), sheet1$V1, data = sheet1) # product of all elements by columns
#' apply_row_if(prod, gt(2), sheet1$V1, data = sheet1) # product of all elements by rows
#'  
#' # Examples borrowed from Microsoft Excel help for COUNTIF
#' sheet1 = text_to_columns(
#'     "
#'        a       b
#'     apples    32
#'     oranges   54
#'     peaches   75
#'     apples    86
#'     "
#' )
#' 
#' count_if("apples", sheet1$a) # 2
#' 
#' count_if("apples", sheet1) # 2
#' 
#' calc(sheet1, count_if("apples", a, b)) # 2
#' 
#' count_if(gt(55), sheet1$b) # greater than 55 = 2
#' 
#' count_if(ne(75), sheet1$b) # not equal 75 = 3
#' 
#' count_if(ge(32), sheet1$b) # greater than or equal 32 = 4
#' 
#' count_if(gt(32) & lt(86), sheet1$b) # 2
#' 
#' # count only integer values between 33 and 85
#' count_if(33:85, sheet1$b) # 2
#' 
#' # values with letters
#' count_if(regex("^[A-z]+$"), sheet1) # 4
#' 
#' # values that started on 'a'
#' count_if(regex("^a"), sheet1) # 2
#' 
#' # count_row_if
#' count_row_if(regex("^a"), sheet1) # c(1,0,0,1)
#' 
#' sheet1 %row_in% 'apples'  # c(TRUE,FALSE,FALSE,TRUE)
#' 
#' # Some of Microsoft Excel examples for SUMIF/AVERAGEIF/etc 
#' sheet1 = text_to_columns(
#'     "
#'     property_value commission data
#'     100000              7000  250000
#'     200000             14000	
#'     300000             21000	
#'     400000             28000
#'     "
#' )
#' 
#' # Sum of commision for property value greater than 160000
#' calc(sheet1, sum_if(gt(160000), property_value, data = commission)) # 63000
#'     
#' # Sum of property value greater than 160000
#' calc(sheet1, sum_if(gt(160000), property_value)) # 900000
#' 
#' # Sum of commision for property value equals to 300000
#' calc(sheet1, sum_if(300000, property_value, data = commission)) # 21000
#'     
#' # Sum of commision for property value greater than first value of data
#' calc(sheet1, sum_if(gt(data[1]), property_value, data = commission)) # 49000
#'     
#' sheet1 = text_to_columns(
#'        "
#'          category     food sales
#'        Vegetables Tomatoes  2300
#'        Vegetables   Celery  5500
#'            Fruits  Oranges   800
#'              NA     Butter   400
#'        Vegetables  Carrots  4200
#'            Fruits   Apples  1200
#'        "
#'        )
#' 
#' # Sum of sales for Fruits
#' calc(sheet1, sum_if("Fruits", category, data = sales)) # 2000
#' 
#' # Sum of sales for Vegetables    
#' calc(sheet1, sum_if("Vegetables", category, data = sales)) # 12000
#' 
#' # Sum of sales for food which is ending on 'es' 
#' calc(sheet1, sum_if(perl("es$"), food, data = sales)) # 4300
#' 
#' # Sum of sales for empty category
#' calc(sheet1, sum_if(NA, category, data = sales))  # 400
#' 
#' 
#' sheet1 = text_to_columns(
#'     "
#'     property_value commission data
#'     100000              7000  250000
#'     200000             14000	
#'     300000             21000	
#'     400000             28000
#'     "
#' )
#' 
#' # Commision average for comission less than 23000
#' calc(sheet1, mean_if(lt(23000), commission)) # 14000
#' 
#' 
#' # Property value average for property value less than 95000
#' calc(sheet1, mean_if(lt(95000), property_value)) #  NaN
#' 
#' # Commision average for property value greater than 250000
#' calc(sheet1, mean_if(gt(250000), property_value, data = commission)) # 24500
#' 
#' 
#' sheet1 = text_to_columns(
#'     '
#'                 region  profits
#'                   East   45678
#'                   West   23789
#'                  North   -4789
#'     "South (New Office)"     0
#'                MidWest    9678
#'     ',
#'     quote = '"'
#' )
#' 
#' 
#' # Mean profits for 'west' regions
#' calc(sheet1, mean_if(contains("West"), region, data = profits)) # 16733.5
#' 
#' 
#' # Mean profits for regions wich doesn't contain New Office
#' calc(sheet1, mean_if(not(contains("New Office")), region, data = profits))  # 18589
#' 
#' 
#' sheet1 = text_to_columns(
#'     "
#'     grade weight 
#'     89      1
#'     93      2
#'     96      2
#'     85      3
#'     91      1
#'     88      1
#'     "
#' )
#' 
#' # Minimum grade for weight equals to 1
#' calc(sheet1, min_if(1, weight, data = grade)) # 88
#' 
#' 
#' # Maximum grade for weight equals to 1
#' calc(sheet1, max_if(1, weight, data = grade)) #91
#' 
#' 
#' # Example with offset
#' sheet1 = text_to_columns(
#'     "
#'     weight grade 
#'        10    b
#'        11    a
#'       100    a
#'       111    b
#'         1    a
#'         1    a
#'     "
#' )
#' 
#' calc(sheet1, min_if("a", grade[2:5], data = weight[1:4])) # 10
#' 
#' 
count_if=function(criterion,...){
    cond = build_condition_matrix(criterion, ..., logical_as_numeric = TRUE)
    matrixStats::sum2(cond, na.rm=TRUE)
}

#' @export
#' @rdname count_if
count_row_if=function(criterion,...){
    cond = build_condition_matrix(criterion, ..., logical_as_numeric = TRUE)
    matrixStats::rowSums2(cond)
}


#' @export
#' @rdname count_if
count_col_if=function(criterion,...){
    cond = build_condition_matrix(criterion, ..., logical_as_numeric = TRUE)
    res = matrixStats::colSums2(cond)
    names(res) = colnames(cond)
    res
}




#' @export
#' @rdname count_if
'%row_in%'=function(x, criterion){
    cond = build_condition_matrix(criterion, x)
    matrixStats::rowAnys(cond)
}

#' @export
#' @rdname count_if
'%has%' = `%row_in%`

#' @export
#' @rdname count_if
'%col_in%'=function(x, criterion){
    cond = build_condition_matrix(criterion, x)
    res = matrixStats::colAnys(cond)
    names(res) = colnames(cond)
    res
}

#' @export
#' @rdname count_if
sum_if=function(criterion, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    matrixStats::sum2(data, na.rm = TRUE)
}

#' @export
#' @rdname count_if
sum_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    matrixStats::rowSums2(data, na.rm=TRUE)
}


#' @export
#' @rdname count_if
sum_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    res = matrixStats::colSums2(data, na.rm=TRUE)
    names(res) = colnames(data)
    res
}


################################################

#' @export
#' @rdname count_if
mean_if=function(criterion, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    matrixStats::mean2(data, na.rm=TRUE)
}

#' @export
#' @rdname count_if
mean_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    matrixStats::rowMeans2(data, na.rm=TRUE)
}


#' @export
#' @rdname count_if
mean_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    res = matrixStats::colMeans2(data, na.rm=TRUE)
    names(res) = colnames(data)
    res
}


################################################

#' @export
#' @rdname count_if
sd_if=function(criterion, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    if(!is.numeric(data)) stop("sd_if: argument should be numeric.")
    stats::sd(data, na.rm=TRUE)
}

#' @export
#' @rdname count_if
sd_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    matrixStats::rowSds(data, na.rm=TRUE)
}


#' @export
#' @rdname count_if
sd_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    res = matrixStats::colSds(data, na.rm=TRUE)
    names(res) = colnames(data)
    res
}

################################################

#' @export
#' @rdname count_if
median_if=function(criterion, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    if(!is.numeric(data)) stop("median_if: argument should be numeric.")
    stats::median(data, na.rm=TRUE)
}

#' @export
#' @rdname count_if
median_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    matrixStats::rowMedians(data, na.rm=TRUE)
}


#' @export
#' @rdname count_if
median_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    res = matrixStats::colMedians(data, na.rm=TRUE)
    names(res) = colnames(data)
    res
}


###################################################

#' @export
#' @rdname count_if
max_if=function(criterion, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    res = suppressWarnings(max(data, na.rm=TRUE))
    res[!is.finite(res)] = NA
    res
}

#' @export
#' @rdname count_if
max_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    res = matrixStats::rowMaxs(data, na.rm=TRUE)
    res[!is.finite(res)] = NA
    res
}


#' @export
#' @rdname count_if
max_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    res = matrixStats::colMaxs(data, na.rm=TRUE)
    res[!is.finite(res)] = NA
    names(res) = colnames(data)
    res
}

##########################################################

#' @export
#' @rdname count_if
min_if=function(criterion, ..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    res = suppressWarnings(min(data, na.rm=TRUE))
    res[!is.finite(res)] = NA
    res
}

#' @export
#' @rdname count_if
min_row_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    res = matrixStats::rowMins(data, na.rm=TRUE)
    res[!is.finite(res)] = NA
    res
}


#' @export
#' @rdname count_if
min_col_if=function(criterion,..., data = NULL){
    data = fun_if_helper(criterion = criterion, ..., data = data, logical_as_numeric = TRUE)
    res = matrixStats::colMins(data, na.rm=TRUE)
    res[!is.finite(res)] = NA
    names(res) = colnames(data)
    res
}



#' @export
#' @rdname count_if
apply_row_if=function(fun, criterion,..., data = NULL){
    fun = match.fun(fun)
    cond = build_condition_matrix(criterion, ...)
    if (is.null(data)){
        data = flat_list(
            list(...), 
            flat_df = FALSE
        )
        data = do.call(cbind, data)
    } 
    if(!is.matrix(data)) data = as.matrix(data)
    stopifnot(
        NROW(cond)==NROW(data),
        NCOL(cond)==1 || NCOL(cond)==NCOL(data)
    )
    
    rows = seq_len(nrow(data))
    res = lapply(rows, function(each_row){
        filtered_row = data[each_row,][cond[each_row,]]
        fun(filtered_row)
    })
     
    if(any(lengths(res) != 1)){
        stop("'apply_row_if': incorrect result - function returns values with length greater than one.")    
    }
    unlist(res, use.names = FALSE, recursive = TRUE)
}




#' @export
#' @rdname count_if
apply_col_if=function(fun, criterion,..., data = NULL){
    fun = match.fun(fun)
    cond = build_condition_matrix(criterion, ...)
    if (is.null(data)){
        data = flat_list(
            list(...), 
            flat_df = FALSE
        )
        data = do.call(cbind, data)
    } 
    if(!is.matrix(data)) data = as.matrix(data)
    stopifnot(
        NROW(cond)==NROW(data),
        NCOL(cond)==1 || NCOL(cond)==NCOL(data)
    )
    
    cols = seq_len(ncol(data))
    if(NCOL(cond) > 1){
        res = lapply(cols, function(each_col){
            filtered_row = data[, each_col][cond[,each_col]]
            fun(filtered_row)
        })
        
    } else {
        data = data[cond, ,drop = FALSE]
        res = lapply(cols, function(each_col){
            filtered_row = data[, each_col]
            fun(filtered_row)
        })
    }
    if(any(lengths(res) != 1)){
        stop("'apply_col_if': incorrect result - function returns values with length greater than one.")    
    }
    res = unlist(res, use.names = FALSE, recursive = TRUE)
    names(res) = colnames(data)
    res
}

#########################################################
fun_if_helper = function(criterion,..., data, logical_as_numeric = FALSE){
    if(is.null(data)){
        args = flat_list(
            list(...), 
            flat_df = FALSE
        )
        data = do.call(cbind, args)
    }
    if(!is.matrix(data)) data = as.matrix(data)
    if(logical_as_numeric && is.logical(data)) storage.mode(data) = "integer"
    cond = build_condition_matrix(criterion, ...)
    stopifnot(
        NROW(cond)==NROW(data),
        NCOL(cond)==1 || NCOL(cond)==NCOL(data)
    )
    data[!cond] = NA
    data

}


build_condition_matrix = function(criterion, ..., logical_as_numeric = FALSE){
    cond = flat_list(
        list(...), 
        flat_df = FALSE
    )

    if(!inherits(criterion, "criterion")) criterion = as.criterion(criterion)
    cond = apply_criterion(cond, criterion)
    cond = do.call(cbind, cond)
    if(!is.matrix(cond)) cond = as.matrix(cond)
    if(logical_as_numeric && is.logical(cond)) storage.mode(cond) = "integer"
    cond
}

# optimization after profiling
apply_criterion = function(obj, crit){
    UseMethod("apply_criterion")    
}

#' @export
apply_criterion.list = function(obj, crit){
    lapply(obj, apply_criterion, crit)   
}

#' @export
apply_criterion.data.frame = function(obj, crit){
    obj[] = lapply(obj, apply_criterion, crit)   
    obj
}


#' @export
apply_criterion.default = function(obj, crit){
    crit(obj)
}

#' @export
apply_criterion.matrix = function(obj, crit){
    res = crit(obj)
    res = matrix(res, nrow = nrow(obj), ncol = ncol(obj))
    dimnames(res) = dimnames(obj)
    res
}
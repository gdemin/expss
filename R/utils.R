## stop if condition with message
stopif = function(cond,...){
    if (isTRUE(cond)) {
        stop(do.call(paste0,c(list(...))),call. = FALSE)
    }
    invisible()
}


######## build_criterion ###########
# TODO Удалить
#' @export
build_criterion = function(criterion,dfs){
    # dfs should be data.frame
    # build criterion should return logical matrix with the form of dfs (e. g. the same dimension)
    if (is.null(criterion)){
        return(!is.na(dfs))
    } 
    UseMethod("build_criterion")
}

# TODO Удалить
#' @export
build_criterion.function = function(criterion,dfs){
    res = lapply(dfs,function(colmn){
        cond = criterion(colmn)
        stopif(length(cond)!=length(colmn),"Cells number of criterion doesn't equal cells number of argument.")
        if_na(as.logical(cond), FALSE)
    })
    do.call(cbind,res)
}

# TODO Удалить
#' @export
build_criterion.default = function(criterion,dfs){
    
    build_criterion.function(function(x) x %in% criterion,dfs)
}


#' @export
build_criterion.logical = function(criterion,dfs){
    # uncertainty if criterion is result of something is.na(dfs[,i]) 
    # should we count NA in such case - possible solution - forbid logical criterion for count if
    if (is.atomic(criterion) && (length(criterion)==1) && is.na(criterion)) {
        return(build_criterion(as.numeric(criterion), dfs))
    }
    check_conformance(dfs, criterion)
    res = matrix(nrow = NROW(dfs), ncol = NCOL(dfs))
    if(NCOL(criterion)>1){
        for(i in seq_along(dfs)){
            res[,i] = criterion[,i] 
        }
    } else {
        for(i in seq_along(dfs)){
            res[,i] = criterion 
        }
    }
    res
}

#' @export
build_criterion.data.frame = function(criterion,dfs){
    build_criterion(as.matrix(criterion), dfs)
}

#' @export
build_criterion.matrix = function(criterion,dfs){
    stopif(!is.logical(criterion), "matrix/data.frame criterion should be logical.")
    build_criterion.logical(criterion, dfs)
}

#' @export
build_criterion.list = function(criterion,dfs){
    stop("Condition of type 'list' doesn't supported.")
    #stopif(length(criterion)==0, "Zero-length list is provided as argument.")
    #res = lapply(seq_along(criterion), function(i) build_criterion(column(criterion, i), as.data.frame(column(dfs, i))))
    #do.call(cbind, res)
}

# TODO Удалить
#' @export
build_criterion.criterion = function(criterion,dfs){
    build_criterion.function(criterion,dfs) 
}


###### check_conformance ################

# value should be ncol(value)==1 or ncol(value) = ncol(x) 
# value should be nrow(value)==1 or nrow(value) = nrow(x) 
check_conformance = function(x,value){
    UseMethod("check_conformance")
}

check_conformance.default = function(x,value){
    stopif(length(value)==0, "'value' has zero length.")
    if(is.list(value) && !is.data.frame(value)){
        stopif(length(value)>1 && NCOL(x)!=length(value), "Length of 'value' should be 
               1 or equals to number of columns of 'x' but length(value)=",length(value),", NCOL(x)=", NCOL(x))
    } else {
        stopif(NCOL(value)>1 && NCOL(x)!=NCOL(value), "Number of columns in 'value' should be 
               1 or equals to number of columns of 'x' but NCOL(value)=",NCOL(value),", NCOL(x)=", NCOL(x))
        stopif(NROW(value)>1 && NROW(x)!=NROW(value), "Number of rows in 'value' should be
               1 or equals number of rows of 'x' but NROW(value)=",NROW(value),", NROW(x)=", NROW(x))
    }
    invisible(TRUE)
}

check_conformance.list = function(x, value){
    
    invisible(TRUE)    
}

####### column ###########

column = function(x, column_num, condition = NULL){
    UseMethod("column")
}

column.data.frame = function(x, column_num, condition = NULL){
    stopif(column_num>ncol(x) && ncol(x)>1, "Too large column_num:",column_num, " but only ", ncol(x), " columns in the data.frame.")
    if (ncol(x)>1) {
        res = x[[column_num]]
    } else {
        res = x[[1]]
    }    
    if(!is.null(condition) && nrow(x)>1){
        res[condition]
    } else {
        res
    }    
}

column.matrix = function(x, column_num, condition = NULL){
    stopif(column_num>ncol(x) && ncol(x)>1, "Too large column_num:",column_num, " only ", ncol(x), " columns in the matrix.")
    if (ncol(x)>1) {
        res = x[,column_num]
    } else {
        res = x[,1]
    } 
    if(!is.null(condition) && nrow(x)>1){
        res[condition]
    } else {
        res
    } 
}


column.list = function(x, column_num, condition = NULL){
    stopif(column_num>length(x) && length(x)>1, "Too large column_num:",column_num, " but only ", ncol(x), " elements in the list.")
    stopif(!is.null(condition), "Extract column from list with condition doesn't allowed.")
    if (length(x)>1) {
        x[[column_num]]
    } else {
        x[[1]]
    }  
}

column.default = function(x, column_num, condition = NULL){
    if(is.null(condition)){
        x
    } else {
        if(length(x)>1){
            x[condition]
        }  else {
            x
        }  
    }     
    
}    

#######
"column<-" = function(x, column_num, condition = NULL, value){
    UseMethod("column<-")
}

"column<-.data.frame" = function(x, column_num, condition = NULL, value){
    stopif(column_num>ncol(x), "Too large column_num:",column_num, " only ", ncol(x), " columns in the data.frame.")
    
    if(!is.null(condition)){
        x[condition, column_num] = value
    } else {
        x[,column_num] = value
    }
    x
}

"column<-.matrix" = function(x, column_num, condition = NULL, value){
    stopif(column_num>ncol(x), "Too large column_num:",column_num, " only ", ncol(x), " columns in the matrix.")
    if(!is.null(condition)){
        x[condition, column_num] = value
    } else {
        x[,column_num] = value
    }
    x
}


"column<-.list" = function(x, column_num, condition = NULL, value){
    stop("Assignment for list doesn't implemented.")
    
}

"column<-.default" = function(x, column_num, condition = NULL, value){
    if(is.null(condition)){
        x[] = value
    } else {
        x[condition] = value
    }     
    x
}  


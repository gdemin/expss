

# TODO Удалить
#' @export
build_criterion = function(criterion,dfs){
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

# @export
build_criterion.character = function(criterion,dfs){
    has_logical_operator =  any(grepl("^(==|!=|>=|<=|>|<)", criterion, perl = TRUE))
    if(length(criterion)==1 && has_logical_operator){
        operator = gsub("^(==|!=|>=|<=|>|<).*$", "\\1", criterion, perl = TRUE)
        
        argument = gsub("^(==|!=|>=|<=|>|<)(.*)$", "\\2", criterion, perl = TRUE)
        operator = match.fun(operator)
        argument = type.convert(argument, numerals = "no.loss")
        if (is.factor(argument)) argument = as.character(argument)
        build_criterion.function(function(x) operator(x, argument), dfs)
    } else {
        if (length(criterion)>1 && has_logical_operator){
            warning('Only scalar logical operators (e. g. ">5") are allowed. Vector logical operators are ignored.')
        }
        build_criterion.function(function(x) x %in% criterion,dfs)
    }
}

# @export
build_criterion.logical = function(criterion,dfs){
    # uncertainty if criterion is result of something is.na(dfs[,i]) 
    # should we count NA in such case - possible solution - forbid logical criterion for count if
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

# @export
build_criterion.data.frame = function(criterion,dfs){
    stop("criteria of type data.frame is not allowed.")
}

# @export
build_criterion.list = function(criterion,dfs){
    stopif(length(criterion)==0, "Zero-length list is provided as argument.")
    res = TRUE
    for(i in seq_along(criterion)){
            res = res & build_criterion(criterion[[i]], dfs)
    }
    res
}


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

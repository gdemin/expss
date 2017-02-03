## stop if condition with message
stopif = function(cond,...){
    if (cond) {
        stop(do.call(paste0,c(list(...))),call. = FALSE)
    }
    invisible()
}


######## build_criterion ###########
build_criterion = function(criterion,dfs){
    # dfs should be data.frame
    # build criterion should return logical matrix with the form of dfs (e. g. the same dimension)
    UseMethod("build_criterion")
}

#' @export
build_criterion.function = function(criterion,dfs){
    res = lapply(dfs,function(colmn){
        cond = criterion(colmn)
        stopif(length(cond)!=length(colmn),"Cells number of criterion doesn't equal cells number of argument.")
        if_na(as.logical(cond), FALSE)
    })
    do.call(cbind,res)
}

#' @export
build_criterion.default = function(criterion,dfs){
    res = lapply(dfs, function(colmn){
        if(("POSIXct" %in% class(colmn)) & !("POSIXct" %in% class(criterion))){
            criterion = as.POSIXct(criterion)
        }
        colmn %in% criterion
    })
    do.call(cbind,res)
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
    if_na(res, FALSE)
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

#' @export
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

#' @export
check_conformance.list = function(x, value){
    
    invisible(TRUE)    
}

####### column ###########

column = function(x, column_num, condition = NULL){
    UseMethod("column")
}

#' @export
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

#' @export
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

#' @export
column.list = function(x, column_num, condition = NULL){
    stopif(column_num>length(x) && length(x)>1, "Too large column_num:",column_num, " but only ", length(x), " elements in the list.")
    # stopif(!is.null(condition), "Extract column from list with condition doesn't allowed.")
    if (length(x)>1) {
        x[[column_num]]
    } else {
        x[[1]]
    }  
}

#' @export
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

#' @export
"column<-.data.frame" = function(x, column_num, condition = NULL, value){
    stopif(column_num>ncol(x), "Too large column_num:",column_num, " only ", ncol(x), " columns in the data.frame.")
    
    if(!is.null(condition)){
        x[condition, column_num] = value
    } else {
        x[,column_num] = value
    }
    x
}

#' @export
"column<-.matrix" = function(x, column_num, condition = NULL, value){
    stopif(column_num>ncol(x), "Too large column_num:",column_num, " only ", ncol(x), " columns in the matrix.")
    if(!is.null(condition)){
        x[condition, column_num] = value
    } else {
        x[,column_num] = value
    }
    x
}

#' @export
"column<-.list" = function(x, column_num, condition = NULL, value){
    stop("Assignment for list doesn't implemented.")
    
}

#' @export
"column<-.default" = function(x, column_num, condition = NULL, value){
    if(is.null(condition)){
        x[] = value
    } else {
        x[condition] = value
    }     
    x
}  

#' @export
"column<-.factor" = function(x, column_num, condition = NULL, value){
    fac_levels = levels(x)
    if(!all(value %in% fac_levels)){
        fac_levels = union(fac_levels, value)
        levels(x) = fac_levels
    }
    if(is.null(condition)){
        x[] = value
    } else {
        x[condition] = value
    }     
    x
}  

###########################
# use this function only inside other functions
# eval_dynamic_scoping = function(expr, envir, skip_up_to_frame = ""){
#     all_env = rev(sys.frames())[-(1:2)] # remove current and calling environement
#     sys_calls = lapply(rev(sys.calls())[-(1:2)], function(each_call){
#         res = as.character(as.list(each_call)[[1]])
#         if(res[1] %in% c("::", ":::")){
#             res[3]
#         } else {
#             res[1]
#         }
#     })
#     sys_calls = unlist(sys_calls)
#     skip = stats::na.omit(match(skip_up_to_frame, sys_calls))
#     if(length(skip)==0) {
#         skip = 0
#     } else {
#         skip = max(skip)
#     }    
#     
#     if(skip>0){
#         all_env = c(all_env[-seq_len(skip)], .GlobalEnv) 
#     } else {
#         all_env = c(all_env, .GlobalEnv) 
#     }
#     
#     succ = FALSE
#     i = 1
#     while(!succ && i<=length(all_env)){
#         succ = TRUE
#         parent.env(envir) = all_env[[i]]
#         res = tryCatch(eval(expr, envir), error = function(e) {succ<<-FALSE})
#         if(!succ) i = i + 1
#     }
#     stopif(!succ, "`", deparse(substitute(expr)),"` - some variables not found.")
#     res
# }



#############################

#########################################

valid = function(x){
    UseMethod("valid")
}

#' @export
valid.default = function(x){
    !is.na(x)
}

#' @export
valid.matrix = function(x){
    rowSums(!is.na(x))>0
}

#' @export
valid.data.frame = function(x){
    rowSums(!is.na(x))>0
}


###########

prepare_env = function(env, n, column_names){
    env$.n = n
    env$.N = n
    env$set = set_generator(env$.N)
    if(!is.null(column_names)){
        env$.internal_column_names0 = column_names
        lockBinding(".internal_column_names0", env)
    }
    lockBinding(".n", env)
    lockBinding(".N", env)
    lockBinding("set", env)    
    
}

clear_env = function(env){
    rm(".n", "set", ".N", envir = env)  
    if(exists(".internal_column_names0", envir = env)) rm(".internal_column_names0", envir = env)
}

# we need this function to keep variables in order of data.frame
internal_ls = function(column_names, env = parent.frame()){
        curr = ls(envir = env, all.names = TRUE, sorted = FALSE)
        curr = curr %d% c(".n", "set", ".N", ".internal_column_names0")
        # removed = names(curr)[vapply(curr, is.null, NA, USE.NAMES = FALSE)]
        # curr = names(curr) %d% removed # remove deleted variables?
        new_names = column_names %i% curr 
        curr = curr %d% new_names
        new_names %u% rev(curr)
    }


### TRUE if argument is list, not data.frame
is_list=function(x)
    
{
    is.list(x) && (!is.data.frame(x))
}


##################
uniq_elements=function(x)
{
    UseMethod("uniq_elements")
}

uniq_elements.default=function(x){
    unique(x)
}

uniq_elements.matrix=function(x){
    unique(c(x))
}

uniq_elements.data.frame=function(x){
    unique(unlist(lapply(x, unique)))
}

uniq_elements.list=function(x){
    unique(unlist(lapply(x, uniq_elements)))
}


#######
integer_encoding=function(x, dict = NULL)
{
    UseMethod("integer_encoding")
}

integer_encoding.default=function(x, dict = NULL){
    if(is.null(dict)) dict = sort(uniq_elements(x))
    matrix(match(x, dict, incomparables=NA))
}

integer_encoding.matrix=function(x, dict = NULL){
    if(is.null(dict)) dict = sort(uniq_elements(x))
    matrix(match(x, dict, incomparables=NA), nrow = nrow(x))
}

integer_encoding.data.frame=function(x, dict = NULL){
    if(is.null(dict)) dict = sort(uniq_elements(x))
    matrix(match(unlist(x), dict, incomparables=NA), nrow = nrow(x))
}

## Flatten list
### list(a,list(b,c))->list(a,b,c)
### flat_df = FALSE data.frame will be left as data.frame
### flat_df = TRUE data.frame will be converted to list
flat_list=function(x, flat_df = FALSE)
{
    if(flat_df){
        check_list = is.list
    } else {
        check_list = is_list
    }
    if(is.null(x)) return(NULL)
    if(!check_list(x)) return(list(x))
    need_unlist=vapply(x, check_list, FUN.VALUE = logical(1))
    if (any(need_unlist)) {
        res=lapply(x,function(elem){
            if (check_list(elem)){
                flat_list(elem, flat_df = flat_df)
            } else list(elem)
            
        })
        do.call(c, res)
    } else as.list(x)
    
}

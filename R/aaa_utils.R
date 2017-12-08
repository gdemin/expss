# data.table = data.table::data.table
# as.data.table = data.table::as.data.table
# '[.data.table' = data.table::`[.data.table`

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
    column(x[[column_num]], 1, condition = condition) = value
    x
}

#' @export
"column<-.matrix" = function(x, column_num, condition = NULL, value){
    stopif(column_num>ncol(x), "Too large column_num:",column_num, " only ", ncol(x), " columns in the matrix.")
    column(x[, column_num], 1, condition = condition) = value
    x
}

#' @export
"column<-.list" = function(x, column_num, condition = NULL, value){
    stop("Assignment for list doesn't implemented.")
    
}

#' @export
"column<-.default" = function(x, column_num, condition = NULL, value){
    if(is.factor(value)){
        x = as.factor(x)
        column(x, column_num = column_num, condition) = value
        return(x)
    } 
    
    if(inherits(value, "POSIXct") && (is.logical(x) || is.numeric(x))){
        # first assignment - we expect that x with all NA and set its class to POSIXct
        x = as.POSIXct(x)
        column(x, column_num = column_num, condition) = value
        return(x)
    } 
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
valid.data.frame = function(x){
    if (length(x)) {
        res = do.call(cbind, lapply(x, is.na))
    } else {
        res = matrix(FALSE, NROW(x), 0)
    }    
    !rowAlls(res)
}

#' @export
valid.dichotomy = function(x){
    rowSums(x, na.rm = TRUE)>0
}

#' @export
valid.matrix = function(x){
    !rowAlls(is.na(x))
}




###########

prepare_env = function(env, n, column_names){
    env$.n = n
    env$.N = n
    env$.. = expss::..
    env$.new_var = new_var_generator(function(x) rep(NA, x), env$.N)
    env$.new_character = new_var_generator(character, env$.N)
    env$.new_numeric = new_var_generator(numeric, env$.N)
    env$.new_logical = new_var_generator(logical, env$.N)
    if(!is.null(column_names)){
        env$.internal_column_names0 = column_names
        lockBinding(".internal_column_names0", env)
    }
    lockBinding(".n", env)
    lockBinding(".N", env)
    lockBinding(".new_var", env)
    lockBinding(".new_character", env)
    lockBinding(".new_numeric", env)
    lockBinding(".new_logical", env)

}

clear_env = function(env){
    rm(".n", ".N", "..", 
       ".new_var", ".new_character", 
       ".new_numeric", ".new_logical",
       envir = env)  
    if(exists(".internal_column_names0", envir = env)) rm(".internal_column_names0", envir = env)
}


# we need this function to keep variables in order of data.frame
get_current_variables = function(envir){
        if(exists(".internal_column_names0", envir =envir)){
            column_names = envir[[".internal_column_names0"]]
            curr = ls(envir = envir, all.names = TRUE, sorted = FALSE)
            curr = curr %d% c(".n",  ".N", ".internal_column_names0", 
                              "..", ".new_var", ".new_character", ".new_numeric", ".new_logical")
            # removed = names(curr)[vapply(curr, is.null, NA, USE.NAMES = FALSE)]
            # curr = names(curr) %d% removed # remove deleted variables?
            new_names = column_names %i% curr 
            curr = curr %d% new_names
            new_names %u% rev(curr)
        } else {
            ls(envir = envir)
        }

}

########################

new_var_generator = function(FUN, number_of_rows){
    force(number_of_rows)
    function(){
       FUN(number_of_rows)
    }
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

#' @export
uniq_elements.default=function(x){
    unique(x)
}

#' @export
uniq_elements.matrix=function(x){
    unique(c(x, use.names = FALSE))
}

#' @export
uniq_elements.data.frame=function(x){
    unique(unlist(lapply(x, unique), use.names = FALSE))
}

#' @export
uniq_elements.list=function(x){
    unique(unlist(lapply(x, uniq_elements), use.names = FALSE))
}


#######
integer_encoding=function(x, dict = NULL)
{
    UseMethod("integer_encoding")
}

#' @export
integer_encoding.default=function(x, dict = NULL){
    if(is.null(dict)) dict = sort(uniq_elements(x))
    matrix(fast_match(x, dict, NA_incomparable = TRUE), nrow = NROW(x))
}

#' @export
integer_encoding.data.frame=function(x, dict = NULL){
    if(is.null(dict)) dict = sort(uniq_elements(x))
    matrix(fast_match(c(x, recursive = TRUE, use.names = FALSE), 
                      dict, 
                      NA_incomparable = TRUE),
           nrow = nrow(x))
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


####

"insert_value_before<-" = function(x, needle, value){
    needle_pos = which(x %in% needle)
    if(length(needle_pos)){
        needle_pos = needle_pos[1]
        append(x, value, after = needle_pos - 1)
    } else {
        x
    }
}


"insert_value_after<-" = function(x, needle, value){
    needle_pos = which(x %in% needle)
    if(length(needle_pos)){
        needle_pos = needle_pos[1]
        append(x, value, after = needle_pos)
    } else {
        x
    }
}



## round all numerics in the data.frame
round_dataframe = function(x, digits = NULL){
    if(is.null(digits)) digits = 1
    if(is.na(digits)) return(x)
    for (i in seq_len(NCOL(x))){
        if(is.numeric(x[[i]])){
            x[[i]] = round(x[[i]], digits)
        }
    }
    x
}

format_to_character = function(x, digits = NULL){
    if(is.null(digits)) digits = 1
    if(is.na(digits)) return(x)
    res = format(x, nsmall = digits)
    res[is.na(x)] = ""
    res
}



#####

make_items_unique = function(x, with_warning = NULL, sep = "_"){
    if(!is.character(x)) x = as.character(x)
    if(length(x)<2) return(x)
    if(!is.null(with_warning)){
        if (anyDuplicated(x)){
            duplicates = duplicated(x)
            warning(paste0(with_warning, paste(paste0("'", x[duplicates], "'"), collapse = "', '")))
            make.unique(x, sep = sep)
        } else {
            x
        }
    } else {
        make.unique(x, sep = sep)
    }
}


#################

if_null = function(x, value){
    if(is.null(x)){
        value
    } else {
        x
    }
}



##### 

expr_to_character = function(expr){
    res = deparse(expr, width.cutoff = 500L)
    if(length(res)>1) res = paste(res, collapse = " ")
    res
}

# if item of list is character then it will be convereted to name
convert_characters_to_names = function(list_of_items){
    flat_list(lapply(list_of_items, function(curr_symbol) {
        if(length(curr_symbol)>1){
            convert_characters_to_names(curr_symbol)
        } else {
            if(is.character(curr_symbol)){
                as.name(curr_symbol)
            } else {
                curr_symbol   
            }
        }
    }))
}


# expr - expression as after 'substitute'
# symbols - named list  - names will be substituted with values 
substitute_symbols = function(substitute_result, symbols) {
    eval(bquote(substitute(.(substitute_result), symbols)))
}

convert_top_level_symbols_to_characters = function (as_list_substitute) {
    lapply(as_list_substitute, function(elem){
             if((length(elem)<=1) && is.symbol(elem)){
                as.character(elem) 
            } else {
                elem
            }
    })
}

##################################
## return vector of integers - positions of columns
## variables_names = substitute(list(...))
variables_names_to_indexes = function(curr_names, variables_names, envir, symbols_to_characters = TRUE){
    variables_names = evaluate_variable_names(variables_names, 
                                              envir = envir, 
                                              symbols_to_characters = symbols_to_characters)
    create_indexes_from_evaluated_names(curr_names, variables_names)
   
}

create_indexes_from_evaluated_names = function(curr_names, variables_names){
    keep_indexes = numeric(0)
    characters_names = character(0) # for checking non-existing names
    numeric_indexes = numeric(0) # for checking non-existing indexes
    for (each in variables_names){
        if(is.character(each)){
            next_indexes = which(curr_names %in% each)
            characters_names = c(characters_names, each)
        } else {
            if(is.numeric(each)){
                next_indexes = each
                numeric_indexes = c(numeric_indexes, each)
            } else {
                next_indexes = which(curr_names %in% (curr_names %i% each))
            }
        }
        keep_indexes = c(keep_indexes, next_indexes %d% keep_indexes)
    }
    if(anyDuplicated(characters_names)){
        warning("duplicated names: ",
                paste(characters_names[duplicated(characters_names)], collapse = ","),
                ". Repeated names are ignored."
                
        )
    }
    if(anyDuplicated(numeric_indexes)){
        warning("duplicated indexes: ",
                paste(numeric_indexes[duplicated(numeric_indexes)], collapse = ","),
                ". Repeated indexes are ignored."
                
        )
    }
    stopif(any(!(characters_names %in% curr_names)), 
           "names not found: '", paste(characters_names %d% curr_names, collapse = "', '"),"'")
    stopif(any(numeric_indexes > length(curr_names), na.rm = TRUE), 
           "indexes are greater then number of items: ", paste(numeric_indexes %i% gt(length(curr_names)), collapse = ", "))
    keep_indexes
}

## variables_names = substitute(list(...))
evaluate_variable_names = function(variables_names, envir, symbols_to_characters = TRUE){
    variables_names = substitute_symbols(variables_names,
                                         list("%to%" = expr_internal_to,
                                              ".." = expr_internal_parameter)
    )
    if(symbols_to_characters){
        variables_names = as.list(variables_names)
        variables_names[-1] = convert_top_level_symbols_to_characters(variables_names[-1])
        variables_names = as.call(variables_names)
    }
    variables_names = eval(variables_names, envir = envir,
                           enclos = baseenv())
    variables_names = rapply(variables_names, function(item) {
        if(length(item)>1 && !inherits(item, "formula") && !is.function(item)) {
            as.list(item)
        } else {
            item
        }
    }, how = "replace")
    flat_list(variables_names)
}



####################

fast_match = function(x, table, nomatch = NA_integer_, NA_incomparable = FALSE){
    if(is.character(x) && is.character(table)){
        ind = chmatch(x, table, nomatch = nomatch) 
        if(NA_incomparable) {
            ind[is.na(x)] = nomatch
        }
    } else {
        if(NA_incomparable) {
            ind = match(x, table, 
                        nomatch = nomatch, 
                        incomparables = NA)
        } else {
            ind = match(x, table,
                        nomatch = nomatch, 
                        incomparables = NULL)
        }    
    }
    ind
}
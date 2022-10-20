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




ENV_INTERNAL_NAMES = c(".n", ".N", "..", 
                       ".new_var", ".new_character", 
                       ".new_numeric", ".new_logical")
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
    lapply(ENV_INTERNAL_NAMES %d% "..", lockBinding, env)
}

clear_env = function(env){
    rm(list = ENV_INTERNAL_NAMES,
       envir = env)  
    if(exists(".internal_column_names0", envir = env)) rm(".internal_column_names0", envir = env)
}


# we need this function to keep variables in order of data.frame
get_current_variables = function(envir){
        if(exists(".internal_column_names0", envir =envir)){
            column_names = envir[[".internal_column_names0"]]
            curr = ls(envir = envir, all.names = TRUE, sorted = FALSE)
            curr = curr %d% c(ENV_INTERNAL_NAMES, ".internal_column_names0")
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
is_list=function(x){
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
flat_list=function(x, flat_df = FALSE){
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
    if (anyDuplicated(x)){
        duplicates = duplicated(x)
        is.null(with_warning) || warning(paste0(with_warning, paste(paste0("'", x[duplicates], "'"), collapse = "', '")))
        while(anyDuplicated(x)){
            x[duplicates] = paste0(x[duplicates], "|")   
            duplicates = duplicated(x)
        }
        
    } 
    x
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

fast_in = function(x, value){
    if(is.numeric(value) && 
       length(value)>0 && 
       length(value)<3 && 
       !anyNA(value) && 
       !any(is.infinite(value)) && 
       is.numeric(x)){
        # optimization for very special and very frequent case after profiling
        res = x == value[[1]]
        for(each in value[-1]){
            res = res | (x==each)
        }
        res & !is.na(res)
    } else {
        fast_match(x, value, nomatch = 0L)>0
    }
}

#################

add_class = function(x, ...){
    new_class = unlist(list(...))
    class(x) = union(new_class, class(x))
    x
}

remove_class = function(x, ...){
    new_class = unlist(list(...))
    class(x) = setdiff(class(x), new_class)
    x
}

############



#' Bug workaround
#' 
#' Function is added to workaround strange bug with data.table (issue #10).
#' @param ... arguments
#'
#' @return list 
#' @export
#'
## copied from https://github.com/Rdatatable/data.table/blob/master/R/utils.R
## added exclusively to workaround strange bug with data.table (issue #10)
name_dots <- function(...) {
    dot_sub <- as.list(substitute(list(...)))[-1L]
    vnames = names(dot_sub)
    if (is.null(vnames)) {
        vnames = rep.int("", length(dot_sub))
        novname = rep.int(TRUE, length(dot_sub))
    } else {
        vnames[is.na(vnames)] = ""
        if (any(vnames==".SD")) stop("A column may not be called .SD. That has special meaning.")
        novname = vnames==""
    }
    for (i in which(novname)) {
        if ((tmp <- deparse(dot_sub[[i]])[1L]) == make.names(tmp))
            vnames[i] = tmp
    }
    still_empty = vnames==""
    if (any(still_empty)) vnames[still_empty] = paste0("V", which(still_empty))
    list(vnames=vnames, novname=novname)
}

# dots result of substitute(list(...))
# return list of expressions 
get_named_expressions = function(dots){
    res = as.list(dots)[-1]
    vnames = names(res)
    if (is.null(vnames)) {
        vnames = rep.int("", length(res))
    } else {
        vnames[is.na(vnames)] = ""
    }
    setNames(res, vnames)
}

############

# for assignment functions
# to avoid warning about shallow copy
fix_datatable = function(x){
    if(is.data.table(x)) setDT(x, check.names = FALSE)
    x
}

######

set_names = function (object = nm, nm){
    names(object) <- nm
    object
}
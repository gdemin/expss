#' Keep or drop elements by name/criteria in data.frame/matrix/list/vector
#' 
#' \code{keep} selects variables/elements from data.frame by their names or by 
#' criteria (see \link{criteria}). \code{except} drops  variables/elements from 
#' data.frame by their names or by criteria. For non-infix versions of functions
#' names at the top-level will be used as is (non-standard evaluation). For 
#' standard evaluation of parameters you can surround them by round brackets.
#' See example. Methods for list will apply \code{keep}/\code{except} to each
#' element of the list separately. \code{.keep}/\code{.except} are versions
#' which works with \link{default_dataset}.
#'
#' @param data data.frame/matrix/list/vector
#' @param ... column names of type character/numeric or criteria/logical functions
#'
#' @return object of the same type as \code{data}
#' @export
#'
#' @examples
#' keep(iris, Sepal.Length, Sepal.Width)  
#' except(iris, Species)
#' 
#' keep(iris, Species, other()) # move 'Species' to the first position
#' keep(iris, to("Petal.Width")) # keep all columns up to 'Species'
#' 
#' except(iris, perl("^Petal")) # remove columns which names start with 'Petal'
#' 
#' except(iris, 5) # remove fifth column
#' 
#' data(airquality)
#' keep(airquality, from("Ozone") & to("Wind")) # keep columns from 'Ozone' to 'Wind'
#' keep(airquality, Ozone %to% Wind) # the same result
#' 
#' # character expansion
#' dfs = data.frame(
#'      a = 10 %r% 5,
#'      b_1 = 11 %r% 5,
#'      b_2 = 12 %r% 5,
#'      b_3 = 12 %r% 5,
#'      b_4 = 14 %r% 5,
#'      b_5 = 15 %r% 5 
#'  )
#'  i = 1:5
#'  keep(dfs, b_1 %to% b_5) 
#'  keep(dfs, subst("b_`i`")) # the same result
#'  
#'  # standard and non-standard evaluation
#'  data(mtcars)
#'  # generally it is not recommended to name parameter as column in data.frame
#'  am = "vs" 
#'  # non-standard evaluation of top-level items
#'  keep(mtcars, am) # we get 'am' column
#'  # but standard evaluation of other items
#'  keep(mtcars, (am)) # we get 'vs' column
#'  
#'  many_vars = c("am", "vs", "cyl")
#'  keep(mtcars, (many_vars))
#'  
keep = function(data, ...){
    variables_names = substitute(list(...))
    keep_internal(data, variables_names, envir = parent.frame())
}

keep_internal = function(data, variables_names, envir){
    UseMethod("keep_internal")
}

#' @export
keep_internal.list = function(data, variables_names, envir){
    for(each in seq_along(data)){
        data[[each]] = keep_internal(data[[each]], variables_names, envir = envir)
    }
    data
}

#' @export
keep_internal.data.frame = function(data, variables_names, envir){
    curr_names = colnames(data)
    new_vars = keep_helper(curr_names, variables_names, envir = envir)
    if(is.data.table(data)){
        res = data[ , new_vars, with = FALSE]
    } else {
        res = data[ , new_vars, drop = FALSE]
        colnames(res) = curr_names[new_vars] # prevents names correction    
    }
   
    res
}

#' @export
keep_internal.matrix = function(data, variables_names, envir){
    curr_names = colnames(data)
    new_vars = keep_helper(curr_names, variables_names, envir = envir)
    res = data[ , new_vars, drop = FALSE]
    colnames(res) = curr_names[new_vars] # prevents names correction
    res
}

#' @export
#' @rdname keep
'%keep%' = keep

#' @export
#' @rdname keep
.keep = function(...){
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    variables_names = substitute(list(...))
    data = keep_internal(data, variables_names, envir = parent.frame())
    ref(reference) = data
    invisible(data)
}


#' @export
#' @rdname keep
except = function(data, ...){
    variables_names = substitute(list(...))
    except_internal(data, variables_names, envir = parent.frame())
}


except_internal = function(data, variables_names, envir){
    UseMethod("except_internal")
}


#' @export
except_internal.list = function(data, variables_names, envir){
    for(each in seq_along(data)){
        data[[each]] = except_internal(data[[each]], variables_names, envir = envir)
    }
    data
}

#' @export
except_internal.data.frame = function(data, variables_names, envir){
    curr_names = colnames(data)
    new_vars = keep_helper(curr_names, variables_names, envir = envir)
    new_vars = -unique(new_vars)
    if(length(new_vars)==0){
        return(data)
    }
    if(is.data.table(data)){
        res = data[ , new_vars, with = FALSE]
    } else {
        res = data[ , new_vars, drop = FALSE]
        colnames(res) = curr_names[new_vars] # prevents names correction    
    }
    res
}

#' @export
except_internal.matrix = function(data, variables_names, envir){
    curr_names = colnames(data)
    new_vars = keep_helper(curr_names, variables_names, envir = envir)
    new_vars = -unique(new_vars)
    if(length(new_vars)==0){
        return(data)
    }
    res = data[ , new_vars, drop = FALSE]
    colnames(res) = curr_names[new_vars] # prevents names correction
    res
}


#' @export
#' @rdname keep
'%except%' = except



#' @export
#' @rdname keep
.except = function(...){
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    variables_names = substitute(list(...))
    data = except_internal(data, variables_names, envir = parent.frame())
    ref(reference) = data
    invisible(data)
}

## return vector of integers - positions of columns
keep_helper = function(curr_names, variables_names, envir){
    variables_names = substitute_symbols(variables_names,
                              list("%to%" = as.name(".internal_to_"))
    )
    variables_names = substitute_symbols(variables_names,
                                         setNames(curr_names, curr_names),
                                         top_level_only = TRUE
    )
    variables_names = as.list(variables_names)
    variables_names[-1] = convert_top_level_symbols_to_characters(variables_names[-1])
    variables_names = eval(as.call(variables_names), envir = envir,
                enclos = baseenv())
    variables_names = rapply(variables_names, function(item) {
        if(length(item)>1) {
            as.list(item)
        } else {
            item
        }
    }, how = "replace")
    variables_names = flat_list(variables_names)
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
           "'keep'/'except' - names not found: '", paste(characters_names %d% curr_names, collapse = "', '"),"'")
    stopif(any(numeric_indexes > length(curr_names), na.rm = TRUE), 
           "'keep'/'except' - indexes are greater then number of columns: '", paste(numeric_indexes %i% gt(length(curr_names)), collapse = "', '"),"'")
    keep_indexes
    
}


    



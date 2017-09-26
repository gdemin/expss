#' Keep or drop elements by name/criteria in data.frame/matrix
#' 
#' \code{keep} selects variables/elements from data.frame by their names or by 
#' criteria (see \link{criteria}). \code{except} drops variables/elements from 
#' data.frame by their names or by criteria. Names at the top-level can be
#' unquoted (non-standard evaluation). For standard evaluation of parameters you
#' can surround them by round brackets. See examples. Methods for list will apply
#' \code{keep}/\code{except} to each element of the list separately.
#' \code{.keep}/\code{.except} are versions which works with
#' \link{default_dataset}.
#'
#' @param data data.frame/matrix/list
#' @param ... column names of type character/numeric or criteria/logical functions
#'
#' @return object of the same type as \code{data}
#' @export
#'
#' @examples
#' data(iris)
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
#' data(mtcars)
#' keep(mtcars, from("mpg") & to("qsec")) # keep columns from 'mpg' to 'qsec'
#' keep(mtcars, mpg %to% qsec) # the same result
#' 
#'  # standard and non-standard evaluation
#'  many_vars = c("am", "vs", "cyl")
#'  \dontrun{
#'  keep(mtcars, many_vars) # error - names not found: 'many_vars'  
#'   }
#'  keep(mtcars, (many_vars)) # ok
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
    new_vars = variables_names_to_indexes(curr_names, variables_names, envir = envir)
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
    if(is.null(curr_names)){
        curr_names = rep("", NCOL(data))
    }
    new_vars = variables_names_to_indexes(curr_names, variables_names, envir = envir)
    res = data[ , new_vars, drop = FALSE]
    if(!is.null(colnames(data))) colnames(res) = curr_names[new_vars] # prevents names correction
    res
}


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
    new_vars = variables_names_to_indexes(curr_names, variables_names, envir = envir)
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
    new_vars = variables_names_to_indexes(curr_names, variables_names, envir = envir)
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
.except = function(...){
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    variables_names = substitute(list(...))
    data = except_internal(data, variables_names, envir = parent.frame())
    ref(reference) = data
    invisible(data)
}




    



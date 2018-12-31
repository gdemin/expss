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
#'  keep(dfs, text_expand("b_{i}")) # the same result  
keep = function(data, ...){
    UseMethod("keep")
}


#' @export
keep.list = function(data, ...){
    for(each in seq_along(data)){
        data[[each]] = eval.parent(substitute(keep(data[[each]], ...)))
    }
    data
}

#' @export
keep.data.frame = function(data, ...){
    variables_names = substitute(list(...))
    curr_names = colnames(data)
    new_vars = variables_names_to_indexes(curr_names, variables_names, envir = parent.frame())
    if(is.data.table(data)){
        res = data[ , new_vars, with = FALSE]
    } else {
        res = data[ , new_vars, drop = FALSE]
        colnames(res) = curr_names[new_vars] # prevents names correction    
    }
    res
}

#' @export
keep.matrix = function(data, ...){
    variables_names = substitute(list(...))
    curr_names = colnames(data)
    if(is.null(curr_names)){
        curr_names = rep("", NCOL(data))
    }
    new_vars = variables_names_to_indexes(curr_names, variables_names, envir = parent.frame())
    res = data[ , new_vars, drop = FALSE]
    if(!is.null(colnames(data))) colnames(res) = curr_names[new_vars] # prevents names correction
    res
}


#' @export
#' @rdname keep
.keep = function(...){
    reference = suppressMessages(default_dataset() )
    variables_names = substitute(list(...))
    data = eval.parent(substitute(keep(ref(reference), ...)))
    ref(reference) = data
    invisible(data)
}


#' @export
#' @rdname keep
except = function(data, ...){
    UseMethod("except")
}



#' @export
except.list = function(data, ...){
    for(each in seq_along(data)){
        data[[each]] = eval.parent(substitute(except(data[[each]], ...)))
    }
    data
}

#' @export
except.data.frame = function(data, ...){
    variables_names = substitute(list(...))
    curr_names = colnames(data)
    new_vars = variables_names_to_indexes(curr_names, variables_names, envir = parent.frame())
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
except.matrix = function(data, ...){
    variables_names = substitute(list(...))
    curr_names = colnames(data)
    new_vars = variables_names_to_indexes(curr_names, variables_names, envir = parent.frame())
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
    data = eval.parent(substitute(except(ref(reference), ...)))
    ref(reference) = data
    invisible(data)
}




    



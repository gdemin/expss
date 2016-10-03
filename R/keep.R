#' Keep or drop elements by name/criteria in data.frame/matrix/list/vector
#' 
#' \code{keep} selects variables/elements from data.frame by their names or by 
#' criteria (see \link{criteria}). \code{except} drops  variables/elements from 
#' data.frame by their names or by criteria. There is no non-standard evaluation
#' in these functions by design so use quotes for names of your variables or use
#' \link{qc}. \code{\%keep\%}/\code{\%except\%} are infix versions of these 
#' functions. \code{.keep}/\code{.except} are versions which works with 
#' \link{deafult_dataset}.
#'
#' @param data data.frame/matrix/list/vector
#' @param ... column names/element names of type character or criteria/logical functions
#' @param variables column names/element names of type character or criteria/logical functions
#'
#' @return object of the same type as \code{data}
#' @export
#'
#' @examples
#' 1
keep = function(data, ...){
    UseMethod("keep")
}

#' @export
keep.default = function(data, ...){
    vars = names(data)
    new_vars = keep_helper(vars, ...)
    data[new_vars]
    
}

#' @export
keep.dataframe = function(data, ...){
    vars = colnames(data)
    new_vars = keep_helper(vars, ...)
    data[ , new_vars, drop = FALSE]
    
}

#' @export
keep.matrix = function(data, ...){
    vars = colnames(data)
    new_vars = keep_helper(vars, ...)
    data[ , new_vars, drop = FALSE]
}

#' @export
#' @rdname keep
except = function(data, ...){
    UseMethod("except")
}

#' @export
except.default = function(data, ...){
    vars = names(data)
    new_vars = vars %d% keep_helper(vars, ...)
    data[new_vars]
    
}

#' @export
except.dataframe = function(data, ...){
    vars = colnames(data)
    new_vars = vars %d% keep_helper(vars, ...)
    data[ , new_vars, drop = FALSE]
    
}

#' @export
except.matrix = function(data, ...){
    vars = colnames(data)
    new_vars = vars %d% keep_helper(vars, ...)
    data[ , new_vars, drop = FALSE]
}



keep_helper = function(x, ...){
    keep_names = character(0)
    old_names = x
    new_names = c(list(...), recursive = TRUE)
    for (each in new_names){
        if(is.character(each)){
            keep_names = c(keep_names, each)
        } else {
            keep_names = c(keep_names, old_names %i% each)
        }
        old_names = old_names %d% each
    }
    stopif(any(!(keep_names %in% x)), "names not found: ", paste(keep_names %d% x, collapse = ", "))
    keep_names
    
}

#' @export
#' @rdname keep
'%keep%' = function(data, variables){
    keep(data, variables)
}

#' @export
#' @rdname keep
'%except%' = function(data, variables){
    keep(data, variables)
}

#' @export
#' @rdname keep
.keep = function(...){
    keep(data, variables)
}

#' @export
#' @rdname keep
.except = function(...){
    keep(data, variables)
}

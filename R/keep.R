#' Keep or drop elements by name/criteria in data.frame/matrix/list/vector
#' 
#' \code{keep} selects variables/elements from data.frame by their names or by 
#' criteria (see \link{criteria}). \code{except} drops  variables/elements from 
#' data.frame by their names or by criteria. There is no non-standard evaluation
#' in these functions by design so use quotes for names of your variables or use
#' \link{qc}. \code{\%keep\%}/\code{\%except\%} are infix versions of these 
#' functions. \code{.keep}/\code{.except} are versions which works with 
#' \link{default_dataset}.
#'
#' @param data data.frame/matrix/list/vector
#' @param ... column names/element names of type character or criteria/logical functions
#' @param variables column names/element names of type character or criteria/logical functions
#'
#' @return object of the same type as \code{data}
#' @export
#'
#' @examples
#' keep(iris, "Sepal.Length", "Sepal.Width")  
#' keep(iris, qc(Sepal.Length, Sepal.Width)) # same result with non-standard eval 
#' except(iris, "Species")
#' 
#' keep(iris, "Species", other) # move 'Species' to the first position
#' keep(iris, to("Petal.Width")) # keep all columns except 'Species'
#' 
#' except(iris, perl("^Petal")) # remove columns which names starts with 'Petal'
#' 
#' keep(airquality, (from("Ozone") & to("Wind"))) # keep columns from 'Ozone' to 'Wind'
#' 
#' # the same examples with infix operators
#' 
#' iris %keep% c("Sepal.Length", "Sepal.Width") 
#' iris %keep% qc(Sepal.Length, Sepal.Width) # same result with non-standard eval
#' iris %except% "Species"
#' 
#' iris %keep% c("Species", other) # move 'Species' to the first position
#' iris %keep% to("Petal.Width")   # keep all columns except 'Species'
#' 
#' iris %except% perl("^Petal")    # remove columns which names starts with 'Petal'
#' 
#' airquality %keep% (from("Ozone") & to("Wind")) # keep columns from 'Ozone' to 'Wind'
#'     
keep = function(data, ...){
    UseMethod("keep")
}

#' @export
keep.default = function(data, ...){
    vars = names(data)
    args = list(...)
    new_vars = keep_helper(vars, args)
    res = data[new_vars]
    names(res) = vars[new_vars] # prevents names correction
    res
}

#' @export
keep.data.frame = function(data, ...){
    vars = colnames(data)
    args = list(...)
    new_vars = keep_helper(vars, args)
    res = data[ , new_vars, drop = FALSE]
    colnames(res) = vars[new_vars] # prevents names correction
    res
}

#' @export
keep.matrix = function(data, ...){
    vars = colnames(data)
    args = list(...)
    new_vars = keep_helper(vars, args)
    res = data[ , new_vars, drop = FALSE]
    colnames(res) = vars[new_vars] # prevents names correction
    res
}

#' @export
#' @rdname keep
except = function(data, ...){
    UseMethod("except")
}

#' @export
except.default = function(data, ...){
    vars = names(data)
    args = list(...)
    new_vars = keep_helper(vars, args)
    new_vars = -unique(new_vars)
    if(length(new_vars)==0){
        return(data)
    }
    res = data[new_vars]
    names(res) = vars[new_vars] # prevents names correction
    res
}

#' @export
except.data.frame = function(data, ...){
    vars = colnames(data)
    args = list(...)
    new_vars = keep_helper(vars, args)
    new_vars = -unique(new_vars)
    if(length(new_vars)==0){
        return(data)
    }
    res = data[ , new_vars, drop = FALSE]
    colnames(res) = vars[new_vars] # prevents names correction
    res
}

#' @export
except.matrix = function(data, ...){
    vars = colnames(data)
    args = list(...)
    new_vars = keep_helper(vars, args)
    new_vars = -unique(new_vars)
    if(length(new_vars)==0){
        return(data)
    }
    res = data[ , new_vars, drop = FALSE]
    colnames(res) = vars[new_vars] # prevents names correction
    res
}



keep_helper = function(old_names, args){
    keep_names = numeric(0)
    new_names = rapply(args, function(each){
        if(!is.function(each) && !is.character(each)){
            as.character(each)
        } else {
            each
        }
    }, how = "unlist")
    # new_names = c(args, recursive = TRUE)
    characters_names = character(0) # for checking non-existing names
    for (each in new_names){
        if(is.character(each)){
            expanded_each = eval(bquote(subst(.(each))), 
                                 envir = parent.frame(2),
                                 enclos = baseenv()
            )
            next_names = which(old_names %in% expanded_each)
            characters_names = c(characters_names, expanded_each)
        } else {
            next_names = which(old_names %in% (old_names %i% each))
        }
        keep_names = c(keep_names, next_names %d% keep_names)
    }
    if(anyDuplicated(characters_names)){
        warning("duplicated names: ", 
                paste(characters_names[duplicated(characters_names)], collapse = ","),
                ". Repeated names are ignored."
                
                )
    }
    stopif(any(!(characters_names %in% old_names)), 
           "names not found: '", paste(characters_names %d% old_names, collapse = "', '"),"'")
    keep_names
    
}

#' @export
#' @rdname keep
'%keep%' = function(data, variables){
    data = substitute(data)
    variables = substitute(variables)
    eval(
       bquote(keep(.(data), .(variables))),
        envir = parent.frame(),
        enclos = baseenv()
    )
}

#' @export
#' @rdname keep
'%except%' = function(data, variables){
    data = substitute(data)
    variables = substitute(variables)
    eval(
        bquote(except(.(data), .(variables))),
        envir = parent.frame(),
        enclos = baseenv()
    )
}

#' @export
#' @rdname keep
.keep = function(...){
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    args = substitute(list(...))
    data = eval(
        bquote(keep(.(data), .(args))), 
        envir = parent.frame(),
        enclos = baseenv()                
    )
    ref(reference) = data
    invisible(data)
}

#' @export
#' @rdname keep
.except = function(...){
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    args = substitute(list(...))
    data = eval(
        bquote(except(.(data), .(args))), 
        envir = parent.frame(),
        enclos = baseenv()                
    )
    ref(reference) = data
    invisible(data)
}

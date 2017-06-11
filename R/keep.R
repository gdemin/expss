#' Keep or drop elements by name/criteria in data.frame/matrix/list/vector
#' 
#' \code{keep} selects variables/elements from data.frame by their names or by 
#' criteria (see \link{criteria}). \code{except} drops  variables/elements from 
#' data.frame by their names or by criteria. There is no non-standard evaluation
#' in these functions by design so use quotes for names of your variables or use
#' \link{qc}. The only exception with non-standard evaluation is \code{\%to\%}. 
#' See example.  
#' \code{\%keep\%}/\code{\%except\%} are infix versions of these functions.
#' Methods for list will apply \code{keep}/\code{except} to each element of
#' the list separately. \code{.keep}/\code{.except} are versions which works
#' with \link{default_dataset}.
#'
#' @param data data.frame/matrix/list/vector
#' @param ... column names/element names of type character or criteria/logical functions
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
#' keep(iris, to("Petal.Width")) # keep all columns up to 'Species'
#' 
#' except(iris, perl("^Petal")) # remove columns which names start with 'Petal'
#' 
#' except(iris, items(5)) # remove fifth column
#' 
#' keep(airquality, from("Ozone") & to("Wind")) # keep columns from 'Ozone' to 'Wind'
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
#' iris %except% perl("^Petal")    # remove columns which names start with 'Petal'
#' 
#' airquality %keep% (from("Ozone") & to("Wind")) # keep columns from 'Ozone' to 'Wind'
#'     
#' keep(airquality, Ozone %to% Wind) # the same result 
#' airquality %keep% (Ozone %to% Wind) # the same result 
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
#'  keep(dfs, subst("b_`i`"))
#'  keep(dfs, b_1 %to% b_5) # the same result
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

keep_helper = function(curr_names, variables_names, envir){
    variables_names = substitute_symbols(variables_names,
                              list("%to%" = as.name(".internal_to_"))
    )
    variables_names = eval(variables_names, envir = envir,
                enclos = baseenv())
    keep_names = numeric(0)
    new_names = rapply(variables_names, function(each){
        if(!is.function(each) && !is.character(each)){
            as.character(each)
        } else {
            each
        }
    }, how = "unlist")
    # new_names = c(variables_names, recursive = TRUE)
    characters_names = character(0) # for checking non-existing names
    for (each in new_names){
        if(is.character(each)){
            next_names = which(curr_names %in% each)
            characters_names = c(characters_names, each)
        } else {
            next_names = which(curr_names %in% (curr_names %i% each))
        }
        keep_names = c(keep_names, next_names %d% keep_names)
    }
    if(anyDuplicated(characters_names)){
        warning("duplicated names: ", 
                paste(characters_names[duplicated(characters_names)], collapse = ","),
                ". Repeated names are ignored."
                
        )
    }
    stopif(any(!(characters_names %in% curr_names)), 
           "names not found: '", paste(characters_names %d% curr_names, collapse = "', '"),"'")
    keep_names
    
}


    



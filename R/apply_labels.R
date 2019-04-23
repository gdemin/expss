#' Set variable labels/value labels on variables in the data.frame
#' 
#' \code{apply_labels} tries automatically detect what is variable label and
#' what are value labels. \code{.apply_labels} is version for working
#' with default dataset. See also \link{var_lab} and \link{val_lab}.
#'
#' @param data data.frame/list 
#' @param ...  named arguments. Name of argument is a variable name in
#'   \code{data}. Argument values are variable or value labels. Unnamed
#'   characters of length 1 are considered as variable labels and named vectors
#'   are considered as value labels.
#'   
#' @return \code{data} with applied labels
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       vs = "Engine",
#'                       vs = num_lab("
#'                              0 V-engine 
#'                              1 Straight engine
#'                              "),
#'                       am = "Transmission",
#'                       am = num_lab("
#'                              0 Automatic 
#'                              1 Manual
#'                              ")
#' )
#' 
#' # 'table' from base R
#' table(mtcars$vs, mtcars$am)
#' 
#' # more sofisticated crosstable
#' calculate(mtcars, cro(vs, am))
#' 
apply_labels = function(data, ...){
    UseMethod("apply_labels")
}

#' @export
apply_labels.list = function(data, ...){
    data_names = names(data)
    args = list(...)
    names_args = names(args)
    unknowns = setdiff(names_args, data_names)
    if(length(unknowns)){
        warning("Some names don't exist in `data`: ", paste(unknowns, collapse = ", "))
    }
    for(i in seq_along(args)){
        curr_name = names_args[i]
        if(curr_name %in% data_names){
            curr_lab = args[[i]]
            if(is.null(names(curr_lab))){
                stopif(length(curr_lab)>1, 
                       "Variable label should have length 1 but label for `", 
                       curr_name, "` has length ", length(curr_lab), ".")
                var_lab(data[[curr_name]]) = curr_lab
            } else {
                val_lab(data[[curr_name]]) = curr_lab
            }
        }
    }
    data
}

#' @export
apply_labels.data.frame = function(data, ...){
    apply_labels.list(data, ...)
}

#' @export
#' @rdname apply_labels
#' @usage NULL
apply_var_labs = function(data, ...){
    .Deprecated("apply_labels")
    UseMethod("apply_var_labs")
}

#' @export
apply_var_labs.list = function(data, ...){
    data_names = names(data)
    args = list(...)
    names_args = names(args)
    if(anyDuplicated(names_args)){
        warning("Some variable names are duplicated: ", 
                paste(names_args[duplicated(names_args)], collapse = ", "))
    }
    unknowns = setdiff(names_args, data_names)
    if(length(unknowns)){
        warning("Some names don't exist in `data`: ", paste(unknowns, collapse = ", "))
    }
    for(i in seq_along(args)){
        curr_name = names_args[i]
        if(curr_name %in% data_names){
            curr_lab = args[[i]]
            stopif(length(curr_lab)>1, 
                   "Variable label should have length 1 but label for `", 
                   curr_name, "` has length ", length(curr_lab), ".")
            var_lab(data[[curr_name]]) = curr_lab
        }
    }
    data    
}

#' @export
apply_var_labs.data.frame = function(data, ...){
    apply_var_labs.list(data, ...)
}

#' @export
#' @rdname apply_labels
#' @usage NULL
apply_val_labs = function(data, ...){
    .Deprecated("apply_labels")
    UseMethod("apply_val_labs")
}

#' @export
apply_val_labs.list = function(data, ...){
    data_names = names(data)
    args = list(...)
    names_args = names(args)
    if(anyDuplicated(names_args)){
        warning("Some variable names are duplicated: ", 
                paste(names_args[duplicated(names_args)], collapse = ", "))
    }
    unknowns = setdiff(names_args, data_names)
    if(length(unknowns)){
        warning("Some names don't exist in `data`: ", paste(unknowns, collapse = ", "))
    }
    for(i in seq_along(args)){
        curr_name = names_args[i]
        if(curr_name %in% data_names){
            curr_lab = args[[i]]
            stopif(is.null(names(curr_lab)), 
                   "Value labels should be named vector but labels for `", 
                   curr_name, "` are not.")
            val_lab(data[[curr_name]]) = curr_lab
        }
    }
    data    
}

#' @export
apply_val_labs.data.frame = function(data, ...){
    apply_val_labs.list(data, ...)
}


#' @export
#' @rdname apply_labels
.apply_labels = function(...){
    reference = suppressMessages(default_dataset())
    data = ref(reference)
    data = apply_labels(data, ...)
    ref(reference) = data
    invisible(data)
}

#' @export
#' @rdname apply_labels
#' @usage NULL
.apply_val_labs = function(...){
    reference = suppressMessages(default_dataset())
    data = ref(reference)
    data = apply_val_labs(data, ...)
    ref(reference) = data
    invisible(data)
}

#' @export
#' @rdname apply_labels
#' @usage NULL
.apply_var_labs = function(...){
    reference = suppressMessages(default_dataset())
    data = ref(reference)
    data = apply_var_labs(data, ...)
    ref(reference) = data
    invisible(data)
}
#' Set variable labels/value labels on variables in the data.frame
#' 
#' \code{apply_labels} tries automatically detect what is variable label and
#' what are value labels. See also \link{var_lab} and \link{val_lab}.
#'
#' @param data data.frame/list 
#' @param ...  named arguments or lists. Name of argument is a variable name in
#'   the \code{data}. Argument values are variable or value labels. Unnamed
#'   characters of length 1 are considered as variable labels and named vectors
#'   are considered as value labels. List arguments should be named lists and contain
#'   value and variable labels.
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
#' # more sophisticated crosstable
#' cross_cases(mtcars, vs, am)
#' 
#' # the same but with list argument
#' list_arg = list( vs = "Engine",
#'                  vs = num_lab("
#'                              0 V-engine 
#'                              1 Straight engine
#'                              "),
#'                  am = "Transmission",
#'                  am = num_lab("
#'                              0 Automatic 
#'                              1 Manual
#'                              ")
#'                  )
#' 
#' mtcars = apply_labels(mtcars, list_arg)
#' 
apply_labels = function(data, ...){
    UseMethod("apply_labels")
}

#' @export
apply_labels.list = function(data, ...){
    data_names = names(data)
    args = list(...)
    args = flat_list(args, flat_df = FALSE) # merge list arguments
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
                (length(curr_lab)>1) && stop(paste0("Variable label should have length 1 but label for `", 
                                                   curr_name, "` has length ", length(curr_lab), "."))
                if(is.data.table(data)){
                    set(data, i = NULL, j = curr_name, value = set_var_lab(data[[curr_name]], curr_lab))
                } else {
                    data[[curr_name]] = set_var_lab(data[[curr_name]], curr_lab)
                }
            } else {
                if(is.data.table(data)){
                    set(data, i = NULL, j = curr_name, value = set_val_lab(data[[curr_name]], curr_lab))
                } else {
                    data[[curr_name]] = set_val_lab(data[[curr_name]], curr_lab)
                }
            }
        }
    }
    if(is.data.table(data)) {
        invisible(data)
    } else {
        data
    }
}

#' @export
apply_labels.data.frame = function(data, ...){
    apply_labels.list(data, ...)
}


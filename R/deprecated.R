#' Deprecated functions (don't use them)
#' 
#' @param criterion deprecated
#' @param text deprecated
#' @param start deprecated
#' @param end deprecated
#' @param pattern deprecated
#' @param expr deprecated
#' @param x deprecated
#' @param ... deprecated
#'
#' @name deprecated
NULL

#' @export
#' @rdname deprecated
'%in_row%'=function(criterion, x){
    str_x = expr_to_character(substitute(x))
    str_criterion = expr_to_character(substitute(criterion))
    .Deprecated(
        paste0("`%in_row%` is deprecated. Please use `", str_x, " %row_in% ", str_criterion,"` instead.")
    )
    x %row_in% criterion
}

#' @export
#' @rdname deprecated
'%in_col%'=function(criterion, x){
    str_x = expr_to_character(substitute(x))
    str_criterion = expr_to_character(substitute(criterion))
    .Deprecated(
        paste0("`%in_col%` is deprecated. Please use `", str_x, " %col_in% ", str_criterion,"` instead.")
    )
    x %col_in% criterion
}


#' @export
#' @rdname deprecated
ml_left = function(text) {
    .Deprecated("num_lab")    
    num_lab(text)
}
#' @export
#' @rdname deprecated
ml_right = function(text) {
    .Deprecated("lab_num")
    lab_num(text)
}
#' @export
#' @rdname deprecated
ml_autonum = function(text) {
    .Deprecated("autonum")
    autonum(text)
}


#' @export
#' @rdname deprecated
vars_range = function(start, end){
    .Deprecated(msg = 
                    paste0("'vars_range' is deprecated. Use 'vars(from('", start,"') & to('", end, "'))' instead."))
    eval(substitute(expss::vars(from(start) & to(end))),
         envir = parent.frame(),
         enclos = baseenv()
    )
    
    
}

#' @export
#' @rdname deprecated
vars_range_list = function(start, end){
    .Deprecated(msg = 
                    paste0("'vars_range_list' is deprecated. Use 'vars_list(from('", start,"') & to('", end, "'))' instead."))
    eval(substitute(expss::vars_list(from(start) & to(end))),
         envir = parent.frame(),
         enclos = baseenv()
    )
    
}

#' @export
#' @rdname deprecated
vars_pattern = function(pattern){
    .Deprecated(msg = 
                    paste0("'vars_pattern' is deprecated. Use 'vars(perl(", pattern,"))' instead."))
    eval(substitute(expss::vars(perl(pattern))),
         envir = parent.frame(),
         enclos = baseenv()
    )
} 


#' @export
#' @rdname deprecated
vars_pattern_list = function(pattern){
    .Deprecated(msg = 
                    paste0("'vars_pattern_list' is deprecated. Use 'vars_list(perl(", pattern,"))' instead."))
    eval(substitute(expss::vars_list(perl(pattern))),
         envir = parent.frame(),
         enclos = baseenv()
    )
    
} 


#' @export
#' @rdname deprecated
.with = function (expr, ...) {
    .Deprecated(".calculate")
    reference = suppressMessages(default_dataset() )
    # expr = substitute(expr)
    data = ref(reference)
    eval(substitute(with(data, expr)), envir = parent.frame(), enclos = baseenv())
} 


modify_default_dataset_light_deprecated = function(x, ...){
    expr = as.character(as.expression(sys.call()))
    expr = parse(text = gsub("^\\.","", expr, perl = TRUE))
    if(as.character(expr[[1]][[1]]) %in% c("set_var_lab", "set_val_lab", "add_val_lab")){
        .Deprecated("apply_labels")
    }
    for_names = as.expression(substitute(x))
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    prepare_env(e, n = nrow(data), column_names = colnames(data))
    if (length(all.vars(for_names, functions = FALSE))==1 & length(all.vars(for_names, functions = TRUE))==1){
        for_names = as.character(for_names) 
    } else {
        for_names = names(eval(for_names, e))
    }
    stopif(length(for_names)==0, "Something is going wrong. Variables not found: ", expr_to_character((substitute(x))))
    res = eval(expr, e)
    data[, for_names] = res
    ref(reference) = data
    invisible(data)
}

#' @export
#' @rdname deprecated
.set_var_lab = modify_default_dataset_light_deprecated


#' @export
#' @rdname deprecated
.set_val_lab = modify_default_dataset_light_deprecated


#' @export
#' @rdname deprecated
.add_val_lab = modify_default_dataset_light_deprecated




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
    str_x = deparse(substitute(x))
    str_criterion = deparse(substitute(criterion))
    .Deprecated(
        paste0("`%in_row%` is deprecated. Please use `", str_x, " %row_in% ", str_criterion,"` instead.")
    )
    x %row_in% criterion
}

#' @export
#' @rdname deprecated
'%in_col%'=function(criterion, x){
    str_x = deparse(substitute(x))
    str_criterion = deparse(substitute(criterion))
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
    stopif(length(for_names)==0, "Something is going wrong. Variables not found: ", deparse((substitute(x))))
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



#' @export
#' @rdname deprecated
category = function(x, prefix = NULL, use_var_lab = TRUE, counted_value=1, compress = FALSE){
    .Deprecated("as.category")
    UseMethod("category")    
}

#' @export
category.matrix = function(x, prefix = NULL, use_var_lab = TRUE, counted_value=1, compress = FALSE){
    vallab = colnames(x)
    res = col(x)
    res[!(x %in% counted_value)] = NA
    compress_and_finish(res = res, vallab = vallab, prefix = prefix, compress = compress)
}

#' @export
category.data.frame = function(x, prefix = NULL, use_var_lab = TRUE, counted_value=1, compress = FALSE){
    if (use_var_lab){
        vallab = unlist(lapply(seq_along(x), function(i){
            varlab = var_lab(x[[i]])
            if(!is.null(varlab) && varlab!=""){
                varlab
            } else {
                colnames(x)[i]
            }
        }))
    } else {
        vallab = colnames(x)    
    } 
    
    # res = col(x)
    for(i in seq_along(x)){
        x[[i]] =  ((x[[i]] %in% counted_value) | NA)*i
        # res[,i][!(x[[i]] %in% counted_value)] = NA
    }
    compress_and_finish(res = x, vallab = vallab, prefix = prefix, compress = compress)
}

#' @export
category.default = function(x, prefix = NULL, use_var_lab = TRUE, counted_value=1, compress = FALSE){
    category.matrix(x = as.matrix(x), 
                    prefix = prefix, 
                    use_var_lab = use_var_lab, 
                    counted_value, 
                    compress)
}

#' @export
#' @rdname deprecated
category_df = function(x, prefix = NULL, use_var_lab = TRUE, counted_value=1, compress = FALSE){
    res =  category(x = x, 
                    prefix = prefix, 
                    use_var_lab = use_var_lab, 
                    counted_value, 
                    compress) 
    vallab = val_lab(res)
    if (!is.data.frame(res)) { 
        res = as.dtfrm(res)
        class(res) = union("category", class(res))   
    }    
    set_val_lab(res, vallab)
}
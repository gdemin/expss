# Experimental functions for working with default dataset
#
#
#
#' @param expr 
#' @param cond
#'
#' @examples 
#' a = a+1
#' @export
..modify = function (expr) {
    # based on 'within' from base R by R Core team
    reference = suppressMessages(default_dataset())
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    e$.n = nrow(data)
    eval(substitute(expr), e)
    rm(".n", envir = e)
    l = as.list(e, all.names = TRUE)
    
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    stopif(any(nrows!=1L & nrows!=nrow(data)),"Bad number of rows")
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    nl = c(names(data), new_vars)
    data[nl] = l[nl]
    ref(reference) = data
    invisible(NULL)
}



#' @export
#' @rdname .modify
..modify_if = function (cond, expr) {
    # based on 'within' from base R by R Core team
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    cond = substitute(cond)
    cond = eval(cond, data, parent.frame())
    if (!is.logical(cond)) 
        stop("'cond' must be logical")
    cond = cond & !is.na(cond)
    new_data = data[cond,, drop = FALSE]
    e = evalq(environment(), new_data, parent)
    e$.n = nrow(new_data)
    eval(substitute(expr), e)
    rm(".n", envir = e)
    l = as.list(e, all.names = TRUE)
    
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    stopif(any(nrows!=1L & nrows!=nrow(new_data)),"Bad number of rows")
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    data[cond, names(data)] = l[names(data)]
    data[, new_vars] = NA
    data[cond, new_vars] = l[new_vars]
    ref(reference) = data
    invisible(NULL)
}

# doesn't create new variables
modify_default_dataset_light = function(x, ...){
    expr = as.character(as.expression(sys.call()))
    expr = parse(text = gsub("^\\.\\.","", expr, perl = TRUE))
    for_names = as.expression(substitute(x))
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    e$.n = nrow(data)
    if (length(all.vars(for_names, functions = TRUE))==1){
        for_names = as.character(for_names) 
    } else {
        for_names = names(eval(for_names, e))
    }
    res = eval(expr, e)
    data[, for_names] = res
    ref(reference) = data
    invisible(NULL)
}

# doesn't modify dataset, just evaluate expression
eval_in_default_dataset = function(...){
    expr = as.character(as.expression(sys.call()))
    expr = parse(text = gsub("^\\.","", expr, perl = TRUE))
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    e$.n = nrow(data)
    eval(expr, e)
}


#' @export
#' @rdname .modify
..do_if = ..modify_if


#' @export
#' @rdname .modify
..compute = ..modify


#' @export
#' @rdname .modify
.val_lab = eval_in_default_dataset

#' @export
#' @rdname .modify
.var_lab = eval_in_default_dataset

#' @export
#' @rdname .modify
..set_var_lab = modify_default_dataset_light


#' @export
#' @rdname .modify
..set_val_lab = modify_default_dataset_light


#' @export
#' @rdname .modify
..add_val_lab = modify_default_dataset_light

#' @export
#' @rdname .modify
..if_val = modify_default_dataset_light

#' @export
#' @rdname .modify
..recode = function(x, ...){
    expr = as.character(as.expression(sys.call()))
    expr = parse(text = gsub("^\\.\\.recode","if_val", expr, perl = TRUE))
    for_names = as.expression(substitute(x))
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    e$.n = nrow(data)
    if (length(all.vars(for_names, functions = TRUE))==1){
        for_names = as.character(for_names) 
    } else {
        for_names = names(eval(for_names, e))
    }
    res = eval(expr, e)
    data[, for_names] = res
    ref(reference) = data
    invisible(NULL)
}

#' @export
#' @rdname .modify
.fre = eval_in_default_dataset

#' @export
#' @rdname .modify
.cro = eval_in_default_dataset

#' @export
#' @rdname .modify
.cro_cpct = eval_in_default_dataset

#' @export
#' @rdname .modify
.cro_rpct = eval_in_default_dataset

#' @export
#' @rdname .modify
.cro_tpct = eval_in_default_dataset




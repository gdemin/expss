#' Repeats the same transformations on a specified set of variables/values
#'
#' @param data data.frame/list. If \code{data} is list then \code{do_repeat}
#'   will be applied to each element of the list.
#' @param ...  stand-in name(s) followed by equals sign and a vector of 
#'   replacement variables or values. Characters considered as variables names, 
#'   all other types considered as is. Last argument should be expression in 
#'   curly brackets which will be evaluated in the scope of data.frame 
#'   \code{data}. See examples.
#' 
#' @details There is a special constant \code{.N} which equals to number of 
#'   cases in \code{data} for usage in expression inside \code{do_repeat}. Also 
#'   there is a variable \code{.item_num} which is equal to the current
#'   iteration number. 
#' @return transformed data.frame \code{data}
#' @export
#' @seealso \link{compute}, \link{do_if}
#'
#' @examples
#' data(iris)
#' scaled_iris = do_repeat(iris, 
#'                         i = qc(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width), 
#'                         {
#'                             i = scale(i)
#'                         })
#' head(scaled_iris)
#' 
#' # several stand-in names
#' old_names = qc(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#' new_names = paste0("scaled_", old_names)
#' scaled_iris = do_repeat(iris, 
#'                         orig = old_names, 
#'                         scaled = new_names, 
#'                         {
#'                             scaled = scale(orig)
#'                         })
#' head(scaled_iris)
#' 
#' # numerics
#' new_df = data.frame(id = 1:20)
#' new_df = do_repeat(new_df, 
#'                    item = qc(i1, i2, i3), 
#'                    value = c(1, 2, 3), 
#'                    {
#'                        item = value
#'                    })
#' head(new_df)
#' 
#' # the same result with internal variable '.item_num'
#' new_df = data.frame(id = 1:20)
#' new_df = do_repeat(new_df, 
#'                    item = qc(i1, i2, i3),
#'                    {
#'                        item = .item_num
#'                    })
#' head(new_df)
#' 
#' # functions
#' set.seed(123)
#' new_df = data.frame(id = 1:20)
#' new_df = do_repeat(new_df, 
#'                    item = qc(i1, i2, i3), 
#'                    fun = c("rnorm", "runif", "rexp"), 
#'                    {
#'                        item = fun(.N)
#'                    })
#' head(new_df)
#' 
#' 
do_repeat = function(data, ...){
    args = substitute(list(...))
    do_repeat_internal(data, args, parent = parent.frame())   
}



#' @export
#' @name do_repeat
.do_repeat = function (...) {
    reference = suppressMessages(default_dataset())
    data = ref(reference)
    args = substitute(list(...))
    data = do_repeat_internal(data, args, parent = parent.frame())
    ref(reference) = data
    invisible(data)

}



#' @export
#' @rdname do_repeat
as_is = function(...){
    I(list(...))
}

looping_values = function(variables_names, envir){
    variables_names = substitute_symbols(variables_names,
                                         list("%to%" = expr_into_helper,
                                              ".." = expr_internal_parameter)
    )
    existing_vars = get_current_variables(envir)
    variables_names = as.list(variables_names)
    variables_names[-1] = convert_top_level_symbols_to_characters(variables_names[-1])
    variables_names = as.call(variables_names)
    variables_names = eval(variables_names, envir = envir,
                           enclos = baseenv())
    variables_names
}    



do_repeat_internal = function(data, args, parent){
    UseMethod("do_repeat_internal")    
    
}    

#' @export    
do_repeat_internal.data.frame = function(data, args, parent){    
    expr = args[[length(args)]]
    args[length(args)] = NULL
    
    e = evalq(environment(), envir = data, enclos = parent)
    prepare_env(e, n = nrow(data), column_names = colnames(data))
    items = looping_values(args, envir = e)

    items_names = names(items)
    stopif(is.null(items_names) || any(is.na(items_names) | items_names==""), 
           "All variables among '...' should be named.")
    
    for(i in items_names){
        curr_item = items[[i]]
        if(inherits(curr_item, "AsIs")){
            curr_item = eval(args[[i]], envir = e, enclos = baseenv())
        } else {
            curr_item = unlist(curr_item, recursive = TRUE, use.names = FALSE)
            existing_vars = colnames(data)
            for(each_element in seq_along(curr_item)){
                each_name = curr_item[[each_element]]
                if(is.function(each_name)){
                    curr_item[[each_element]] = v_intersect(existing_vars, each_name)
                    existing_vars = v_diff(existing_vars, each_name)
                } 
            }
            curr_item = convert_characters_to_names(curr_item)
        }
        items[[i]] = unlist(curr_item, recursive = TRUE, use.names = FALSE)
    }
    items_lengths = lengths(items) 
    stopif(!all(items_lengths %in% c(1, max(items_lengths))),
           "All variables should have equal length or length 1")
    
    expr = substitute(expr)
    for(each_item in seq_len(max(items_lengths))){
        curr_loop = lapply(items, function(item){
            if(length(item)>1){
                item[[each_item]]
            } else {
                item[[1]]  
            }
        })
        e$.item_num = each_item
        e$.item_value = curr_loop
        lockBinding(".item_num", e)
        lockBinding(".item_value", e)
        names(curr_loop) = items_names
        substituted_expr = substitute_symbols(expr, curr_loop)
        eval(substituted_expr, e, enclos = baseenv())
        rm(".item_num", envir = e)
        rm(".item_value", envir = e)
    }
    clear_env(e)
    l = as.list(e, all.names = TRUE)
    l = l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    del = setdiff(names(data), names(l))
    if(length(del)){
        data[, del] = NULL
    }
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    wrong_rows = nrows!=1L & nrows!=nrow(data)
    if(any(wrong_rows)){
        er_message = utils::head(paste0("'", names(l)[wrong_rows], "' has ", nrows[wrong_rows], " rows"), 5)
        er_message = paste(er_message, collapse = ", ")
        stop(paste0("Bad number of rows: ", er_message, " instead of ", nrow(data), " rows."))
    }
    
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    nl = c(names(data), new_vars)
    data[nl] = l[nl]
    data    
}    

#' @export    
do_repeat_internal.list = function(data, args, parent){ 
    for(each in seq_along(data)){
        data[[each]] = do_repeat_internal(data[[each]], args = args,  parent = parent)
    }
    data    
}

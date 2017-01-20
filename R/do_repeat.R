#' Repeats the same transformations on a specified set of variables/values
#'
#' @param data data.frame
#' @param ...  stand-in name(s) followed by equals sign and a vector of 
#'   replacement variables or values. Characters considered as variables names, 
#'   all other types considered as is. Last argument should be expression in 
#'   curly brackets which will be evaluated in the scope of data.frame 
#'   \code{data}. See examples.
#' 
#' @details There is a special constant \code{.N} which equals to number of
#'   cases in \code{data} for usage in expression inside \code{do_repeat}.
#'   
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
#' set.seed(123)
#' new_df = data.frame(id = 1:20)
#' new_df = do_repeat(new_df, 
#'                    item = qc(i1, i2, i3), 
#'                    value = c(1, 2, 3), 
#'                    {
#'                        item = value
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
    UseMethod("do_repeat")    
}

#' @export
do_repeat.data.frame = function(data, ...){
    args = substitute(list(...))
    expr = args[[length(args)]]
    args[length(args)] = NULL
    items = eval(args, envir = parent.frame())
    items_lengths = lengths(items)
    stopif(!all(items_lengths %in% c(1, max(items_lengths))),
           "All variables should have equal length or length 1")
    items_names = names(items)
    stopif(is.null(items_names) || any(is.na(items_names) | items_names==""), 
           "All variables among '...' should be named.")
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    prepare_env(e, n = nrow(data))
    expr = substitute(expr)
    for(each_item in seq_len(max(items_lengths))){
        curr_loop = lapply(items, function(item){
            if(length(item)>1){
                item[[each_item]]
            } else {
                item[[1]]  
            }
        })
        names(curr_loop) = items_names
        
        substituted_expr = substitute_symbols(expr, curr_loop)
        eval(substituted_expr, e)
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


substitute_symbols = function (expr, symbols) {
    as.call(rapply(as.list(expr), function(elem){
        if(length(elem)<=1){
            str_elem = as.character(elem)
            if(is.symbol(elem) && (str_elem %in% names(symbols))){
                curr_symbol = symbols[[str_elem]] 
                if(is.character(curr_symbol)){
                    as.name(curr_symbol)
                } else {
                    curr_symbol   
                }
            } else {
                elem
            }
        } else {
            substitute_symbols(elem, symbols) 
        }
    },
    classes = "ANY",
    how = "replace"))
}




#' @export
#' @name do_repeat
.do_repeat = function (...) {
    # based on 'within' from base R by R Core team
    reference = suppressMessages(default_dataset())
    data = ref(reference)
    data = eval(substitute(do_repeat(data, ...)), envir = parent.frame(), enclos = baseenv())
    ref(reference) = data
    invisible(data)

}

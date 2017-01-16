#' Title
#'
#' @param data dfdf
#' @param ...  sdsdds
#' @param expr sdsdds
#'
#' @return sdsd
#' @export
#'
#' @examples
#' sdsdds
do_repeat = function(data, ..., expr){
    UseMethod("do_repeat")    
}

#' @export
do_repeat.data.frame = function(data, ..., expr){
    items = list(...)
    items_lengths = lengths(items)
    stopif(!all(items_lengths %in% c(1, max(items_lengths))),
           "All variable should have equal length or length 1")
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    e$.n = nrow(data)
    e$.N = nrow(data)
    e$set = set_generator(e$.N)
    lockBinding(".n", e)
    lockBinding(".N", e)
    lockBinding("set", e)
    expr = substitute(expr)
    for(each_item in seq_len(max(items_lengths))){
        e$.item_num = each_item
        lockBinding(".item_num", e)
        curr_loop = lapply(items, function(item){
            if(length(item)>1){
                item[each_item]
            } else {
                item  
            }
        })
        names(curr_loop) = names(items)
        
        substituted_expr = substitute_symbols(expr, curr_loop)
        eval(substituted_expr, e)
        rm(".item_num", envir = e)
    }
    rm(".n", "set", ".N", envir = e)
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
        if(length(elem)<=1 && is.symbol(elem)){
            str_elem = as.character(elem)
            if(str_elem %in% names(symbols)){
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



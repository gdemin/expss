#' Compute nested variable(-s) from several variables
#' 
#' \code{nest} mainly intended for usage with table functions such as 
#' \link{cro}. See examples. \code{\%nest\%} is infix version of this function. 
#' You can apply \code{nest} on multiple-response variables/list of variables
#' and data.frames.
#' 
#' @param ... vectors/data.frames/lists
#' @param x vector/data.frame/list
#' @param y vector/data.frame/list
#'
#' @return vector/data.frame/list
#' @export
#' @seealso See also \link[base]{interaction}
#' @examples
#' data(mtcars)
#'
#' mtcars = apply_labels(mtcars,
#'                       cyl = "Number of cylinders",
#'                       vs = "Engine",
#'                       vs = num_lab("
#'                              0 V-engine 
#'                              1 Straight engine
#'                              "),
#'                       am = "Transmission",
#'                       am = num_lab("
#'                              0 Automatic 
#'                              1 Manual
#'                              "),
#'                       carb = "Number of carburetors"
#' )
#' 
#' cross_cases(mtcars, cyl, am %nest% vs)
#' 
#' # list of variables
#' cross_cases(mtcars, cyl, am %nest% list(vs, cyl))
#' 
#' # list of variables - multiple banners/multiple nesting
#' cross_cases(mtcars, cyl, list(total(), list(am, vs) %nest% cyl))
#' 
#' # three variables 
#' cross_cases(mtcars, am %nest% vs %nest% carb, cyl)
#' 
#' # the same with usual version
#' cross_cases(mtcars, cyl, nest(am, vs))
#' 
#' # three variables 
#' cross_cases(mtcars, nest(am, vs, carb), cyl)
nest = function(...){
    arg = list(...)
    if (length(arg)<2) return(arg[[1]])
    x = arg[[1]]
    if (!is_list(x)) x = list(x)  
    x = flat_list(dichotomy_to_category_encoding(x), flat_df = FALSE)
    y = arg[[2]]
    if (!is_list(y)) y = list(y)
    y = flat_list(dichotomy_to_category_encoding(y), flat_df = FALSE)
    x = to_labelled(x)
    y = to_labelled(y)
    if (length(y)<2) {
        res = lapply(x, function(first) {
            nest_xy(first, y[[1]])
        })
        
    } else {
        res = lapply(x, function(first) {
            nest_xlist(first, y)
        })
    }
    if (length(arg)>2) res = do.call(nest, c(res, arg[-(1:2)]))
    if (length(res) == 1) res = res[[1]]
    res = set_var_lab(res, "")
    res
}

to_labelled = function(x){
    UseMethod("to_labelled")
}

#' @export
to_labelled.default = function(x){
    if(is.factor(x)) x = as.labelled(x)
    if("POSIXct" %in% class(x)) x = as.labelled(x)
    x
}

#' @export
to_labelled.list = function(x){
    rapply(x, function(item){
        to_labelled(item)
    },
    classes = "ANY",
    how = "replace")
} 


nest_xlist = function(x, y)
    # x -  vector or data.frame. data.frame is considered as multiple-response
    # y - list
{
    
    x = set_val_lab(as.list(as.dichotomy(x, keep_unused = TRUE, presence = 1, absence = NA)), c("|" = 1))
    
    res = lapply(x, function(x_elem){
             lapply(y, function(y_elem) nest_xy(x_elem, y_elem))
        })
    flat_list(res, flat_df = FALSE)
}



nest_xy = function(x, y){
    # x -  vector or data.frame. data.frame is considered as multiple-response 
    # y - vector or data.frame. data.frame is considered as multiple-response 

    vlabs_x = var_lab(x)
    vlabs_y = var_lab(y)
    labs_x = val_lab(x)
    labs_y = val_lab(y)
    uniq_x = sort(unique(c(uniq_elements(x), labs_x)))
    uniq_y = sort(unique(c(uniq_elements(y), labs_y)))
    encoded_x = integer_encoding(x, uniq_x)
    encoded_y = integer_encoding(y, uniq_y)
    encoded_labs_x = integer_encoding(labs_x, uniq_x)
    encoded_labs_y = integer_encoding(labs_y, uniq_y)
    max_x = length(uniq_x)
    max_y = length(uniq_y)
    nrow_x = nrow(encoded_x)
    nrow_y = nrow(encoded_y)
    stopif(!all(c(nrow_x, nrow_y) %in% c(1, max(nrow_x, nrow_y))),
           "Number of rows in nested variables should be equal or equal to 1.")
    if(nrow_x == 1){
        encoded_x = encoded_x[rep(1, nrow_y), , drop = FALSE]
    }
    if(nrow_y == 1){
        encoded_y = encoded_y[rep(1, nrow_x), , drop = FALSE]
    }
    
    x = (encoded_x - 1)*max_y
    y = encoded_y
    ncol_x = ncol(x)
    ncol_y = ncol(y)
    res = matrix(NA, nrow(x), ncol = ncol_x*ncol_y)
    seq_x = seq_len(ncol_x)
    seq_y = seq_len(ncol_y)
    for (i in seq_x) for (j in seq_y){
        res[, (i - 1)*ncol_y + j] = x[, i]+y[, j]
    }
    empty_columns = matrixStats::colAlls(res, value = NA, na.rm = FALSE)
    if(all(empty_columns)) {
        # we need at least one column in the result
        empty_columns[1] = FALSE
    }
    res = res[, !empty_columns]
    new_lab_x = values2labels(set_val_lab(uniq_x, labs_x))
    new_lab_y = values2labels(set_val_lab(uniq_y, labs_y))
    if (!is.null(vlabs_x)) new_lab_x = paste(vlabs_x, new_lab_x, sep = "|")
    if (!is.null(vlabs_y)) new_lab_y = paste(vlabs_y, new_lab_y, sep = "|")
    # new_uniq = unique(c(values_with_labs, uniq_elements(res)))
    res_lab = seq_len(max_y*max_x)
    names(res_lab) = outer(new_lab_y, new_lab_x, function(x, y) paste(y, x, sep = "|"))
    # res_lab = res_lab[res_lab %in% new_uniq]
    if(is.matrix(res)) res = as.sheet(res)
    if(!is.null(res_lab)) {
        names(res_lab) = remove_unnecessary_splitters(names(res_lab))
    }    
    val_lab(res) = res_lab
    if(NCOL(res)>1){
        mrset(res)
    } else {
        res    
    }
}

#' @export
#' @rdname nest
'%nest%' = function(x, y) nest(x, y)




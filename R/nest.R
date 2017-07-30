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
#' with(mtcars, cro(cyl, am %nest% vs))
#' 
#' # three variables 
#' with(mtcars, cro(am %nest% vs %nest% carb, cyl))
#' 
#' # the same with usual version
#' with(mtcars, cro(cyl, nest(am, vs)))
#' 
#' # three variables 
#' with(mtcars, cro(nest(am, vs, carb), cyl))
nest = function(...){
    arg = list(...)
    if (length(arg)<2) return(arg[[1]])
    x = to_labelled(arg[[1]])
    y = to_labelled(arg[[2]])
    if (!is_list(x)) x = list(x) else x = flat_list(x)
    if (!is_list(y)) {
        res = lapply(x, function(first) {
            nest_xy(first, y)
        })
    } else {
        y = flat_list(y)
        
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
    # x - vector or data.frame, y - list
{
    labs = val_lab(x)
    vlab = var_lab(x)
    uniqs = sort(unique(c(uniq_elements(x), labs)))
    xlist = lapply(uniqs, function(item){
        res = (x == item) | NA
        res[res] = item
        res
    })
    if(!is.null(labs)){
        names(labs) = remove_unnecessary_splitters(names(labs))
    }
    res = lapply(seq_along(uniqs), function(i){
        item = uniqs[i]
        # browser()
        newlab = labs[labs == item]
        if (length(newlab)>0) val_lab(xlist[[i]]) = newlab else val_lab(xlist[[i]]) = NULL
        var_lab(xlist[[i]]) = vlab
        # nest(xlist[[i]], y)
        lapply(y, function(item2){
             nest_xy(xlist[[i]], item2)
        })

    })
    unlist(res, recursive = FALSE, use.names = FALSE)
}



nest_xy = function(x, y){
    res_mrset = FALSE
    if(is.dichotomy(x)) x = as.category(x, compress = FALSE)
    if(is.dichotomy(y)) y = as.category(y, compress = FALSE)
    if(is.category(x) || is.category(y)){
        res_mrset = TRUE
    }
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
    values_with_labs = c(outer((encoded_labs_x-1)*max_x, encoded_labs_x, "+"))
    res = matrix(NA, nrow(x), ncol = ncol(x)*ncol(y))
    for (i in seq_len(ncol(x))) for (j in seq_len(ncol(y))){
        res[, j*i] = x[, i]+y[, j]
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
    if(is.matrix(res)) res = as.dtfrm(res)
    if(!is.null(res_lab)) {
        names(res_lab) = remove_unnecessary_splitters(names(res_lab))
    }    
    val_lab(res) = res_lab
    if(res_mrset){
        mrset(res)
    } else {
        res    
    }
}

#' @export
#' @rdname nest
'%nest%' = function(x, y) nest(x, y)




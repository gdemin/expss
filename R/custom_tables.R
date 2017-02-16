make_empty_intermediate_table = function(deparsed_call){
    res = list(data = NULL, 
               colvars = NULL, 
               rowvars = NULL, 
               cellvars = NULL,
               weight = NULL, 
               subset = NULL,
               call = deparsed_call
    )
    class(res) = union("intermediate_table", class(res))
    res
    
}

col_vars = function(...){
    UseMethod("col_vars")
}

where.intermediate_table = function(...){
    
}

weight.intermediate_table = function(weight){
    
}

col_vars.default = function(...){
    colvars = list(...)
    stopif(length(colvars) == 0, "'col_vars' - there is no variables.")
    colvars = flat_list(dichotomy_to_category_encoding(colvars), flat_df = FALSE) # process_mdsets
    colvars = rapply(colvars, as.labelled, classes = c("factor", "POSIXct"), how = "replace")
    res = make_empty_intermediate_table(deparse(sys.call()))
    res$colvars = colvars
    res
}

col_vars.intermediate_table = function(...){
    args = list(...)
    res = args[[1]]
    args = args[-1]
    colvars = flat_list(dichotomy_to_category_encoding(args), flat_df = FALSE) # process_mdsets
    colvars = rapply(colvars, as.labelled, classes = c("factor", "POSIXct"), how = "replace")
    stopif(length(colvars) == 0, "'col_vars' - there is no variables.")
    res$colvars = colvars
    if(!missing(weight)){
        res$weight = weight
    }
    if(!missing(subset)){
        res$subset = subset
    }
    res$cell_name = ""
    res$call = paste0(res$call, " %>% ", deparse(sys.call()))
    class(res) = union("intermediate_table", class(res))
    res
}


row_vars = function(..., weight = NULL, subset = NULL){
    UseMethod("row_vars")
}

row_vars.default = function(..., weight = NULL, subset = NULL){
    stop(paste0(
        "you can't use 'row_vars' without precending 'col_vars'. Try col_vars(somo variables) %>% ", 
        deparse(sys.call())
    )
    )
}

row_vars.intermediate_table = function(..., weight = NULL, subset = NULL){
    args = list(...)
    res = args[[1]]
    args = args[-1]
    rowvars = flat_list(multiples_to_single_columns_with_dummy_encoding(args), flat_df = FALSE) # process_mdsets
    rowvars = rapply(rowvars, as.labelled, classes = c("factor", "POSIXct"), how = "replace")
    stopif(length(rowvars) == 0, "'col_vars' - there is no variables.")
    res$rowvars = rowvars
    if(!missing(weight)){
        res$weight = weight
    }
    if(!missing(subset)){
        res$subset = subset
    }
    res$call = paste0(res$call, " %>% ", deparse(sys.call()))
    class(res) = union("intermediate_table", class(res))
    res
}


cell = function(..., weight = NULL, subset = NULL){
    UseMethod("row_vars")
}

cell.default = function(..., weight = NULL, subset = NULL){
    stop(paste0(
        "you can't use 'cell' without precending 'col_vars'. Try col_vars(somo variables) %>% ", 
        deparse(sys.call())
    )
    )
}

cell.intermediate_table = function(..., cell_name = "", weight = NULL, subset = NULL){
    args = list(...)
    res = args[[1]]
    args = args[-1]
    cellvars = flat_list(args, flat_df = FALSE)
    stopif(length(cellvars) == 0, "'cell' - there is no variables for calculations.")
    res$cellvars = cellvars
    if(!missing(weight)){
        res$weight = weight
    }
    if(!missing(subset)){
        res$subset = subset
    }
    res$cell_name = cell_name
    res$call = paste0(res$call, " %>% ", deparse(sys.call()))
    newdata = calculate_cell(res, fun = fun, stat_names = stat_names, cell_name = cell_name)
    res$data = data.table::rbind(res$data, newdata, use.name = TRUE, fill = TRUE)
    class(res) = union("with_cells", "intermediate_table", class(res))
    res
}

"[.with_cells" = function(x, ..., stat_names = NULL, need_margins = c("rows", "columns", "total")){
    res = x
    fun = list(...)
    stopif(length(fun)==0, "Functions are needed")
    stat_names = names(list(...))
    for(each in funs){
        funs[[each]] = match.fun(funs[[each]])
    }
    
    res$call = paste0(res$call, " %>% ", deparse(sys.call()))
    newdata = calculate_cell(res, funs = funs, stat_names = stat_names)
    res$data = data.table::rbind(res$data, newdata, use.name = TRUE, fill = TRUE)
    class(res) = union("intermediate_table", class(res))
    res
    
}

calculate_cell = function(res, fun = fun, stat_names = stat_names, cell_name = cell_name){
    
    
    
}
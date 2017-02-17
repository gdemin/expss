#' Functions for constracting custom tables
#'
#' @param ... jjjkj
#'
#' @return jkjk
#' @export
#'
#' @examples
#' 1
col_vars_ = function(...){
    UseMethod("col_vars_")
}


#' @export
col_vars_.default = function(...){
    res = make_empty_intermediate_table()
    col_vars_(res, ...)
}

#' @export
col_vars_.intermediate_table = function(...){
    args = list(...)
    res = args[[1]]
    colvars = args[-1]
    if(length(colvars)){
        colvars = flat_list(dichotomy_to_category_encoding(colvars), flat_df = FALSE) # process_mdsets
        colvars = rapply(colvars, as.labelled, classes = c("factor", "POSIXct"), how = "replace")
        res$colvars = colvars
    } else {
        res$colvars = NULL    
    }
    res
}



######

#' @rdname col_vars_
#' @export
cell_ = function(..., cell_name = NULL){
    UseMethod("cell_")
}


#' @export
cell_.default = function(..., cell_name = NULL){
    res = make_empty_intermediate_table()
    cell_(res, ..., cell_name = cell_name)
}

#' @export
cell_.intermediate_table = function(..., cell_name = NULL){
    args = list(...)
    res = args[[1]]
    cell = args[-1]
    res$cell_name = cell_name
    if(length(cell)){
        cell = flat_list(multiples_to_single_columns_with_dummy_encoding(cell), flat_df = TRUE) # process_mdsets
        cell = rapply(cell, as.labelled, classes = c("factor", "POSIXct"), how = "replace")
        res$cellvars = cell
        class(res) = union("with_cell", class(res))
    } else {
        res$cellvars = NULL 
        class(res) = class(res) %d% "with_cell"
    }
    res
}

#########

#' @rdname col_vars_
#' @export
row_vars_ = function(...){
    UseMethod("row_vars_")
}


#' @export
row_vars_.default = function(...){
    res = make_empty_intermediate_table()
    row_vars_(res, ...)
}

#' @export
row_vars_.intermediate_table = function(...){
    args = list(...)
    res = args[[1]]
    rowvars = args[-1]
    if(length(rowvars)){
        rowvars = flat_list(multiples_to_single_columns_with_dummy_encoding(row_vars), flat_df = TRUE) # process_mdsets
        rowvars = rapply(rowvars, as.labelled, classes = c("factor", "POSIXct"), how = "replace")
        res$rowvars = rowvars
    } else {
        res$rowvars = NULL    
    }
    res
}


#####################
#' @rdname col_vars_
#' @export
stat = function(..., margins = c("rows", "columns", "total")){
    UseMethod("stat")
}

#' @export
stat.with_cell = function(..., margins = c("rows", "columns", "total")){
    
    args = list(...)
    res = args[[1]]
    stopif(length(args)<2, "Functions are needed")
    fun = match.fun(args[[2]])
    stat_names = names(args)[[2]]
    new_long_table = long_table_summary(
        summary_vars = res$cell,
        col_vars = res$colvars,
        fun = fun,
        weight = res$weight,
        subgroup = res$subgroup,
        row_vars = res$rowvars,
        stat_names = stat_names,
        custom_labels = NULL
        
    )
    if(is.null(res$long_table)){
        res$long_table = new_long_table
    } else {
        res$long_table = rbind(res$long_table, new_long_table, use.name = TRUE, fill = TRUE)    
    }
    class(res) = union("table_with_result", class(res))
    res
}

#' @rdname col_vars_
#' @export
stat_ = stat

#########

#' @rdname col_vars_
#' @export
weight_ = function(...){
    UseMethod("weight_")
}

#' @export
weight_.default = function(...){
    res = make_empty_intermediate_table()
    weight_(res, ...)
}

#' @export
weight_.intermediate_table = function(...){
    args = list(...)
    res = args[[1]]
    args = args[-1]
    if(length(args)==0){
        res$weight = NULL
    } else {
        stopif(length(args)>1, "'weight' should be single column variable.")
        weight = args[[1]]
        stopif(!is.numeric(weight) && !is.logical(weight), "'weight' should be numeric or logical")
        res$weight = set_negative_and_na_to_zero(as.numeric(weight))
    }
    res
}

#########

#' @rdname col_vars_
#' @export
subgroup_ = function(...){
    UseMethod("subgroup_")
}

#' @export
subgroup_.default = function(...){
    res = make_empty_intermediate_table()
    subgroup_(res, ...)
}

#' @export
subgroup_.intermediate_table = function(...){
    args = list(...)
    res = args[[1]]
    args = args[-1]
    if(length(args)==0){
        res$subgroup = NULL
    } else {
        stopif(length(args)>1, "'subgroup' should be single column variable.")
        subgroup = args[[1]]
        stopif(!is.numeric(subgroup) && !is.logical(subgroup), "'subgroup' should be numeric or logical")
        res$subgroup = subgroup
    }
    res
}

#######
calculate_cell = function(res, fun = fun, stat_names = stat_names, cell_name = cell_name){
    
    
    
}

########

make_empty_intermediate_table = function(){
    res = list(long_table = NULL, 
               colvars = NULL, 
               rowvars = NULL, 
               cellvars = NULL,
               weight = NULL, 
               subgroup = NULL,
               cell_name = NULL    
    )
    class(res) = union("intermediate_table", class(res))
    res
    
}

where.intermediate_table = function(...){
    
}
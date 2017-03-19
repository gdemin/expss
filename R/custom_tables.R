### constants for intermediate_table


DATA  = "data"   
RESULT = "result"    
COL_VAR = "col_var"  
ROW_VAR = "row_var"     
CELL_VAR = "cell_var"   
SUBGROUP = "subgroup"   
WEIGHT  = "weight" 


#' Functions for constracting custom tables
#'
#' @param ... jjjkj
#'
#' @return jkjk
#' @export
#'
#' @examples
#' 1
tab_cols = function(data, ...){
    UseMethod("tab_cols")
}


#' @export
tab_cols.data.frame = function(data, ...){
    res = make_empty_intermediate_table()
    tab_cols(res, ..., label = label)
}

#' @export
tab_cols.intermediate_table = function(data, ...){
    args = list(...)
    res = args[[1]]
    args = args[-1]
    if(length(args)){
        args = flat_list(dichotomy_to_category_encoding(args), flat_df = FALSE) # process_mdsets
        args = rapply(args, as.labelled, classes = c("factor", "POSIXct"), how = "replace")
        res[[COL_VAR]] = args
    } else {
        res[[COL_VAR]] = NULL    
    }
    res[[COL_LABEL]] = label
    res[[COL_NUM]]  = res[[COL_NUM]] + 1
    res
}



######

#' @rdname tab_cols
#' @export
tab_cells = function(data, ...){
    UseMethod("tab_cells")
}


#' @export
tab_cells.data.frame = function(data, ...){
    res = make_empty_intermediate_table()
    tab_cells(res, ..., label = label)
}

#' @export
tab_cells.intermediate_table = function(data, ...){
    args = list(...)
    res = args[[1]]
    args = args[-1]
    if(length(args)){
        args = flat_list(args, flat_df = FALSE)
        args = rapply(args, as.labelled, classes = c("factor", "POSIXct"), how = "replace")
        res[[CELL_VAR]] = args
    } else {
        res[[CELL_VAR]] = NULL    
    }
    res[[CELL_LABEL]] = label
    res[[CELL_NUM]] = res[[CELL_NUM]] + 1
    res
}

#########

#' @rdname tab_cols
#' @export
tab_rows = function(data, ...){
    UseMethod("tab_rows")
}


#' @export
tab_rows.default = function(data, ...){
    res = make_empty_intermediate_table()
    tab_rows(res, ...)
}

#' @export
tab_rows.intermediate_table = function(data, ...){
    args = list(...)
    res = args[[1]]
    args = args[-1]
    if(length(args)){
        args = flat_list(multiples_to_single_columns_with_dummy_encoding(args), flat_df = TRUE) # process_mdsets
        args = rapply(args, as.labelled, classes = c("factor", "POSIXct"), how = "replace")
        res[[ROW_VAR]] = args
    } else {
        res[[ROW_VAR]] = NULL    
    }
    res[[ROW_LABEL]] = label
    res[[ROW_NUM]]  = res[[ROW_NUM]] + 1

    res
}


#####################
#' @rdname tab_cols
#' @export
tab_stat_fun = function(data, ...,  label = NULL){
    UseMethod("tab_stat_fun")
}

#' @rdname tab_cols
#' @export
tab_stat_fun_df = function(data, ..., label = NULL){
    UseMethod("tab_stat_fun_df")
}

#' @rdname tab_cols
#' @export
tab_stat_cases = function(data, label = NULL){
    UseMethod("tab_stat_cases")
}

#' @rdname tab_cols
#' @export
tab_stat_cpct = function(data, label = NULL){
    UseMethod("tab_stat_cpct")
}

#' @rdname tab_cols
#' @export
tab_stat_cpct_responses = function(data, label = NULL){
    UseMethod("tab_stat_cpct_responses")
}

#' @rdname tab_cols
#' @export
tab_stat_tpct = function(data,  label = NULL){
    UseMethod("tab_stat_tpct")
}

#########

#' @rdname tab_cols
#' @export
tab_weight = function(data, weight = NULL){
    UseMethod("tab_weight")
}

#' @export
tab_weight.default = function(data, weight = NULL){
    res = make_empty_intermediate_table()
    tab_weight(res, weight)
}

#' @export
tab_weight.intermediate_table = function(data, weight = NULL){
    if(is.null(args)==0){
        res[[WEIGHT]] = NULL
    } else {
        stopif(!is.numeric(weight) && !is.logical(weight), "'weight' should be numeric or logical.")
    }
    res
}

#########

#' @rdname tab_cols
#' @export
tab_subgroup = function(data, subgroup = NULL){
    UseMethod("tab_subgroup")
}

#' @export
tab_subgroup.default = function(data, subgroup = NULL){
    res = make_empty_intermediate_table()
    tab_subgroup(res, ..., label = NULL)
}

#' @export
tab_subgroup.intermediate_table = function(data, subgroup = NULL){
    if(is.null(subgroup)){
        res[[SUBGROUP]] = NULL
    } else {
        stopif(!is.numeric(subgroup) && !is.logical(subgroup), "'subgroup' should be numeric or logical.")
        res[[SUBGROUP]] = subgroup
    }
    res
}



########
tab_pivot = function(data, stat_label_position = c("inside_rows", 
                                             "outside_rows", 
                                             "inside_columns", 
                                             "outside_columns")){
    UseMethod("tab_pivot")
    # stat_label_position = match.arg(stat_label_position)
}

tab_intermediate_pivot = function(data, stat_label_position = c("inside_rows", 
                                                   "outside_rows", 
                                                   "inside_columns", 
                                                   "outside_columns")){
    UseMethod("tab_intermediate_pivot")
    # stat_label_position = match.arg(stat_label_position)
    
}
    

make_empty_intermediate_table = function(data){
    table_names = c(DATA, 
                    COL_VAR,
                    ROW_VAR,
                    CELL_VAR,
                    SUBGROUP,
                    WEIGHT,
                    RESULT
                    )
    res = list(.Data = rep(list(NULL), length(table_names)),
               .Names = table_names)
    res[[COL_VAR]] = total()
    res[[ROW_VAR]] = total(label = "")
    res[[CELL_VAR]] = total()
    res[[SUBGROUP]] = NULL
    res[[WEIGHT]] = NULL
    res[[RESULT]] = list()
    class(res) = union("intermediate_table", class(res))
    res
    
}

print.intermediate_table = function(x, ...){
    cat("Intermediate_table. Use 'tab_pivot' to finish table creation.")
}
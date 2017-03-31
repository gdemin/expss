### constants for intermediate_table


DATA  = "data"   
RESULT = "result"    
COL_VAR = "col_var"  
ROW_VAR = "row_var"     
CELL_VAR = "cell_var"   
SUBGROUP = "subgroup"   
WEIGHT  = "weight" 
STAT_LABELS = "stat_labels"
MIS_VAL = "mis_val"

#' Functions for constracting custom tables
#'
#' @param ... jjjkj
#'
#' @return jkjk
#' @export
#'
#' @name tables
#' @examples
#' 1
tab_cols = function(data, ...){
    UseMethod("tab_cols")
}


#' @export
tab_cols.data.frame = function(data, ...){
    res = make_empty_intermediate_table(data)
    tab_cols(res, ...)
}

#' @export
tab_cols.intermediate_table = function(data, ...){
    args = eval(substitute(calculate(data[["data"]], list(...))),
                envir = parent.frame(),
                enclos = baseenv())
    args = add_names_to_list(args, ...)
    if(length(args)>0){
        args = flat_list(args, flat_df = FALSE)
        data[[COL_VAR]] = args
    } else {
        data[[COL_VAR]] = list(total())    
    }
    data
}



######

#' @rdname tables
#' @export
tab_cells = function(data, ...){
    UseMethod("tab_cells")
}


#' @export
tab_cells.data.frame = function(data, ...){
    res = make_empty_intermediate_table(data)
    tab_cells(res, ...)
}

#' @export
tab_cells.intermediate_table = function(data, ...){
    # expr = substitute(create_list_with_names(...))
    # args = eval(bquote(calculate(data[["data"]], .(expr))))
    # expr = substitute(list(...))
    args = eval(substitute(calculate(data[["data"]], list(...))),
                envir = parent.frame(),
                enclos = baseenv())
    args = add_names_to_list(args, ...)
    if(length(args)>0){
        args = flat_list(args, flat_df = FALSE)
        data[[CELL_VAR]] = args
    } else {
        data[[CELL_VAR]] = list(total())    
    }
    data
}

#########

#' @rdname tables
#' @export
tab_rows = function(data, ...){
    UseMethod("tab_rows")
}


#' @export
tab_rows.default = function(data, ...){
    res = make_empty_intermediate_table(data)
    tab_rows(res, ...)
}



#' @export
tab_rows.intermediate_table = function(data, ...){
    # expr = substitute(create_list_with_names(...))
    # args = eval(bquote(calculate(data[["data"]], .(expr))))
    # expr = substitute(list(...))
    args = eval(substitute(calculate(data[["data"]], list(...))),
                envir = parent.frame(),
                enclos = baseenv())
    args = add_names_to_list(args, ...)
    if(length(args)>0){
        args = flat_list(multiples_to_single_columns_with_dummy_encoding(args),
                         flat_df = TRUE)
        data[[ROW_VAR]] = args
    } else {
        data[[ROW_VAR]] = list(total())    
    }
    data
}

#########

#' @rdname tables
#' @export
tab_weight = function(data, weight = NULL){
    UseMethod("tab_weight")
}

#' @export
tab_weight.default = function(data, weight = NULL){
    res = make_empty_intermediate_table(data)
    # expr = substitute(weight)
    eval(substitute(tab_weight(res, weight)),
         envir = parent.frame(),
         enclos = baseenv())
}

#' @export
tab_weight.intermediate_table = function(data, weight = NULL){
    # expr = substitute(weight)
    weight = eval(substitute(calculate(data[["data"]], weight)),
                  envir = parent.frame(),
                  enclos = baseenv())
    if(is.null(weight)){
        data[[WEIGHT]] = NULL
    } else {
        stopif(!is.numeric(weight) && !is.logical(weight), "'weight' should be numeric or logical.")
        data[[WEIGHT]] = weight
    }
    data
}

############

#' @rdname tables
#' @export
tab_mis_val = function(data, ...){
    UseMethod("tab_mis_val")
}

#' @export
tab_mis_val.default = function(data, ...){
    res = make_empty_intermediate_table(data)
    tab_mis_val(res, ...)
}

#' @export
tab_mis_val.intermediate_table = function(data, ...){
    # expr = substitute(weight)
    args = eval(substitute(calculate(data[["data"]], list(...))),
                envir = parent.frame(),
                enclos = baseenv())
    if(length(args)>0){
        data[[MIS_VAL]] = unlist(args)
    } else {
        data[[MIS_VAL]] = NULL
    }
    data
}

#########

#' @rdname tables
#' @export
tab_subgroup = function(data, subgroup = NULL){
    UseMethod("tab_subgroup")
}

#' @export
tab_subgroup.default = function(data, subgroup = NULL){
    res = make_empty_intermediate_table(data)
    # expr = substitute(subgroup)
    eval(substitute(tab_subgroup(res, subgroup)),
         envir = parent.frame(),
         enclos = baseenv())
}

#' @export
tab_subgroup.intermediate_table = function(data, subgroup = NULL){
    # expr = substitute(subgroup)
    subgroup = eval(substitute(calculate(data[["data"]], subgroup)),
                  envir = parent.frame(),
                  enclos = baseenv())
    if(is.null(subgroup)){
        data[[SUBGROUP]] = NULL
    } else {
        stopif(!is.numeric(subgroup) && !is.logical(subgroup), "'subgroup' should be numeric or logical.")
        data[[SUBGROUP]] = subgroup
    }
    data
}


#####################
#' @rdname tables
#' @export
tab_stat_fun = function(data, ..., 
                        mis_val = NULL, 
                        label = NULL){
    UseMethod("tab_stat_fun")
}

#' @rdname tables
#' @export
tab_stat_fun_df = function(data, ..., 
                           mis_val = NULL, 
                           label = NULL){
    UseMethod("tab_stat_fun_df")
}

#' @rdname tables
#' @export
tab_stat_cases = function(data, 
                          total_label = NULL,
                          total_statistic = "u_cases",
                          total_row_position = c("below", "above", "none"),
                          label = NULL){
    UseMethod("tab_stat_cases")
}

#' @rdname tables
#' @export
tab_stat_cpct = function(data, 
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none"),
                         label = NULL){
    UseMethod("tab_stat_cpct")
}

#' @rdname tables
#' @export
tab_stat_cpct_responses =function(data, 
                                  total_label = NULL,
                                  total_statistic = "u_responses",
                                  total_row_position = c("below", "above", "none"),
                                  label = NULL){
    UseMethod("tab_stat_cpct_responses")
}

#' @rdname tables
#' @export
tab_stat_tpct = function(data, 
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none"),
                         label = NULL){
    UseMethod("tab_stat_tpct")
}

#' @rdname tables
#' @export
tab_stat_rpct = function(data, 
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none"),
                         label = NULL){
    UseMethod("tab_stat_rpct")
}

############
#' @export
tab_stat_fun.intermediate_table = function(data, ..., 
                                label = NULL){
    # fun = eval(substitute(combine_functions(...)))
    args = list(...)
    if(length(args)>1 || !is.null(names(args))){
        fun = combine_functions(...)
    } else {
        fun = args[[1]]
    }
    # label = substitute(label)
    label = eval(substitute(calculate(data[["data"]], label)),
                 envir = parent.frame(),
                 enclos = baseenv())
    result = cro_fun(
        cell_vars = get_cells(data),
        col_vars = data[[COL_VAR]],
        row_vars = data[[ROW_VAR]],
        weight = data[[WEIGHT]],
        subgroup = data[[SUBGROUP]],
        fun = fun
    )
    add_result_to_intermediate_table(data, result, label)
}

#' @rdname tables
#' @export
tab_stat_mean = function(data, label = "Mean"){
    eval(substitute(tab_stat_fun(data, 
                             w_mean, 
                             label = label)),
         envir = parent.frame(),
         enclos = baseenv())    
}

#' @rdname tables
#' @export
tab_stat_median = function(data, label = "Median"){
    eval(substitute(tab_stat_fun(data, 
                             w_median, 
                             label = label)),
         envir = parent.frame(),
         enclos = baseenv())    
}

#' @rdname tables
#' @export
tab_stat_se = function(data, label = "S. E."){
    eval(substitute(tab_stat_fun(data,
                             w_se, 
                             label = label)),
         envir = parent.frame(),
         enclos = baseenv())    
}

#' @rdname tables
#' @export
tab_stat_sum = function(data, label = "Sum"){
    eval(substitute(tab_stat_fun(data, 
                             w_sum, 
                             label = label)),
         envir = parent.frame(),
         enclos = baseenv())    
}

#' @rdname tables
#' @export
tab_stat_min = function(data, label = "Min."){
    eval(substitute(tab_stat_fun(data, 
                                 w_min, 
                                 label = label)),
         envir = parent.frame(),
         enclos = baseenv())    
}

#' @rdname tables
#' @export
tab_stat_max = function(data, label = "Max."){
    eval(substitute(tab_stat_fun(data, 
                                 w_max, 
                                 label = label)),
         envir = parent.frame(),
         enclos = baseenv())    
}

#' @rdname tables
#' @export
tab_stat_sd = function(data, label = "Std. dev."){
    eval(substitute(tab_stat_fun(data, 
                             w_sd, 
                             label = label)),
         envir = parent.frame(),
         enclos = baseenv())
}

#' @rdname tables
#' @export
tab_stat_valid_n = function(data, label = "Valid N"){
    eval(substitute(tab_stat_fun(data, 
                             valid_n, 
                             label = label)),
         envir = parent.frame(),
         enclos = baseenv())    
}

#' @rdname tables
#' @export
tab_stat_unweighted_valid_n = function(data, label = "Unw. valid N"){
    eval(substitute(tab_stat_fun(data, 
                             unweighted_valid_n, 
                             label = label)),
         envir = parent.frame(),
         enclos = baseenv())    
}


#' @export
tab_stat_fun_df.intermediate_table = function(data, ..., 
                                   label = NULL){
    
    # fun = eval(substitute(combine_functions(...)))
    args = list(...)
    if(length(args)>1 || !is.null(names(args))){
        fun = combine_functions(...)
    } else {
        fun = args[[1]]
    }
    label = eval(substitute(calculate(data[["data"]], label)),
                 envir = parent.frame(),
                 enclos = baseenv())
    result = cro_fun_df(
        cell_vars = get_cells(data),
        col_vars = data[[COL_VAR]],
        row_vars = data[[ROW_VAR]],
        weight = data[[WEIGHT]],
        subgroup = data[[SUBGROUP]],
        fun = fun
    )
    add_result_to_intermediate_table(data, result, label)
}

#' @export
tab_stat_cases.intermediate_table = function(data, 
                                  total_label = NULL,
                                  total_statistic = "u_cases",
                                  total_row_position = c("below", "above", "none"),
                                  label = NULL){
    label = eval(substitute(calculate(data[["data"]], label)),
                 envir = parent.frame(),
                 enclos = baseenv())
    result = cro_cases(
        cell_vars = get_cells(data),
        col_vars = data[[COL_VAR]],
        row_vars = data[[ROW_VAR]],
        weight = data[[WEIGHT]],
        subgroup = data[[SUBGROUP]],
        total_label = total_label,
        total_statistic = total_statistic,
        total_row_position = total_row_position
    )
    add_result_to_intermediate_table(data, result, label)
}

#' @export
tab_stat_cpct.intermediate_table = function(data, 
                                 total_label = NULL,
                                 total_statistic = "u_cases",
                                 total_row_position = c("below", "above", "none"),
                                 label = NULL){
    label = eval(substitute(calculate(data[["data"]], label)),
                 envir = parent.frame(),
                 enclos = baseenv())
    result = cro_cpct(
        cell_vars = get_cells(data),
        col_vars = data[[COL_VAR]],
        row_vars = data[[ROW_VAR]],
        weight = data[[WEIGHT]],
        subgroup = data[[SUBGROUP]],
        total_label = total_label,
        total_statistic = total_statistic,
        total_row_position = total_row_position
    )
    add_result_to_intermediate_table(data, result, label)
}


#' @export
tab_stat_cpct_responses.intermediate_table =function(data, 
                                          total_label = NULL,
                                          total_statistic = "u_responses",
                                          total_row_position = c("below", "above", "none"),
                                          label = NULL){
    label = eval(substitute(calculate(data[["data"]], label)),
                 envir = parent.frame(),
                 enclos = baseenv())
    result = cro_cpct_responses(
        cell_vars = get_cells(data),
        col_vars = data[[COL_VAR]],
        row_vars = data[[ROW_VAR]],
        weight = data[[WEIGHT]],
        subgroup = data[[SUBGROUP]],
        total_label = total_label,
        total_statistic = total_statistic,
        total_row_position = total_row_position
    )
    add_result_to_intermediate_table(data, result, label)
}

#' @export
tab_stat_tpct.intermediate_table = function(data, 
                                 total_label = NULL,
                                 total_statistic = "u_cases",
                                 total_row_position = c("below", "above", "none"),
                                 label = NULL){
    label = eval(substitute(calculate(data[["data"]], label)),
                 envir = parent.frame(),
                 enclos = baseenv())
    result = cro_tpct(
        cell_vars = get_cells(data),
        col_vars = data[[COL_VAR]],
        row_vars = data[[ROW_VAR]],
        weight = data[[WEIGHT]],
        subgroup = data[[SUBGROUP]],
        total_label = total_label,
        total_statistic = total_statistic,
        total_row_position = total_row_position
    )
    add_result_to_intermediate_table(data, result, label)
}

#' @export
tab_stat_rpct.intermediate_table = function(data, 
                                            total_label = NULL,
                                            total_statistic = "u_cases",
                                            total_row_position = c("below", "above", "none"),
                                            label = NULL){
    label = eval(substitute(calculate(data[["data"]], label)),
                 envir = parent.frame(),
                 enclos = baseenv())
    result = cro_rpct(
        cell_vars = get_cells(data),
        col_vars = data[[COL_VAR]],
        row_vars = data[[ROW_VAR]],
        weight = data[[WEIGHT]],
        subgroup = data[[SUBGROUP]],
        total_label = total_label,
        total_statistic = total_statistic,
        total_row_position = total_row_position
    )
    add_result_to_intermediate_table(data, result, label)
}

######
############
#' @export
tab_stat_fun.default = function(data, ..., 
                        mis_val = NULL, 
                        label = NULL){
    tab_stat_()
}

#' @export
tab_stat_fun_df.default = function(data, ..., 
                           mis_val = NULL, 
                           label = NULL){
    tab_stat_()
}

#' @export
tab_stat_cases.default = function(data, 
                          total_label = NULL,
                          total_statistic = "u_cases",
                          total_row_position = c("below", "above", "none"),
                          label = NULL){
    tab_stat_()
}

#' @export
tab_stat_cpct.default = function(data, 
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none"),
                         label = NULL){
    tab_stat_()
}


#' @export
tab_stat_cpct_responses.default =function(data, 
                                  total_label = NULL,
                                  total_statistic = "u_responses",
                                  total_row_position = c("below", "above", "none"),
                                  label = NULL){
    tab_stat_()
}

#' @export
tab_stat_tpct.default = function(data, 
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none"),
                         label = NULL){
    tab_stat_()
}

#' @export
tab_stat_rpct.default = function(data, 
                                 total_label = NULL,
                                 total_statistic = "u_cases",
                                 total_row_position = c("below", "above", "none"),
                                 label = NULL){
    tab_stat_()
}

######
tab_stat_ = function(){
    stop("No data for 'tab_stat_*'. Use at least one of 'tab_cells'/'tab_rows'/'tab_cols' before the 'tab_stat'.")
}

########
#' @rdname tables
#' @export
tab_pivot = function(data, stat_position = c("outside_rows",
                                             "inside_rows",
                                             "outside_columns",
                                             "inside_columns"), 
                     stat_label = c("inside", "outside")){
    UseMethod("tab_pivot")
}

#' @export
tab_pivot.intermediate_table = function(data, stat_position = c("outside_rows",
                                                       "inside_rows",
                                                       "outside_columns",
                                                       "inside_columns"), 
                                        stat_label = c("inside", "outside")){
    stopif(length(data[[RESULT]])==0, 
           "No statistics in the table. Use at least one of 'tab_stat' before the 'pivot'.")
    stat_position = match.arg(stat_position)
    res = switch(stat_position, 
                 outside_rows = pivot_rows(data, stat_position = "outside", 
                                           stat_label = stat_label),
                 inside_rows = pivot_rows(data, stat_position = "inside", 
                                          stat_label = stat_label),
                 outside_columns = pivot_columns(data, stat_position = "outside", 
                                                 stat_label = stat_label),
                 inside_columns = pivot_columns(data, stat_position = "inside", 
                                                stat_label = stat_label)
    )
    res[["row_labels"]] = remove_unnecessary_splitters(res[["row_labels"]])
    colnames(res) = remove_unnecessary_splitters(colnames(res))
    rownames(res) = NULL
    res
}

#' @export
tab_pivot.default = function(data, stat_position = c("outside_rows",
                                                     "inside_rows",
                                                     "outside_columns",
                                                     "inside_columns"), 
                             stat_label = c("inside", "outside")
                             ){
    stop("No data for 'tab_pivot'. 
         Use at least one of 'tab_cells'/'tab_rows'/'tab_cols' and at least one of 'tab_stat' before the 'tab_pivot'.")
}

#' @rdname tables
#' @export
tab_transpose = function(data){
    UseMethod("tab_transpose")
}

#' @export
tab_transpose.default = function(data){
    t(data)
}

#' @export
tab_transpose.intermediate_table = function(data){
    result_num = length(data[[RESULT]])
    stopif(result_num==0,
           "No results for transposition. Use 'tab_transpose' after 'tab_stat_*' or after 'tab_pivot'.")
    data[[RESULT]][[result_num]] = t(data[[RESULT]][[result_num]])
    data
}
# ########
# #' @rdname tables
# #' @export
# tab_intermediate_pivot = function(data, stat_position = c("outside_rows",
#                                                           "inside_rows",
#                                                           "outside_columns",
#                                                           "inside_columns"), 
#                                   stat_label = c("inside", "outside")
# ){
#     UseMethod("tab_intermediate_pivot")
#     
# }
# 
# #' @export
# tab_intermediate_pivot.intermediate_table = function(data, stat_position = c("outside_rows",
#                                                                              "inside_rows",
#                                                                              "outside_columns",
#                                                                              "inside_columns"), 
#                                                      stat_label = c("inside", "outside"),
#                                                      label = NULL
# ){
#     stopif(length(data[[RESULT]])==0, 
#            "No statistics in the table. Use at least one of 'tab_stat' before the 'pivot'.")
#     res = tab_pivot(data, stat_position = stat_position, stat_label = stat_label)
#     data[[RESULT]] = list(res)
#     data[[STAT_LABELS]] = if_null(label, "")
#     data
# }
# 
# #' @export
# tab_intermediate_pivot.default = function(data, stat_label_position = c("outside_rows",
#                                                            "inside_rows", 
#                                                            "outside_columns", 
#                                                            "inside_columns")
# ){
#     stop("No data for 'tab_pivot'. 
#          Use at least one of 'tab_cells'/'tab_rows'/'tab_cols' and at least one of 'tab_stat' before the 'tab_pivot'.")
# }
################

pivot_rows = function(data, stat_position = c("inside", "outside"), 
                      stat_label = c("inside", "outside")){
    stat_position = match.arg(stat_position)  
    stat_label = match.arg(stat_label)  
    results = data[[RESULT]]
    labels = data[[STAT_LABELS]]
    labels_index = seq_along(labels)
    
    results = lapply(labels_index, function(item_num){
        curr = results[[item_num]]
        curr[["..label_index__"]] = item_num
        curr[["..label__"]] = labels[item_num]
        curr
    })
    results = Reduce(add_rows, results)

    if(stat_position == "inside"){
        results[["..row_labels__"]] = match(results[["row_labels"]], 
                                            unique(results[["row_labels"]])
        )
        results = sort_asc(results, "..row_labels__", "..label_index__")
        
        results[["..row_labels__"]] = NULL
    }
    if(stat_label == "inside"){
        results[["row_labels"]] = paste0( results[["row_labels"]], "|", results[["..label__"]])     
    } else {
        results[["row_labels"]] = paste0( results[["..label__"]], "|", results[["row_labels"]])
    }
    
    results[["..label__"]] = NULL
    results[["..label_index__"]] = NULL
    results
    
}

################

pivot_columns = function(data, stat_position = c("inside", "outside"), 
                         stat_label = c("inside", "outside")){
    stat_position = match.arg(stat_position)  
    stat_label = match.arg(stat_label)   
    results = data[[RESULT]]
    labels = data[[STAT_LABELS]]
    labels_index = seq_along(labels)
    
    all_colnames = unlist(lapply(results, function(item) colnames(item)[-1]))
    colnames_index = match(all_colnames, unique(all_colnames))
    results_ncols = vapply(results, NCOL, FUN.VALUE = numeric(1)) - 1 # 'row_labels' excluded
    
    results = lapply(labels_index, function(item_num){
        curr = results[[item_num]]
        if(stat_label == "inside"){
            colnames(curr)[-1] = paste0(colnames(curr)[-1], "|", labels[item_num])
        } else {
            colnames(curr)[-1] = paste0(labels[item_num], "|", colnames(curr)[-1])
        }
        curr
    })
    
    results = Reduce(merge, results)
    
    labels_index = rep.int(labels_index, times = results_ncols)
    if(stat_position == "inside"){
        new_order = order(colnames_index, labels_index, decreasing = FALSE)
    } else {
        new_order = order(labels_index, colnames_index, decreasing = FALSE)   
    }
    old_colnames = colnames(results)
    results = results[, c(1, new_order + 1), drop = FALSE]
    colnames(results) = old_colnames[c(1, new_order + 1)]
    results
    
}

#############

add_result_to_intermediate_table = function(data, result, label){
    new_result_position = length(data[[RESULT]]) + 1
    label = if_null(label, "")
    data[[RESULT]][[new_result_position]] = result
    data[[STAT_LABELS]][[new_result_position]] = label
    data
}

#############
make_empty_intermediate_table = function(data){
    res = list()
    res[["data"]] = data
    res[[COL_VAR]] = list(total())
    res[[ROW_VAR]] = list(total(label = ""))
    res[[CELL_VAR]] = list(total())
    res[[SUBGROUP]] = NULL
    res[[WEIGHT]] = NULL
    res[[MIS_VAL]] = NULL
    res[[RESULT]] = list()
    res[[STAT_LABELS]] = character(0)
    class(res) = union("intermediate_table", class(res))
    res
    
}

##############

get_cells = function(intermediate_table){
    cells = intermediate_table[[CELL_VAR]]
    mis_val = intermediate_table[[MIS_VAL]]
    if(is.list(mis_val) && length(mis_val)==1){
        mis_val = mis_val[[1]]
    }
    na_if(cells, mis_val)
}

##############

#' @export
print.intermediate_table = function(x, ...){
    cat("Object of class 'intermediate_table'. Use 'tab_pivot' to finish table creation.\n")
}

###############
add_names_to_list = function(args, ...){
    if(length(args)==0) return(NULL)
    possible_names = unlist(lapply(as.list(substitute(list(...)))[-1], deparse))
    arg_names = names(args)
    if(length(possible_names)>0){
        if(is.null(arg_names)) {
            names(args) = possible_names
        } else {
            names(args)[arg_names==""] = possible_names[arg_names==""]
        } 
    }
    for(each_item in seq_along(names(args))){
        curr_args = args[[each_item]]
        if(!is.list(curr_args) && 
           !is.data.frame(curr_args) && 
           !is.function(curr_args) && 
           !is.matrix(curr_args)){
            curr_lab = var_lab(curr_args)
            if(is.null(curr_lab)){
                var_lab(args[[each_item]]) = names(args)[[each_item]]
            }
        } else {
            names(args)[each_item] = ""
        }
        
    }
    args
}
### constants for intermediate_table

strange_name = function(x){
    paste0("...", x, "___")
}


DATA  = "data"   # only in the 'intermediate_table' 
LONG_TABLE = "long_table"    # only in the 'intermediate_table' 

COL_VAR = strange_name("col_var") # only in the 'intermediate_table' 
COL_NUM = strange_name("col_num")
COL_LABEL = strange_name("col_label") # visible by user

ROW_VAR = strange_name("row_var")    # only in the 'intermediate_table' 
ROW_NUM = strange_name("row_num") 
ROW_LABEL = strange_name("row_label")  # visible by user

CELL_VAR = strange_name("cell_var")   # only in the 'intermediate_table' 
CELL_NUM = strange_name("cell_num")
CELL_LABEL = strange_name("cell_label")  # visible by user

SUBGROUP = strange_name("subgroup")   # only in the 'intermediate_table' 
SUBGROUP_NUM = strange_name("subgroup_num")
SUBGROUP_LABEL = strange_name("subgroup_label") # visible by user

STAT_LABEL = strange_name("stat_label")  # visible by user
WEIGHT  = strange_name("weight") 

# additional constant for long_table
# we need *_VALUE_NUM because if use COL_VALUE we can have bad sorting when
# combining variables of different types in the single column

STAT_NUM = strange_name("stat_num")

COL_VAR_NUM = strange_name("col_var_num")
COL_VAR_LABEL = strange_name("col_var_label") # visible by user
COL_VALUE = strange_name("col_value")   
COL_VALUE_NUM = strange_name("col_value_num") 
COL_VALUE_LABEL = strange_name("col_value_label") # visible by user

ROW_VAR_NUM = strange_name("row_var_num") 
ROW_VAR_LABEL = strange_name("row_var_label")  # visible by user
ROW_VALUE = strange_name("row_value") 
ROW_VALUE_NUM = strange_name("row_value_num") 
ROW_VALUE_LABEL = strange_name("row_value_label") # visible by user


CELL_VAR_NUM = strange_name("cell_var_num")
CELL_VAR_LABEL = "cell_var_label"  # visible by user
CELL_VALUE = strange_name("cell_value")
CELL_VALUE_NUM = strange_name("cell_value_num")
CELL_VALUE_LABEL = "cell_value_label"  # visible by user

FUN_VALUE_LABEL = "fun_value_label"  # visible by user
FUN_VALUE_NUM = strange_name("fun_value_num")

#' Functions for constracting custom tables
#'
#' @param ... jjjkj
#'
#' @return jkjk
#' @export
#'
#' @examples
#' 1
col_vars_ = function(..., label = NULL){
    UseMethod("col_vars_")
}


#' @export
col_vars_.default = function(..., label = NULL){
    res = make_empty_intermediate_table()
    col_vars_(res, ..., label = label)
}

#' @export
col_vars_.intermediate_table = function(..., label = NULL){
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

#' @rdname col_vars_
#' @export
cell_ = function(..., label = NULL){
    UseMethod("cell_")
}


#' @export
cell_.default = function(..., label = NULL){
    res = make_empty_intermediate_table()
    cell_(res, ..., label = label)
}

#' @export
cell_.intermediate_table = function(..., label = NULL){
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

#' @rdname col_vars_
#' @export
row_vars_ = function(..., label = NULL){
    UseMethod("row_vars_")
}


#' @export
row_vars_.default = function(..., label = NULL){
    res = make_empty_intermediate_table()
    row_vars_(res, ..., label = label)
}

#' @export
row_vars_.intermediate_table = function(..., label = NULL){
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
#' @rdname col_vars_
#' @export
stat = function(..., fun_value_labels = NULL, label = NULL){
    UseMethod("stat")
}

#' @rdname col_vars_
#' @export
stat_df = function(..., fun_value_labels = NULL, label = NULL){
    UseMethod("stat")
}

#' @export
stat_df.intermediate_table = function(..., fun_value_labels = NULL, label = NULL){
    
    args = list(...)
    res = args[[1]]
    if(is.null(res[[CELL_VAR]])){
        stop("Cell variables should be provided before 'stat': '... %>% cell(...) %>% stat(...)' ")
    }
    res[[STAT_LABEL]] = label
    args = args[[-1]]
    stopif(length(args)<1, "Functions are needed for 'stat'.")
    stopif(length(args)>1, "Currently only one function is allowed in the 'stat'.")
    
    new_long_table = long_table_summary(
        res,
        fun_value_labels = if_null(fun_value_labels, ""),
        fun = args[[1]]
    )
    for(each in c(COL_LABEL, ROW_LABEL, CELL_LABEL, 
                  STAT_LABEL, SUBGROUP_LABEL)
    ){
        new_long_table[[each]] = if_null(intermediate_table[[each]], "")
    }
    for(each in c(COL_NUM, ROW_NUM, 
                  CELL_NUM, SUBGROUP_NUM)
    ){
        new_long_table[[each]] = if_null(intermediate_table[[each]], "")
    }
    if(is.null(res[[LONG_TABLE]])){
        new_long_table[[STAT_NUM]] = 1
        res[[LONG_TABLE]] = new_long_table
    } else {
        new_long_table[[STAT_NUM]] = 
            max(res[[LONG_TABLE]][[STAT_NUM]]) + 1
        res[[LONG_TABLE]] = rbind(res[[LONG_TABLE]], new_long_table, use.names = TRUE, fill = TRUE)    
    }
    res
}

#' @rdname col_vars_
#' @export
stat_ = stat

#' @rdname col_vars_
#' @export
stat_df_ = stat_df

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
    stopif(length(args)>1, "'weight' should have only one argument for weighting.")
    if(length(args)==0){
        res[[WEIGHT]] = NULL
    } else {
        weight = args[[1]]
        stopif(NCOL(weight)>1, "'weight' should be single column variable.")
        stopif(!is.numeric(weight) && !is.logical(weight), "'weight' should be numeric or logical.")
        res[[WEIGHT]] = set_negative_and_na_to_zero(as.numeric(weight))
    }
    res
}

#########

#' @rdname col_vars_
#' @export
subgroup_ = function(..., label = NULL){
    UseMethod("subgroup_")
}

#' @export
subgroup_.default = function(..., label = NULL){
    res = make_empty_intermediate_table()
    subgroup_(res, ..., label = NULL)
}

#' @export
subgroup_.intermediate_table = function(..., label = NULL){
    args = list(...)
    res = args[[1]]
    args = args[-1]
    stopif(length(args)>1, "'subgroup' should have only one argument for weighting.")
    if(length(args)==0){
        res[[SUBGROUP]] = NULL
    } else {
        subgr = args[[1]]
        stopif(NCOL(subgr)>1, "'subgroup' should be single column variable.")
        stopif(!is.numeric(subgr) && !is.logical(subgr), "'subgroup' should be numeric or logical.")
        res[[SUBGROUP]] = subgr
    }
    res[[SUBGROUP_LABEL]] = label
    res[[SUBGROUP_NUM]]  = res[[SUBGROUP_NUM]] + 1
    res
}



########


make_empty_intermediate_table = function(){
    table_names = c(DATA, 
                    LONG_TABLE,
                    COL_VAR,
                    COL_NUM,
                    COL_LABEL,
                    ROW_VAR,
                    ROW_NUM,
                    ROW_LABEL,
                    CELL_VAR,
                    CELL_NUM,
                    CELL_LABEL,
                    SUBGROUP,
                    SUBGROUP_NUM,
                    SUBGROUP_LABEL,
                    STAT_LABEL,
                    WEIGHT)
    res = list(.Data = rep(list(NULL), length(table_names)),
               .Names = table_names)
    res[[COL_NUM]] = 0
    res[[ROW_NUM]] = 0
    res[[CELL_NUM]] = 0
    res[[SUBGROUP_NUM]] = 0
    class(res) = union("intermediate_table", class(res))
    res
    
}


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
TOTAL_LABEL = "total_label"
TOTAL_STATISTIC = "total_statistic"
TOTAL_ROW_POSITION = "total_row_position" 
SIGNIFICANCE_OPTIONS = "significance_options" 

#' Functions for custom tables construction
#' 
#' Table construction consists of at least of three functions chained with 
#' \code{magrittr} pipe operator. At first we need to 
#' specify variables for which statistics will be computed with 
#' \code{tab_cells}. Secondary, we calculate statistics with one of 
#' \code{tab_stat_*} functions. And last, we finalize table creation with 
#' \code{tab_pivot}: \code{dataset \%>\% tab_cells(variable) \%>\%
#' tab_stat_cases() \%>\% tab_pivot()}. After that we can optionally sort table
#' with \link{tab_sort_asc}, drop empty rows/columns with \link{drop_rc} and 
#' transpose with \code{tab_transpose}. Generally, table is just a data.frame so
#' we can use arbitrary operations on it. Statistic is always calculated with 
#' the last cell, column/row variables, weight, missing values and subgroup. To 
#' define new cell/column/row variables we can call appropriate function one more time. 
#' \code{tab_pivot} defines how we combine different statistics and where 
#' statistic labels will appear - inside/outside rows/columns. See examples.
#' For significance testing see \link{significance}.
#' 
#' @details 
#' \itemize{
#' \item{\code{tab_cells}}{ variables on which percentage/cases/summary
#' functions will be computed. Use \link{mrset}/\link{mdset} for
#' multiple-response variables.}
#' \item{\code{tab_cols}}{ optional variables which breaks table by
#'   columns. Use \link{mrset}/\link{mdset} for
#' multiple-response variables.}
#' \item{\code{tab_rows}}{ optional variables which breaks table by rows. Use
#' \link{mrset}/\link{mdset} for multiple-response variables.}
#' \item{\code{tab_weight}}{ optional weight for the statistic.}
#' \item{\code{tab_mis_val}}{ optional missing values for the statistic. It will
#' be applied on variables specified by \code{tab_cells}. It works in the same
#' manner as \link{na_if}.}
#' \item{\code{tab_subgroup}}{ optional logical vector/expression which specify
#' subset of data for table.}
#' \item{\code{tab_row_label}}{ Add to table empty row with specified row
#' labels. It is usefull for making section headings and etc.}
#' \item{\code{tab_total_row_position}}{ Default value for
#' \code{total_row_position} argument in \code{tab_stat_cases} and etc. Can be
#' one of "below", "above", "none".}
#' \item{\code{tab_total_label}}{ Default value for \code{total_label} argument
#' in \code{tab_stat_cases} and etc. You can provide several names - each name
#' for each total statistics.}
#' \item{\code{tab_total_statistic}}{ Default value for \code{total_statistic}
#' argument in \code{tab_stat_cases} and etc. You can provide several values.
#' Possible values are "u_cases", "u_responses", "u_cpct", "u_rpct", "u_tpct", 
#' "w_cases", "w_responses", "w_cpct", "w_rpct", "w_tpct". "u_" means unweighted
#' statistics and "w_" means weighted statistics.}
#' \item{\code{tab_stat_fun}, \code{tab_stat_fun_df}}{ \code{tab_stat_fun} 
#' applies function on each variable in cells separately, \code{tab_stat_fun_df}
#' gives to function each data.frame in cells as a whole 
#' \link[data.table]{data.table} with all names converted to variable labels (if
#' labels exists). So it is not recommended to rely on original variables names 
#' in your \code{fun}. For details see \link{cross_fun}. You can provide several
#' functions as arguments. They will be combined as with
#' \link{combine_functions}. So you can use \code{method} argument. For details
#' see documentation for \link{combine_functions}. }
#' \item{\code{tab_stat_cases}}{ calculate counts.}
#' \item{\code{tab_stat_cpct}, \code{tab_stat_cpct_responses}}{ calculate column
#' percent. These functions give different results only for multiple response
#' variables. For \code{tab_stat_cpct} base of percent is number of valid cases.
#' Case is considered as valid if it has at least one non-NA value. So for
#' multiple response variables sum of percent may be greater than 100. For 
#' \code{tab_stat_cpct_responses} base of percent is number of valid responses. 
#' Multiple response variables can have several responses for single case. Sum 
#' of percent of \code{tab_stat_cpct_responses} always equals to 100\%.}
#' \item{\code{tab_stat_rpct}}{ calculate row percent. Base
#' for percent is number of valid cases.}
#' \item{\code{tab_stat_tpct}}{ calculate table percent. Base
#' for percent is number of valid cases.}
#' \item{\code{tab_stat_mean}, \code{tab_stat_median}, \code{tab_stat_se},
#' \code{tab_stat_sum}, \code{tab_stat_min}, \code{tab_stat_max},
#' \code{tab_stat_sd}, \code{tab_stat_valid_n}, 
#' \code{tab_stat_unweighted_valid_n}}{ different summary statistics. NA's are
#' always omitted.}
#' \item{\code{tab_pivot}}{ finalize table creation and define how different
#' \code{tab_stat_*} will be combined}
#' \item{\code{tab_caption}}{ set caption on the table. Should be used after the \code{tab_pivot}.}
#' \item{\code{tab_transpose}}{ transpose final table after \code{tab_pivot} or last
#' statistic.}}
#' @param data data.frame/intermediate_table  
#' @param ... vector/data.frame/list. Variables for tables. Use
#'   \link{mrset}/\link{mdset} for multiple-response variables.
#' @param label character. Label for the statistic in the \code{tab_stat_*}. 
#' @param weight numeric vector in \code{tab_weight}. Cases with NA's, negative
#'   and zero weights are removed before calculations.
#' @param subgroup logical vector in \code{tab_subgroup}. You can specify
#'   subgroup on which table will be computed.
#' @param weighted_valid_n logical. Sould we show weighted valid N in
#'   \code{tab_stat_mean_sd_n}? By default it is FALSE.
#' @param labels character vector of length 3. Labels for mean, standard
#'   deviation and valid N in \code{tab_stat_mean_sd_n}.
#' @param total_label By default "#Total". You can provide several names - each
#'   name for each total statistics.
#' @param total_statistic  By default it is "u_cases" (unweighted cases). 
#'   Possible values are "u_cases", "u_responses", "u_cpct", "u_rpct", "u_tpct",
#'   "w_cases", "w_responses", "w_cpct", "w_rpct", "w_tpct". "u_" means
#'   unweighted statistics and "w_" means weighted statistics.
#' @param total_row_position Position of total row in the resulting table. Can
#'   be one of "below", "above", "none".
#' @param stat_position character one of the values \code{"outside_rows"}, 
#'   \code{"inside_rows"}, \code{"outside_columns"} or \code{"inside_columns"}.
#'   It defines how we will combine statistics in the table.
#' @param stat_label character one of the values \code{"inside"} or 
#'   \code{"outside"}. Where will be placed labels for the statistics relative
#'   to column names/row labels? See examples.
#' @param unsafe logical If TRUE than \code{fun} will be evaluated as is. It can
#'   lead to significant increase in the performance. But there are some 
#'   limitations. For \code{tab_stat_fun} it means that your function \code{fun} 
#'   should return vector of length one. Also there will be no attempts to make
#'   labels for statistic.  For \code{tab_stat_fun_df} your function should return
#'   vector of length one or list/data.frame (optionally with 'row_labels'
#'   element - statistic labels). If \code{unsafe} is TRUE then further
#'   arguments (\code{...}) for \code{fun} will be ignored.
#' @return All of these functions return object of class 
#'   \code{intermediate_table} except \code{tab_pivot} which returns final
#'   result - object of class \code{etable}. Basically it's a data.frame but
#'   class is needed for custom methods.
#' @seealso \link{fre}, \link{cross_cases}, \link{cross_fun}, \link{tab_sort_asc},
#'   \link{drop_empty_rows}, \link{significance}.
#' @export
#'
#' @name tables
#' @examples
#' \dontrun{
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       mpg = "Miles/(US) gallon",
#'                       cyl = "Number of cylinders",
#'                       disp = "Displacement (cu.in.)",
#'                       hp = "Gross horsepower",
#'                       drat = "Rear axle ratio",
#'                       wt = "Weight (1000 lbs)",
#'                       qsec = "1/4 mile time",
#'                       vs = "Engine",
#'                       vs = c("V-engine" = 0,
#'                              "Straight engine" = 1),
#'                       am = "Transmission",
#'                       am = c("Automatic" = 0,
#'                              "Manual"=1),
#'                       gear = "Number of forward gears",
#'                       carb = "Number of carburetors"
#' )
#' # some examples from 'cro'
#' # simple example - generally with 'cro' it can be made with less typing
#' mtcars %>% 
#'     tab_cells(cyl) %>% 
#'     tab_cols(vs) %>% 
#'     tab_stat_cpct() %>% 
#'     tab_pivot()
#' 
#' # split rows
#' mtcars %>% 
#'     tab_cells(cyl) %>% 
#'     tab_cols(vs) %>% 
#'     tab_rows(am) %>% 
#'     tab_stat_cpct() %>% 
#'     tab_pivot()
#' 
#' # multiple banners
#' mtcars %>% 
#'     tab_cells(cyl) %>% 
#'     tab_cols(total(), vs, am) %>% 
#'     tab_stat_cpct() %>% 
#'     tab_pivot()
#' 
#' # nested banners
#' mtcars %>% 
#'     tab_cells(cyl) %>% 
#'     tab_cols(total(), vs %nest% am) %>% 
#'     tab_stat_cpct() %>% 
#'     tab_pivot()
#' 
#' # summary statistics
#' mtcars %>% 
#'     tab_cells(mpg, disp, hp, wt, qsec) %>%
#'     tab_cols(am) %>% 
#'     tab_stat_fun(Mean = w_mean, "Std. dev." = w_sd, "Valid N" = w_n) %>%
#'     tab_pivot()
#' 
#' # summary statistics - labels in columns
#' mtcars %>% 
#'     tab_cells(mpg, disp, hp, wt, qsec) %>%
#'     tab_cols(am) %>% 
#'     tab_stat_fun(Mean = w_mean, "Std. dev." = w_sd, "Valid N" = w_n, method = list) %>%
#'     tab_pivot()
#' 
#' # subgroup with droping empty columns
#' mtcars %>% 
#'     tab_subgroup(am == 0) %>% 
#'     tab_cells(cyl) %>% 
#'     tab_cols(total(), vs %nest% am) %>% 
#'     tab_stat_cpct() %>% 
#'     tab_pivot() %>% 
#'     drop_empty_columns()
#' 
#' # total position at the top of the table
#' mtcars %>% 
#'     tab_cells(cyl) %>% 
#'     tab_cols(total(), vs) %>% 
#'     tab_rows(am) %>% 
#'     tab_stat_cpct(total_row_position = "above",
#'                   total_label = c("number of cases", "row %"),
#'                   total_statistic = c("u_cases", "u_rpct")) %>% 
#'     tab_pivot()
#' 
#' # this example cannot be made easily with 'cro'             
#' mtcars %>%
#'     tab_cells(am) %>%
#'     tab_cols(total(), vs) %>%
#'     tab_total_row_position("none") %>% 
#'     tab_stat_cpct(label = "col %") %>%
#'     tab_stat_rpct(label = "row %") %>%
#'     tab_stat_tpct(label = "table %") %>%
#'     tab_pivot(stat_position = "inside_rows")
#' 
#' # statistic labels inside columns             
#' mtcars %>%
#'     tab_cells(am) %>%
#'     tab_cols(total(), vs) %>%
#'     tab_total_row_position("none") %>% 
#'     tab_stat_cpct(label = "col %") %>%
#'     tab_stat_rpct(label = "row %") %>%
#'     tab_stat_tpct(label = "table %") %>%
#'     tab_pivot(stat_position = "inside_columns")
#' 
#' # stacked statistics
#' mtcars %>% 
#'     tab_cells(cyl) %>% 
#'     tab_cols(total(), am) %>% 
#'     tab_stat_mean() %>%
#'     tab_stat_se() %>% 
#'     tab_stat_valid_n() %>% 
#'     tab_stat_cpct() %>% 
#'     tab_pivot()
#'     
#' # stacked statistics with section headings
#' mtcars %>% 
#'     tab_cells(cyl) %>% 
#'     tab_cols(total(), am) %>% 
#'     tab_row_label("#Summary statistics") %>% 
#'     tab_stat_mean() %>%
#'     tab_stat_se() %>% 
#'     tab_stat_valid_n() %>% 
#'     tab_row_label("#Column percent") %>% 
#'     tab_stat_cpct() %>% 
#'     tab_pivot()
#' 
#' # stacked statistics with different variables
#' mtcars %>% 
#'     tab_cols(total(), am) %>% 
#'     tab_cells(mpg, hp, qsec) %>% 
#'     tab_stat_mean() %>%
#'     tab_cells(cyl, carb) %>% 
#'     tab_stat_cpct() %>% 
#'     tab_pivot()
#' 
#' # stacked statistics - label position outside row labels
#' mtcars %>% 
#'     tab_cells(cyl) %>% 
#'     tab_cols(total(), am) %>% 
#'     tab_stat_mean() %>%
#'     tab_stat_se %>% 
#'     tab_stat_valid_n() %>% 
#'     tab_stat_cpct(label = "Col %") %>% 
#'     tab_pivot(stat_label = "outside")
#'     
#' # example from 'cross_fun_df' - linear regression by groups with sorting 
#' mtcars %>% 
#'     tab_cells(sheet(mpg, disp, hp, wt, qsec)) %>% 
#'     tab_cols(total(), am) %>% 
#'     tab_stat_fun_df(
#'         function(x){
#'             frm = reformulate(".", response = as.name(names(x)[1]))
#'             model = lm(frm, data = x)
#'             sheet('Coef.' = coef(model), 
#'                   confint(model)
#'             )
#'         }    
#'     ) %>% 
#'     tab_pivot() %>% 
#'     tab_sort_desc()
#' 
#' # multiple-response variables and weight
#' data(product_test)
#' codeframe_likes = num_lab("
#'                           1 Liked everything
#'                           2 Disliked everything
#'                           3 Chocolate
#'                           4 Appearance
#'                           5 Taste
#'                           6 Stuffing
#'                           7 Nuts
#'                           8 Consistency
#'                           98 Other
#'                           99 Hard to answer
#'                           ")
#' 
#' set.seed(1)
#' product_test = product_test %>% 
#'     let(
#'         # recode age by groups
#'         age_cat = recode(s2a, lo %thru% 25 ~ 1, lo %thru% hi ~ 2),
#'         wgt = runif(.N, 0.25, 4),
#'         wgt = wgt/sum(wgt)*.N
#'     ) %>% 
#'     apply_labels(
#'         age_cat = "Age",
#'         age_cat = c("18 - 25" = 1, "26 - 35" = 2),
#'         
#'         a1_1 = "Likes. VSX123",
#'         b1_1 = "Likes. SDF456",
#'         a1_1 = codeframe_likes,
#'         b1_1 = codeframe_likes
#'     )
#' 
#' product_test %>% 
#'     tab_cells(mrset(a1_1 %to% a1_6), mrset(b1_1 %to% b1_6)) %>% 
#'     tab_cols(total(), age_cat) %>% 
#'     tab_weight(wgt) %>% 
#'     tab_stat_cpct() %>% 
#'     tab_sort_desc() %>% 
#'     tab_pivot()
#'     
#' # trick to place cell variables labels inside columns
#' # useful to compare two variables
#' # '|' is needed to prevent automatic labels creation from argument
#' # alternatively we can use list(...) to avoid this
#' product_test %>% 
#'     tab_cols(total(), age_cat) %>% 
#'     tab_weight(wgt) %>% 
#'     tab_cells("|" = unvr(mrset(a1_1 %to% a1_6))) %>% 
#'     tab_stat_cpct(label = var_lab(a1_1)) %>% 
#'     tab_cells("|" = unvr(mrset(b1_1 %to% b1_6))) %>% 
#'     tab_stat_cpct(label = var_lab(b1_1)) %>% 
#'     tab_pivot(stat_position = "inside_columns")
#' 
#' # if you need standard evaluation, use 'vars'
#' tables = mtcars %>%
#'       tab_cols(total(), am %nest% vs)
#' 
#' for(each in c("mpg", "disp", "hp", "qsec")){
#'     tables = tables %>% tab_cells(vars(each)) %>%
#'         tab_stat_fun(Mean = w_mean, "Std. dev." = w_sd, "Valid N" = w_n) 
#' }
#' tables %>% tab_pivot()
#' } 
tab_cols = function(data, ...){
    data = check_class(data)
    expr = substitute(list(...))
    args = calculate_internal(data[[DATA]], expr, parent.frame())
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
    data = check_class(data)
    expr = substitute(list(...))
    args = calculate_internal(data[[DATA]], expr, parent.frame())
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
    data = check_class(data)
    expr = substitute(list(...))
    args = calculate_internal(data[[DATA]], expr, parent.frame())
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
    data = check_class(data)
    expr = substitute(weight)
    tab_weight_internal(data, expr, parent.frame())
}

tab_weight_internal = function(data, expr, parent){
    weight = calculate_internal(data[[DATA]], expr, parent)
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
    data = check_class(data)
    expr = substitute(list(...))
    args = calculate_internal(data[[DATA]], expr, parent.frame())
    if(length(args)>0){
        data[[MIS_VAL]] = unlist(args)
    } else {
        data[[MIS_VAL]] = NULL
    }
    data
}

############

#' @rdname tables
#' @export
tab_total_label = function(data, ...){
    data = check_class(data)
    expr = substitute(list(...))
    args = calculate_internal(data[[DATA]], expr, parent.frame())
    if(length(args)>0){
        data[[TOTAL_LABEL]] = unlist(args)
    } else {
        data[[TOTAL_LABEL]] = NULL
    }
    data
}

############

#' @rdname tables
#' @export
tab_total_statistic = function(data, ...){
    data = check_class(data)
    total_statistic = unlist(list(...))
    if(length(total_statistic)>0){
        unknowns = total_statistic %d% TOTAL_STATISTICS
        stopif(length(unknowns)>0, "unknown total statistics - ", 
               paste(unknowns, collapse = ", "))
        total_statistic = match.arg(total_statistic, TOTAL_STATISTICS, several.ok = TRUE)
        data[[TOTAL_STATISTIC]] = total_statistic
    } else {
        data[[TOTAL_STATISTIC]] = NULL
    }
    data
}




############

#' @rdname tables
#' @export
tab_total_row_position = function(data, 
                                  total_row_position = c("below", "above", "none")){
    data = check_class(data)
    if(!missing(total_row_position)){
        data[[TOTAL_ROW_POSITION]] = match.arg(total_row_position)
    } else {
        data[[TOTAL_ROW_POSITION]] = NULL
    }
    data
}

#################



#' @rdname tables
#' @export
tab_subgroup = function(data, subgroup = NULL){
    data = check_class(data)
    expr = substitute(subgroup)
    tab_subgroup_internal(data, expr, parent.frame())
}

tab_subgroup_internal = function(data, expr, parent){
    subgroup = calculate_internal(data[[DATA]], expr, parent)
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
tab_row_label = function(data, ..., 
                         label = NULL){
    data = check_class(data)
    label = substitute(label)
    tab_row_label_internal(data, 
                           ..., 
                           label_expr = label, 
                           parent = parent.frame()
    )
}


tab_row_label_internal = function(data, ..., label_expr, parent){
    args = substitute(paste(..., sep = "|"))
    row_labels = calculate_internal(data[[DATA]], args, parent)
    label = calculate_internal(data[[DATA]], label_expr, parent)
    result = sheet(row_labels = row_labels)
    class(result) = union("etable", class(result))
    add_result_to_intermediate_table(data, result, label)
}



############

#' @rdname tables
#' @export
tab_stat_fun = function(data, ..., label = NULL, unsafe = FALSE){
    check_class_for_stat(data)
    label = substitute(label)
    tab_stat_fun_internal(data, 
                          ..., 
                          label_expr = label, 
                          unsafe = unsafe, 
                          parent = parent.frame()
    )
}

tab_stat_fun_internal = function(data, ..., label_expr, unsafe, parent){
    args = list(...)
    if(length(args)>1 || !is.null(names(args))){
        fun = combine_functions(...)
    } else {
        fun = args[[1]]
    }
    label = calculate_internal(data[[DATA]], label_expr, parent)
    result = cro_fun(
        cell_vars = get_cells(data),
        col_vars = data[[COL_VAR]],
        row_vars = data[[ROW_VAR]],
        weight = data[[WEIGHT]],
        subgroup = data[[SUBGROUP]],
        fun = fun,
        unsafe = unsafe
    )
    add_result_to_intermediate_table(data, result, label)
    
}

################################

#' @rdname tables
#' @export
tab_stat_mean_sd_n = function(data, weighted_valid_n = FALSE,
                              labels = c("Mean", "Std. dev.", 
                                         ifelse(weighted_valid_n, 
                                                "Valid N", 
                                                "Unw. valid N")
                                         ),
                              label = NULL){
    check_class_for_stat(data)
    label = substitute(label)
    if(weighted_valid_n){
        # here we heavy rely on the fact that weight already checked on NA and negative values
        fun = function(x, weight = NULL) {
            if(is.logical(x)){
                x = as.numeric(x)
            }
            c(
                {
                    res = matrixStats::weightedMean(x, w = weight, na.rm = TRUE)
                    if(is.nan(res)) res = NA
                    res
                },
                weight_helper(matrixStats::weightedSd)(x, weight, na.rm = TRUE), 
                {
                    validn = if(is.null(weight)) sum(!is.na(x)) else sum(weight[!is.na(x)], na.rm = TRUE)
                    if(!is.na(validn) && validn==0) validn = NA
                    validn
                }
            )
        }
    } else {
        fun = function(x, weight = NULL) {
            if(is.logical(x)){
                x = as.numeric(x)
            }
            c(
                {
                    res = matrixStats::weightedMean(x, w = weight, na.rm = TRUE)
                    if(is.nan(res)) res = NA
                    res
                },
                weight_helper(matrixStats::weightedSd)(x, weight, na.rm = TRUE),
                {
                    validn = sum(!is.na(x))
                    if(!is.na(validn) && validn==0) validn = NA
                    validn
                }
            )
        }
    }
    tab_stat_fun_internal(data, 
                          fun, 
                          label_expr = label, 
                          unsafe = labels, 
                          parent = parent.frame()
    ) 
}


#' @rdname tables
#' @export
tab_stat_mean = function(data, label = "Mean"){
    check_class_for_stat(data)
    label = substitute(label)
    tab_stat_fun_internal(data, 
                          w_mean, 
                          label_expr = label, 
                          unsafe = TRUE, 
                          parent = parent.frame()
    ) 
}

#' @rdname tables
#' @export
tab_stat_median = function(data, label = "Median"){
    check_class_for_stat(data)
    label = substitute(label)
    tab_stat_fun_internal(data, 
                          w_median, 
                          label_expr = label, 
                          unsafe = TRUE, 
                          parent = parent.frame()
    )  
}



#' @rdname tables
#' @export
tab_stat_se = function(data, label = "S. E."){
    check_class_for_stat(data)
    label = substitute(label)
    tab_stat_fun_internal(data, 
                          w_se, 
                          label_expr = label, 
                          unsafe = TRUE, 
                          parent = parent.frame()
    )    
}

#' @rdname tables
#' @export
tab_stat_sum = function(data, label = "Sum"){
    check_class_for_stat(data)
    label = substitute(label)
    tab_stat_fun_internal(data, 
                          w_sum, 
                          label_expr = label, 
                          unsafe = TRUE, 
                          parent = parent.frame()
    )   
}

#' @rdname tables
#' @export
tab_stat_min = function(data, label = "Min."){
    check_class_for_stat(data)
    label = substitute(label)
    tab_stat_fun_internal(data, 
                          w_min, 
                          label_expr = label, 
                          unsafe = TRUE, 
                          parent = parent.frame()
    )    
}

#' @rdname tables
#' @export
tab_stat_max = function(data, label = "Max."){
    check_class_for_stat(data)
    label = substitute(label)
    tab_stat_fun_internal(data, 
                          w_max, 
                          label_expr = label, 
                          unsafe = TRUE, 
                          parent = parent.frame()
    )   
}

#' @rdname tables
#' @export
tab_stat_sd = function(data, label = "Std. dev."){
    check_class_for_stat(data)
    label = substitute(label)
    tab_stat_fun_internal(data, 
                          w_sd, 
                          label_expr = label, 
                          unsafe = TRUE, 
                          parent = parent.frame()
    ) 
}

#' @rdname tables
#' @export
tab_stat_valid_n = function(data, label = "Valid N"){
    check_class_for_stat(data)
    label = substitute(label)
    tab_stat_fun_internal(data, 
                          valid_n, 
                          label_expr = label, 
                          unsafe = TRUE, 
                          parent = parent.frame()
    )  
}

#' @rdname tables
#' @export
tab_stat_unweighted_valid_n = function(data, label = "Unw. valid N"){
    check_class_for_stat(data)
    label = substitute(label)
    tab_stat_fun_internal(data, 
                          unweighted_valid_n, 
                          label_expr = label, 
                          unsafe = TRUE, 
                          parent = parent.frame()
    )  
}

#######################################

#' @rdname tables
#' @export
tab_stat_fun_df = function(data, ..., label = NULL, unsafe = FALSE){
    check_class_for_stat(data)
    label = substitute(label)
    tab_stat_fun_df_internal(data, 
                          ..., 
                          label_expr = label, 
                          unsafe = unsafe, 
                          parent = parent.frame()
    )    
}

tab_stat_fun_df_internal = function(data, ..., label_expr, unsafe, parent){
    args = list(...)
    if(length(args)>1 || !is.null(names(args))){
        fun = combine_functions(...)
    } else {
        fun = args[[1]]
    }
    label = calculate_internal(data[[DATA]], label_expr, parent)
    result = cro_fun_df(
        cell_vars = get_cells(data),
        col_vars = data[[COL_VAR]],
        row_vars = data[[ROW_VAR]],
        weight = data[[WEIGHT]],
        subgroup = data[[SUBGROUP]],
        fun = fun,
        unsafe = unsafe
    )
    add_result_to_intermediate_table(data, result, label)
    
}

########################################################

#' @rdname tables
#' @export
tab_stat_cases = function(data, 
                          total_label = NULL,
                          total_statistic = "u_cases",
                          total_row_position = c("below", "above", "none"),
                          label = NULL){
    check_class_for_stat(data)
    label = substitute(label)
    label = calculate_internal(data[[DATA]], label, parent.frame())
    if(missing(total_label) && !is.null(data[[TOTAL_LABEL]])){
        total_label = data[[TOTAL_LABEL]]    
    } else {
        total_label = substitute(total_label)
        total_label = calculate_internal(data[[DATA]], total_label, parent.frame())
    }
    if(missing(total_statistic) && !is.null(data[[TOTAL_STATISTIC]])){
        total_statistic = data[[TOTAL_STATISTIC]]    
    } 
    if(missing(total_row_position) && !is.null(data[[TOTAL_ROW_POSITION]])){
        total_row_position = data[[TOTAL_ROW_POSITION]]    
    } 
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

#' @rdname tables
#' @export
tab_stat_cpct = function(data, 
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none"),
                         label = NULL){
    check_class_for_stat(data)
    label = substitute(label)
    label = calculate_internal(data[[DATA]], label, parent.frame())
    if(missing(total_label) && !is.null(data[[TOTAL_LABEL]])){
        total_label = data[[TOTAL_LABEL]]    
    } else {
        total_label = substitute(total_label)
        total_label = calculate_internal(data[[DATA]], total_label, parent.frame())
    }
    if(missing(total_statistic) && !is.null(data[[TOTAL_STATISTIC]])){
        total_statistic = data[[TOTAL_STATISTIC]]    
    } 
    if(missing(total_row_position) && !is.null(data[[TOTAL_ROW_POSITION]])){
        total_row_position = data[[TOTAL_ROW_POSITION]]    
    }
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

#' @rdname tables
#' @export
tab_stat_cpct_responses =function(data, 
                                  total_label = NULL,
                                  total_statistic = "u_responses",
                                  total_row_position = c("below", "above", "none"),
                                  label = NULL){
    check_class_for_stat(data)
    label = substitute(label)
    label = calculate_internal(data[[DATA]], label, parent.frame())
    if(missing(total_label) && !is.null(data[[TOTAL_LABEL]])){
        total_label = data[[TOTAL_LABEL]]    
    } else {
        total_label = substitute(total_label)
        total_label = calculate_internal(data[[DATA]], total_label, parent.frame())
    }
    if(missing(total_statistic) && !is.null(data[[TOTAL_STATISTIC]])){
        total_statistic = data[[TOTAL_STATISTIC]]    
    } 
    if(missing(total_row_position) && !is.null(data[[TOTAL_ROW_POSITION]])){
        total_row_position = data[[TOTAL_ROW_POSITION]]    
    }
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

#' @rdname tables
#' @export
tab_stat_tpct = function(data, 
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none"),
                         label = NULL){
    check_class_for_stat(data)
    label = substitute(label)
    label = calculate_internal(data[[DATA]], label, parent.frame())
    if(missing(total_label) && !is.null(data[[TOTAL_LABEL]])){
        total_label = data[[TOTAL_LABEL]]    
    } else {
        total_label = substitute(total_label)
        total_label = calculate_internal(data[[DATA]], total_label, parent.frame())
    }
    if(missing(total_statistic) && !is.null(data[[TOTAL_STATISTIC]])){
        total_statistic = data[[TOTAL_STATISTIC]]    
    } 
    if(missing(total_row_position) && !is.null(data[[TOTAL_ROW_POSITION]])){
        total_row_position = data[[TOTAL_ROW_POSITION]]    
    }
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

#' @rdname tables
#' @export
tab_stat_rpct = function(data, 
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none"),
                         label = NULL){
    check_class_for_stat(data)
    label = substitute(label)
    label = calculate_internal(data[[DATA]], label, parent.frame())
    if(missing(total_label) && !is.null(data[[TOTAL_LABEL]])){
        total_label = data[[TOTAL_LABEL]]    
    } else {
        total_label = substitute(total_label)
        total_label = calculate_internal(data[[DATA]], total_label, parent.frame())
    }
    if(missing(total_statistic) && !is.null(data[[TOTAL_STATISTIC]])){
        total_statistic = data[[TOTAL_STATISTIC]]    
    } 
    if(missing(total_row_position) && !is.null(data[[TOTAL_ROW_POSITION]])){
        total_row_position = data[[TOTAL_ROW_POSITION]]    
    }
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

######
check_class_for_stat = function(data){
    stopif(!inherits(data, "intermediate_table"),
           "No data for 'tab_stat_*'. Use at least one of 'tab_cells'/'tab_rows'/'tab_cols' before the 'tab_stat'.")
    TRUE
}

########################

#' @rdname tables
#' @export
tab_last_vstack = function(data, stat_position = c("outside_rows",
                                              "inside_rows"), 
                      stat_label = c("inside", "outside"),
                      label = NULL){
    stopif(!inherits(data, "intermediate_table"), 
           "'tab_last_vstack' - argument 'data' need to be result of 'tab_stats_*'.") 
    result_length = length(data[[RESULT]])
    stopif(result_length < 2, 
           "It is needed at least two 'tab_stat_*' in the intermeadiate tables for 'tab_last_vstack'.")
    stat_position = match.arg(stat_position)
    label = substitute(label)
    label = calculate_internal(data[[DATA]], label, parent.frame())
    two_items = list()
    two_items[[RESULT]] = data[[RESULT]][(result_length-1):result_length]
    two_items[[STAT_LABELS]] = data[[STAT_LABELS]][(result_length-1):result_length] 
    data[[RESULT]][(result_length-1):result_length] = NULL
    data[[STAT_LABELS]] = data[[STAT_LABELS]][-((result_length-1):result_length)]
    res = switch(stat_position, 
                 outside_rows = pivot_rows(two_items, stat_position = "outside", 
                                           stat_label = stat_label),
                 inside_rows = pivot_rows(two_items, stat_position = "inside", 
                                          stat_label = stat_label)
                 
    )
    res[["row_labels"]] = remove_unnecessary_splitters(res[["row_labels"]])
    colnames(res) = remove_unnecessary_splitters(colnames(res))
    rownames(res) = NULL
    add_result_to_intermediate_table(data, res, label)
}

#' @rdname tables
#' @export
tab_last_hstack = function(data, stat_position = c("outside_columns",
                                              "inside_columns"), 
                      stat_label = c("inside", "outside"),
                      label = NULL){
    stopif(!inherits(data, "intermediate_table"), 
           "'tab_last_hstack' - argument 'data' need to be result of 'tab_stats_*'.")   
    result_length = length(data[[RESULT]])
    stopif(result_length < 2, 
           "It is needed at least two 'tab_stat_*' in the intermeadiate tables for 'tab_last_hstack'.")
    stat_position = match.arg(stat_position)
    label = substitute(label)
    label = calculate_internal(data[[DATA]], label, parent.frame())
    two_items = list()
    two_items[[RESULT]] = data[[RESULT]][(result_length-1):result_length]
    two_items[[STAT_LABELS]] = data[[STAT_LABELS]][(result_length-1):result_length] 
    data[[RESULT]][(result_length-1):result_length] = NULL
    data[[STAT_LABELS]] = data[[STAT_LABELS]][-((result_length-1):result_length)]
    res = switch(stat_position, 
                 outside_columns = pivot_columns(two_items, stat_position = "outside", 
                                                 stat_label = stat_label),
                 inside_columns = pivot_columns(two_items, stat_position = "inside", 
                                                stat_label = stat_label)
                 
    )
    res[["row_labels"]] = remove_unnecessary_splitters(res[["row_labels"]])
    colnames(res) = remove_unnecessary_splitters(colnames(res))
    rownames(res) = NULL
    add_result_to_intermediate_table(data, res, label)
}

########
#' @rdname tables
#' @export
tab_pivot = function(data, stat_position = c("outside_rows",
                                             "inside_rows",
                                             "outside_columns",
                                             "inside_columns"), 
                     stat_label = c("inside", "outside")){
    stopif(!inherits(data, "intermediate_table"), 
           "'tab_pivot' - argument 'data' need to be result of 'tab_stats_*'.")
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
    rownames(res) = NULL
    remove_unnecessary_splitters_from_table(res)
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
    replace_last_result(
        data, 
        t(
            get_last_result(data)
        )
    )
}

##################

#' @rdname tables
#' @export
tab_caption = function(data, ...){
    UseMethod("tab_caption")
}

#' @export
tab_caption.etable = function(data, ...){
    set_caption(data, do.call(paste0, list(...)))
}

#' @export
tab_caption.intermediate_table = function(data, ...){
    stop("'tab_caption' should be used after 'tab_pivot'.")
}

################

pivot_rows = function(data, stat_position = c("inside", "outside"), 
                      stat_label = c("inside", "outside")){
    stat_position = match.arg(stat_position)  
    stat_label = match.arg(stat_label)  
    results = data[[RESULT]]
    labels = data[[STAT_LABELS]]
    labels_index = seq_along(labels)
    
    labels_and_index = lapply(labels_index, function(item_num){
        sheet(label_index = rep(item_num, NROW(results[[item_num]])),
             label = rep(labels[item_num], NROW(results[[item_num]]))
        )
    })
    results = Reduce(add_rows, results)
    old_colnames = colnames(results)
    labels_and_index = do.call(rbind, labels_and_index)

    if(stat_position == "inside"){
        labels_and_index[["row_labels"]] = match(results[["row_labels"]], 
                                            unique(results[["row_labels"]])
        )
        results = results[order(labels_and_index[["row_labels"]],
                                labels_and_index[["label_index"]]),
                          ]
        labels_and_index = labels_and_index[order(labels_and_index[["row_labels"]],
                                labels_and_index[["label_index"]]),
                          ]
        
    }
    if(stat_label == "inside"){
        results[[1]] = paste0( results[[1]], "|", labels_and_index[["label"]])     
    } else {
        results[[1]] = paste0( labels_and_index[["label"]], "|", results[[1]])
    }
    colnames(results) = old_colnames

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

check_class = function(data){
    if(inherits(data, "intermediate_table")){
        return(data)
    }
    if(inherits(data, "data.frame")){
        return(make_empty_intermediate_table(data))
    }
    stop("'data' should be data.frame or intermediate table.")
}



################

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
    res[[DATA]] = data
    res[[COL_VAR]] = list(total())
    res[[ROW_VAR]] = list(total(label = ""))
    res[[CELL_VAR]] = list(total())
    res[[SUBGROUP]] = NULL
    res[[WEIGHT]] = NULL
    res[[MIS_VAL]] = NULL
    res[[RESULT]] = list()
    res[[STAT_LABELS]] = character(0)
    class(res) = union("intermediate_table", class(res))
    res[[SIGNIFICANCE_OPTIONS]] = list()
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


get_last_result = function(intermediate_table){
    results = intermediate_table[[RESULT]]
    stopif(length(results)==0, "There are no results yet. Use at least one of 'tab_stat_*' functions.")
    results[[length(results)]]
}

replace_last_result = function(intermediate_table, new_result){
    results = intermediate_table[[RESULT]]
    stopif(length(results)==0, "There are no results yet. Use at least one of 'tab_stat_*' functions.")
    intermediate_table[[RESULT]][[length(results)]] = new_result
    intermediate_table
}
##############

#' @export
print.intermediate_table = function(x, ...){
    cat("Object of class 'intermediate_table'. Use 'tab_pivot' to finish table creation.\n")
}

###############
add_names_to_list = function(args, ...){
    if(length(args)==0) return(NULL)
    possible_names = unlist(lapply(as.list(substitute(list(...)))[-1], expr_to_character))
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
#' @export
#' @rdname cross_cases
#' @usage NULL
calc_cro = function(data,
                    cell_vars, 
                    col_vars = total(), 
                    row_vars = NULL,
                    weight = NULL,
                    subgroup = NULL,
                    total_label = NULL,
                    total_statistic = "u_cases",
                    total_row_position = c("below", "above", "none")
){
    .Deprecated(msg = "'calc_cro' and 'calc_cro_cases' were renamed to 'cross_cases'. To remove this warning just replace them with 'cross_cases' in your code.")
    expr = substitute(cro_cases(cell_vars = cell_vars, 
                                col_vars = col_vars, 
                                row_vars = row_vars,
                                weight = weight,
                                subgroup = subgroup,
                                total_label = total_label,
                                total_statistic = total_statistic,
                                total_row_position = total_row_position)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}

#' @export
#' @rdname cross_cases
#' @usage NULL
calc_cro_cases = calc_cro

########################
#' @export
#' @rdname cross_cases
#' @usage NULL
calc_cro_cpct = function(data,
                         cell_vars, 
                         col_vars = total(), 
                         row_vars = NULL,
                         weight = NULL,
                         subgroup = NULL,
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none")
){
    .Deprecated(msg = "'calc_cro_cpct' was renamed to 'cross_cpct'. To remove this warning just replace 'calc_cro_cpct' with 'cross_cpct' in your code.")
    expr = substitute(cro_cpct(cell_vars = cell_vars, 
                               col_vars = col_vars, 
                               row_vars = row_vars,
                               weight = weight,
                               subgroup = subgroup,
                               total_label = total_label,
                               total_statistic = total_statistic,
                               total_row_position = total_row_position)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}

#' @export
#' @rdname cross_cases
#' @usage NULL
calc_cro_rpct = function(data,
                         cell_vars, 
                         col_vars = total(), 
                         row_vars = NULL,
                         weight = NULL,
                         subgroup = NULL,
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none")
){
    .Deprecated(msg = "'calc_cro_rpct' was renamed to 'cross_rpct'. To remove this warning just replace 'calc_cro_cpct' with 'cross_rpct' in your code.")
    expr = substitute(cro_rpct(cell_vars = cell_vars, 
                               col_vars = col_vars, 
                               row_vars = row_vars,
                               weight = weight,
                               subgroup = subgroup,
                               total_label = total_label,
                               total_statistic = total_statistic,
                               total_row_position = total_row_position)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}

#' @export
#' @rdname cross_cases
#' @usage NULL
calc_cro_tpct = function(data,
                         cell_vars, 
                         col_vars = total(), 
                         row_vars = NULL,
                         weight = NULL,
                         subgroup = NULL,
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none")
){
    .Deprecated(msg = "'calc_cro_tpct' was renamed to 'cross_tpct'. To remove this warning just replace 'calc_cro_tpct' with 'cross_tpct' in your code.")
    expr = substitute(cro_tpct(cell_vars = cell_vars, 
                               col_vars = col_vars, 
                               row_vars = row_vars,
                               weight = weight,
                               subgroup = subgroup,
                               total_label = total_label,
                               total_statistic = total_statistic,
                               total_row_position = total_row_position)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}


#' @export
#' @rdname cross_cases
#' @usage NULL
calc_cro_cpct_responses = function(data,
                                   cell_vars, 
                                   col_vars = total(), 
                                   row_vars = NULL,
                                   weight = NULL,
                                   subgroup = NULL,
                                   total_label = NULL,
                                   total_statistic = "u_responses",
                                   total_row_position = c("below", "above", "none")
){
    .Deprecated(msg = "'calc_cro_cpct_responses' was renamed to 'cross_cpct_responses'. To remove this warning just replace 'calc_cro_cpct_responses' with 'cross_cpct_responses' in your code.")
    expr = substitute(cro_cpct_responses(cell_vars = cell_vars, 
                                         col_vars = col_vars, 
                                         row_vars = row_vars,
                                         weight = weight,
                                         subgroup = subgroup,
                                         total_label = total_label,
                                         total_statistic = total_statistic,
                                         total_row_position = total_row_position)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}

#####

#' @export
#' @rdname cross_fun    
#' @usage NULL
calc_cro_fun = function(data,
                        cell_vars, 
                        col_vars = total(), 
                        row_vars = total(label = ""),
                        weight = NULL,
                        subgroup = NULL,
                        fun, 
                        ...,
                        unsafe = FALSE){
    .Deprecated(msg = "'calc_cro_fun' was renamed to 'cross_fun'. To remove this warning just replace 'calc_cro_fun' with 'cross_fun' in your code.")
    expr = substitute(cro_fun(
        cell_vars = cell_vars, 
        col_vars = col_vars, 
        row_vars = row_vars,
        weight = weight,
        subgroup = subgroup,
        fun = fun, 
        ...,
        unsafe = unsafe)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}

###########################

#' @export
#' @rdname cross_fun
#' @usage NULL
calc_cro_fun_df = function(data,
                           cell_vars, 
                           col_vars = total(), 
                           row_vars = total(label = ""),
                           weight = NULL,
                           subgroup = NULL,
                           fun, 
                           ...,
                           unsafe = FALSE){
    .Deprecated(msg = "'calc_cro_fun_df' was renamed to 'cross_fun_df'. To remove this warning just replace 'calc_cro_fun_df' with 'cross_fun_df' in your code.")
    expr = substitute(cro_fun_df(
        cell_vars = cell_vars, 
        col_vars = col_vars, 
        row_vars = row_vars,
        weight = weight,
        subgroup = subgroup,
        fun = fun, 
        ...,
        unsafe = unsafe)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())    
}

###########################

#' @export
#' @rdname cross_fun
#' @usage NULL
calc_cro_mean = function(data,
                         cell_vars, 
                         col_vars = total(), 
                         row_vars = total(label = ""),
                         weight = NULL,
                         subgroup = NULL
){
    .Deprecated(msg = "'calc_cro_mean' was renamed to 'cross_mean'. To remove this warning just replace 'calc_cro_mean' with 'cross_mean' in your code.")
    expr = substitute(
        cro_mean(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())      
}

#########################

#' @export
#' @rdname cross_fun
#' @usage NULL
calc_cro_mean_sd_n = function(data,
                              cell_vars, 
                              col_vars = total(), 
                              row_vars = total(label = ""),
                              weight = NULL,
                              subgroup = NULL,
                              weighted_valid_n = FALSE,
                              labels = NULL
                              
){
    .Deprecated(msg = "'calc_cro_mean_sd_n' was renamed to 'cross_mean_sd_n'. To remove this warning just replace 'calc_cro_mean_sd_n' with 'cross_mean_sd_n' in your code.")
    expr = substitute(
        cro_mean_sd_n(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup,
            weighted_valid_n =  weighted_valid_n,
            labels = labels)
    )
    calculate_internal(data, expr = expr, parent = parent.frame()) 
}

###########################

#' @export
#' @rdname cross_fun
#' @usage NULL
calc_cro_sum = function(data,
                        cell_vars, 
                        col_vars = total(), 
                        row_vars = total(label = ""),
                        weight = NULL,
                        subgroup = NULL
){
    .Deprecated(msg = "'calc_cro_sum' was renamed to 'cross_sum'. To remove this warning just replace 'calc_cro_sum' with 'cross_sum' in your code.")
    expr = substitute(
        cro_sum(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())      
}    

###########################

#' @export
#' @rdname cross_fun
#' @usage NULL
calc_cro_median = function(data,
                           cell_vars, 
                           col_vars = total(), 
                           row_vars = total(label = ""),
                           weight = NULL,
                           subgroup = NULL
){
    .Deprecated(msg = "'calc_cro_median' was renamed to 'cross_median'. To remove this warning just replace 'calc_cro_median' with 'cross_median' in your code.")
    expr = substitute(
        cro_median(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())      
}

############################

#' @export
#' @rdname cross_fun
#' @usage NULL
calc_cro_pearson = function(data,
                            cell_vars, 
                            col_vars = total(), 
                            row_vars = total(label = ""),
                            weight = NULL,
                            subgroup = NULL
){
    .Deprecated(msg = "'calc_cro_pearson' was renamed to 'cross_pearson'. To remove this warning just replace 'calc_cro_pearson' with 'cross_pearson' in your code.")
    expr = substitute(
        cro_pearson(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup)
    )
    calculate_internal(data, expr = expr, parent = parent.frame()) 
}

############################

#' @export
#' @rdname cross_fun
#' @usage NULL
calc_cro_spearman = function(data,
                             cell_vars, 
                             col_vars = total(), 
                             row_vars = total(label = ""),
                             weight = NULL,
                             subgroup = NULL
){
    .Deprecated(msg = "'calc_cro_spearman' was renamed to 'cross_spearman'. To remove this warning just replace 'calc_cro_spearman' with 'cross_spearman' in your code.")
    expr = substitute(
        cro_spearman(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup)
    )
    calculate_internal(data, expr = expr, parent = parent.frame()) 
}

############################
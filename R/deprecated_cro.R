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
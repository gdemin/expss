

#' Significance testing for custom tables
#'
#' @param data data.frame/intermediate_table 
#' @param sig_level numeric. Significance level - by default it equals to \code{0.05}.
#' @param min_base numeric. Significance test will be conducted if both
#'   columns have bases greater than \code{min_base}. By default it equals to \code{2}.
#' @param delta_cpct numeric. Minimal delta between percent for which we mark 
#'   significant differences (in percent points) - by default it equals to zero.
#'   Note that, for example, for minimal 5 percent difference
#'   \code{delta_cpct} should be equals 5, not 0.05.
#' @param delta_means numeric. Minimal delta between means for which we mark 
#'   significant differences  - by default it equals to zero.
#' @param compare_type Type of compare between columns. By default it is 
#'   \code{subtable} - comparisons will be conducted between columns of each 
#'   subtable. Other possible values are: \code{first_column}, 
#'   \code{adjusted_first_column} and \code{previous_column}. We can conduct
#'   several tests simultaneously.
#' @param bonferroni logical. \code{FALSE} by default. Should we use Bonferroni
#'   adjustment by number of comparisons in each row? 
#' @param subtable_marks character. One of "greater", "both" or "less". By
#'   deafult we mark only values which are significantly greater than some other
#'   columns. We can change this behavior by setting argument to \code{less} or
#'   \code{both}.
#' @param inequality_sign logical. FALSE if \code{subtable_marks} is "less" or 
#'   "greater". Should we show \code{>} or \code{<} before significance marks of
#'   subtable comparisons.
#' @param sig_labels character vector. Labels for marking differences between
#'   columns of subtable.
#' @param sig_labels_previous_column a character vector with two elements. Labels
#'   for marking difference with previous column. First mark means 'lower' (by
#'   default it is \code{v}) and the second means greater (\code{^}).
#' @param sig_labels_first_column a character vector with two elements. Labels
#'   for marking difference with first column of the table. First mark means 'lower' (by
#'   default it is \code{-}) and the second means 'greater' (\code{+}).
#' @param keep character. One or more from "percent", "cases", "means", "bases", 
#'   "sd" or "none". This argument determines which statistics will remain in
#'   the table after significance marking.
#' @param na_as_zero logical. \code{FALSE} by default. Should we treat
#'   \code{NA}'s as zero cases?
#' @param total_marker character. Mark of total rows in table.
#' @param total_row integer/character. In case of several totals per subtable it is
#'   number or name of total row for significance calculation.
#' @param var_equal a logical variable indicating whether to treat the two
#'   variances as being equal. For details see \link[stats]{t.test}.
#' @param digits an integer indicating how much digits after decimal separator
#'   will be shown in the final table.
#'
#' @return All of these functions return object of class 
#'   \code{intermediate_table}. Use \code{tab_pivot} to get final result -
#'   object of class \code{etable}.
#' @export
tab_significance_options = function(data,  
                                    sig_level = 0.05, 
                                    min_base = 2,
                                    delta_cpct = 0,
                                    delta_means = 0,
                                    compare_type ="subtable",
                                    bonferroni = FALSE,
                                    subtable_marks = "greater",
                                    inequality_sign = "both" %in% subtable_marks,
                                    sig_labels = LETTERS,
                                    sig_labels_previous_column = c("v", "^"),
                                    sig_labels_first_column = c("-", "+"),
                                    keep = c("percent", "cases", "means", "sd", "bases"), 
                                    total_marker = "#",
                                    total_row = 1,
                                    digits = get_expss_digits(),
                                    na_as_zero = FALSE,
                                    var_equal = FALSE){
    data = check_class(data)
    sig_options = match.call()[-2]
    sig_options[[1]] = quote(list)
    
    if(length(sig_options)>1){
        env = parent.frame()
        sig_options = calculate_internal(data[[DATA]], sig_options, env)
        data[[SIGNIFICANCE_OPTIONS]][names(sig_options)] = sig_options
    } else {
        data[[SIGNIFICANCE_OPTIONS]] = list()
    }
    data
}


#########

#' @rdname tab_significance_options
#' @export
tab_stat_cpct_significance = function(data, 
                                      sig_level = 0.05, 
                                      delta_cpct = 0,
                                      min_base = 2,
                                      compare_type ="subtable",
                                      bonferroni = FALSE,
                                      subtable_marks = c("greater", "both", "less"),
                                      inequality_sign = "both" %in% subtable_marks,
                                      sig_labels = LETTERS,
                                      sig_labels_previous_column = c("v", "^"),
                                      sig_labels_first_column = c("-", "+"),
                                      keep = c("percent", "bases"), 
                                      na_as_zero = FALSE,
                                      total_marker = "#",
                                      total_row = 1,
                                      digits = get_expss_digits(),
                                      label = NULL){
    check_class_for_stat(data)
    #################
    cpct_sig_params = names(formals(tab_stat_cpct_significance)) %d% c("data", "label")
    sig_options = data[[SIGNIFICANCE_OPTIONS]] %n_i% cpct_sig_params
    curr_sig_options = match.call()[-2]
    curr_sig_options[[1]] = quote(list)
    if(!missing(label)){
        curr_sig_options[["label"]] = NULL
    }
    if(length(curr_sig_options)>1){
        env = parent.frame()
        curr_sig_options = calculate_internal(data[[DATA]], curr_sig_options, env)
        sig_options[names(curr_sig_options)] = curr_sig_options
    } 
    cpct_args = list()
    cpct_args[["total_label"]] = data[[TOTAL_LABEL]]    
    cpct_args[["total_statistic"]] = data[[TOTAL_STATISTIC]]  
    ### rather ugly solution if user don't want bases
    if(!identical(data[[TOTAL_ROW_POSITION]], "none")){
        cpct_args[["total_row_position"]] = data[[TOTAL_ROW_POSITION]]    
    } else {
        if(!("keep" %in% names(sig_options))){
            sig_options[["keep"]] = "percent"
        }
    }
    cpct = do.call(cro_cpct, c(
        list(
            cell_vars = get_cells(data),
            col_vars = data[[COL_VAR]],
            row_vars = data[[ROW_VAR]],
            weight = data[[WEIGHT]],
            subgroup = data[[SUBGROUP]]
        ),
        cpct_args
        )
    )
    res = do.call(significance_cpct, c(list(x = cpct), sig_options))
    #############
    label = substitute(label)
    label = calculate_internal(data[[DATA]], label, parent.frame())
    add_result_to_intermediate_table(data, res, label)
}

######################
#' @rdname tab_significance_options
#' @export
tab_stat_means_significance = function(data, 
                                       weighted_valid_n = FALSE,
                                       labels = c("Mean", "Std. dev.", 
                                                  ifelse(weighted_valid_n, 
                                                         "Valid N", 
                                                         "Unw. valid N")
                                       ),
                                       sig_level = 0.05, 
                                       delta_means = 0,
                                       min_base = 2,
                                       compare_type ="subtable",
                                       bonferroni = FALSE,
                                       subtable_marks = c("greater", "both", "less"),
                                       inequality_sign = "both" %in% subtable_marks,
                                       sig_labels = LETTERS,
                                       sig_labels_previous_column = c("v", "^"),
                                       sig_labels_first_column = c("-", "+"),
                                       keep = c("means", "sd", "bases"), 
                                       var_equal = FALSE,
                                       digits = get_expss_digits(),
                                       label = NULL){
    check_class_for_stat(data)
    #################
    sig_params = names(formals(tab_stat_means_significance)) %d% c("data", "label")
    sig_options = data[[SIGNIFICANCE_OPTIONS]] %n_i% sig_params
    curr_sig_options = match.call()[-2]
    curr_sig_options[[1]] = quote(list)
    if(!missing(label)){
        curr_sig_options[["label"]] = NULL
    }
    if(length(curr_sig_options)>1){
        env = parent.frame()
        curr_sig_options = calculate_internal(data[[DATA]], curr_sig_options, env)
        sig_options[names(curr_sig_options)] = curr_sig_options
    } 
    cpct_args = list()
    cpct_args[["total_label"]] = data[[TOTAL_LABEL]]    
    cpct_args[["total_statistic"]] = data[[TOTAL_STATISTIC]]  
    ### rather ugly solution if user don't want bases
    if(!identical(data[[TOTAL_ROW_POSITION]], "none")){
        cpct_args[["total_row_position"]] = data[[TOTAL_ROW_POSITION]]    
    } else {
        if(!("keep" %in% names(sig_options))){
            sig_options[["keep"]] = "percent"
        }
    }
    cpct = do.call(cro_cpct, c(
        list(
            cell_vars = get_cells(data),
            col_vars = data[[COL_VAR]],
            row_vars = data[[ROW_VAR]],
            weight = data[[WEIGHT]],
            subgroup = data[[SUBGROUP]]
        ),
        cpct_args
    )
    )
    res = do.call(significance_cpct, c(list(x = cpct), sig_options))
    #############
    label = substitute(label)
    label = calculate_internal(data[[DATA]], label, parent.frame())
    add_result_to_intermediate_table(data, res, label)
}

#################################

#' @rdname tab_significance_options
#' @export
tab_last_round = function(data, digits = get_expss_digits()){
    check_class_for_stat(data)
    replace_last_result(
        data, 
        round_dataframe(
            get_last_result(data), 
            digits = digits
        )
    )
}

#' @rdname tab_significance_options
#' @export
tab_last_add_sig_labels = function(data, sig_labels = LETTERS){
    check_class_for_stat(data)
    replace_last_result(
        data, 
        add_sig_labels(
            get_last_result(data), 
            sig_labels = sig_labels
        )
    )
}
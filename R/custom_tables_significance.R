

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
tab_last_sig_cpct = function(data, 
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
                                      mode = c("replace", "append"),
                                      label = NULL
                                      ){
    matched_call = match.call()
    env = parent.frame()
    tab_last_internal_significance(
        data = data,
        matched_call = matched_call,
        sig_params = names(formals(tab_last_sig_cpct)) %d% 
            c("data", "mode", "label"),
        sig_fun = significance_cpct,
        label_expr = substitute(label),
        mode = mode,
        env = parent.frame()
    )
}

######################
#' @rdname tab_significance_options
#' @export
tab_last_sig_means = function(data, 
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
                                       mode = c("replace", "append"),
                                       label = NULL){
    matched_call = match.call()
    env = parent.frame()
    tab_last_internal_significance(
        data = data,
        matched_call = matched_call,
        sig_params = names(formals(tab_last_sig_means)) %d% 
            c("data", "mode", "label"),
        sig_fun = significance_means,
        label_expr = substitute(label),
        mode = mode,
        env = parent.frame()
    )
}

######################
#' @rdname tab_significance_options
#' @export
tab_last_sig_cases = function(data, 
                                 sig_level = 0.05, 
                                 min_base = 2,
                                 keep = c("cases", "bases"), 
                                 total_marker = "#",
                                 total_row = 1,
                                 digits = get_expss_digits(),
                                 mode = c("replace", "append"),
                                 label = NULL){
    matched_call = match.call()
    env = parent.frame()
    tab_last_internal_significance(
        data = data,
        matched_call = matched_call,
        sig_params = names(formals(tab_last_sig_cases)) %d% 
            c("data", "mode", "label"),
        sig_fun = significance_cases,
        label_expr = substitute(label),
        mode = mode,
        env = parent.frame()
    )
}

############################

tab_last_internal_significance = function(data,
                                          matched_call,
                                          sig_params,
                                          sig_fun,
                                          label_expr,
                                          mode = c("replace", "append"),
                                          env){
    check_class_for_stat(data)
    last_table = get_last_result(data)
    #################
    sig_options = data[[SIGNIFICANCE_OPTIONS]] %n_i% sig_params
    curr_sig_options = matched_call
    curr_sig_options[[1]] = quote(list)
    curr_sig_options[c("data", "mode", "label")] = NULL
    
    if(length(curr_sig_options)>1){
        curr_sig_options = calculate_internal(data[[DATA]], curr_sig_options, env)
        sig_options[names(curr_sig_options)] = curr_sig_options
    } 
    res = do.call(sig_fun, c(list(x = last_table), sig_options))
    #############
    mode = match.arg(mode)
    if(mode == "append"){
        label = calculate_internal(data[[DATA]], label_expr, env)
        add_result_to_intermediate_table(data, res, label)
    } else {
        replace_last_result(data, res) 
    }
    
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
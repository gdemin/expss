#' @rdname significance
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
                                    var_equal = FALSE,
                                    mode = c("replace", "append")){
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

#' @rdname significance
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
            c("data", "label"),
        sig_fun = significance_cpct,
        label_expr = substitute(label),
        env = parent.frame()
    )
}

######################
#' @rdname significance
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
            c("data", "label"),
        sig_fun = significance_means,
        label_expr = substitute(label),
        env = parent.frame()
    )
}

######################
#' @rdname significance
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
            c("data", "label"),
        sig_fun = significance_cases,
        label_expr = substitute(label),
        env = parent.frame()
    )
}

############################

tab_last_internal_significance = function(data,
                                          matched_call,
                                          sig_params,
                                          sig_fun,
                                          label_expr,
                                          env){
    check_class_for_stat(data)
    last_table = get_last_result(data)
    #################
    sig_options = data[[SIGNIFICANCE_OPTIONS]] %n_i% sig_params
    curr_sig_options = matched_call
    curr_sig_options[[1]] = quote(list)
    curr_sig_options[c("data", "label")] = NULL
    
    if(length(curr_sig_options)>1){
        curr_sig_options = calculate_internal(data[[DATA]], curr_sig_options, env)
        sig_options[names(curr_sig_options)] = curr_sig_options
    } 
    res = do.call(sig_fun, c(list(x = last_table), sig_options %n_d% "mode"))
    #############
    mode = if_null(sig_options[["mode"]], "replace")
    if(mode == "append"){
        label = calculate_internal(data[[DATA]], label_expr, env)
        add_result_to_intermediate_table(data, res, label)
    } else {
        replace_last_result(data, res) 
    }
    
}
#################################

#' @rdname significance
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

#' @rdname significance
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
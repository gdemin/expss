MEANS_IND = c(TRUE, FALSE, FALSE)
SD_IND =    c(FALSE, TRUE, FALSE)
N_IND =     c(FALSE, FALSE, TRUE)

#' @rdname significance_cpct
#' @export
significance_means = function(x, 
                             sig_level = 0.05, 
                             delta_means = 0,
                             min_base = 2,
                             compare_type ="subtable",
                             bonferroni = FALSE,
                             sig_labels = LETTERS,
                             sig_labels_previous_column = c("v", "^"),
                             sig_labels_first_column = c("-", "+"),
                             keep_means = TRUE,
                             keep_sd = keep_means,
                             keep_bases = keep_means,
                             var_equal = FALSE,
                             digits = get_expss_digits()
){
    UseMethod("significance_means")
}


#' @export
significance_means.etable = function(x, 
                                     sig_level = 0.05, 
                                     delta_means = 0,
                                     min_base = 2,
                                     compare_type ="subtable",
                                     bonferroni = FALSE,
                                     sig_labels = LETTERS,
                                     sig_labels_previous_column = c("v", "^"),
                                     sig_labels_first_column = c("-", "+"),
                                     keep_means = TRUE,
                                     keep_sd = keep_means,
                                     keep_bases = keep_means,
                                     var_equal = FALSE,
                                     digits = get_expss_digits()
){
    
    stopif(NROW(x) %% 3 !=0, 
           "Incorrect table. Table should have rows with means, standard deviations and valid N.")
    if(NCOL(x)<3) return(x)
    
    compare_type = match.arg(compare_type, choices = COMPARE_TYPE, several.ok = TRUE)
    stopif(sum(compare_type %in% c("first_column", "adjusted_first_column"))>1, 
           "mutually exclusive compare types in significance testing:  'first_column' and 'adjusted_first_column'.")
    
    if("subtable" %in% compare_type){
        if(!is.null(sig_labels)){
            x = add_sig_labels(x, sig_labels = sig_labels)
        } 
        all_column_labels = get_category_labels(colnames(x))
    }
    groups = header_groups(colnames(x))
    
    # some types (data.table) doesn't support recycling of logicals
    means_ind = rep_len(MEANS_IND, nrow(x))
    sd_ind = rep_len(SD_IND, nrow(x))
    n_ind = rep_len(N_IND, nrow(x))
    all_means = x[means_ind, ,drop = FALSE]
    all_sds = x[sd_ind, ,drop = FALSE]
    all_ns = x[N_IND, ,drop = FALSE]

    recode(all_ns) = lt(min_base) ~ NA
    
    sig_table = x
    sig_table[, -1] = ""
    
    if(any(c("first_column", "adjusted_first_column") %in% compare_type)){
        sig_table = means_sig_first_column(sig_table = sig_table, 
                                           curr_means = all_means, 
                                           curr_sds = all_sds,
                                           curr_ns = all_ns,
                                           groups = groups,
                                           sig_labels_first_column = sig_labels_first_column,
                                           sig_level = sig_level,
                                           bonferroni = bonferroni,
                                           var_equal = var_equal,
                                           adjust_common_base = "adjusted_first_column" %in% compare_type)
    }
    if(any(c("previous_column") %in% compare_type)){
        sig_table = means_sig_previous_column(sig_table = sig_table, 
                                              curr_means = all_means, 
                                              curr_sds = all_sds,
                                              curr_ns = all_ns,
                                              groups = groups,
                                              sig_labels_previous_column = sig_labels_previous_column,
                                              sig_level = sig_level,
                                              bonferroni = bonferroni,
                                              var_equal = var_equal)
    }
    if("subtable" %in% compare_type){
        sig_table = means_sig_prop(sig_table = sig_table, 
                                   curr_means = all_means, 
                                   curr_sds = all_sds,
                                   curr_ns = all_ns,
                                   groups = groups,
                                   all_column_labels = all_column_labels,
                                   sig_level = sig_level,
                                   bonferroni = bonferroni,
                                   var_equal = var_equal)
    }
        
    x = round_dataframe(x, digits = digits)
    if(keep_means){
        x = format_to_character(x, digits = digits)    
        x[, -1] = paste_df_non_empty(
            x[, -1, drop = FALSE], 
            sig_table[, -1, drop = FALSE],
            sep = " "
        )
    } else {
        x[!total_rows_indicator, -1] = res[!total_rows_indicator, -1, drop = FALSE]
    }
    if(keep_bases) {
        x
    } else {
        x[!total_rows_indicator, ]
    }
}

########################

section_sig_means = function(sig_section, 
                             curr_means, 
                             curr_sds,
                             curr_ns,
                             groups,
                            all_column_labels, 
                            sig_level, 
                            bonferroni,
                            var_equal) {
    for(each_group in groups){
        if(length(each_group)>1){
            if(bonferroni) {
                comparable_values = !(is.na(curr_means[,each_group, drop = FALSE]) |
                    is.na(curr_sds[,each_group, drop = FALSE]) |
                    is.na(curr_ns[,each_group, drop = FALSE]))
                # count number of comaprisons
                valid_values_in_row = rowSums(comparable_values, na.rm = TRUE)
                number_of_comparisons_in_row = valid_values_in_row*(valid_values_in_row-1)/2
                number_of_comparisons_in_row[number_of_comparisons_in_row<0] = 0
                bonferroni_coef = sum(number_of_comparisons_in_row, na.rm = TRUE)
                bonferroni_coef[bonferroni_coef==0] = 1
            } else {
                bonferroni_coef = 1
            } 
            for(col1 in each_group[-length(each_group)]){
                mean1 = curr_means[[col1]] 
                sd1 = curr_sds[[col1]]
                n1 = curr_ns[[col1]]
                for(col2 in (col1 + 1):each_group[length(each_group)]){
                    mean2 = curr_means[[col2]] 
                    sd2 = curr_sds[[col2]]
                    n2 = curr_ns[[col2]]
                    pval = compare_means(mean1 = mean1,
                                         mean2 = mean2,
                                         sd1 = sd1,
                                         sd2 = sd2,
                                         base1 = n1,
                                         base2 = n2,
                                         common_base = 0,
                                         var_equal = var_equal
                                         )
                    if_na(pval) = 1
                    pval = pmin(pval*bonferroni_coef, 1)
                    sig_section[[col1]] = ifelse(mean1>mean2 & pval<sig_level,
                                                 paste_non_empty(sig_section[[col1]],
                                                                 all_column_labels[[col2]],
                                                                 sep = " "),
                                                 sig_section[[col1]]
                    )
                    sig_section[[col2]] = ifelse(mean2>mean1 & pval<sig_level,
                                                 paste_non_empty(sig_section[[col2]], 
                                                                 all_column_labels[[col1]], 
                                                                 sep = " "),
                                                 sig_section[[col2]]
                    )
                    
                    
                }                        
            }        
        }
    }
    sig_section
}

########################

section_sig_previous_column_means = function(sig_section, 
                                             curr_means, 
                                             curr_sds,
                                             curr_ns,
                                             groups,
                                       sig_labels_previous_column, 
                                       sig_level, 
                                       bonferroni,
                                       var_equal) {
    for(each_group in groups){
        if(length(each_group)>1){
            # col1 - current column
            # col2 - previous column
            if(bonferroni) {
                comparable_values = !(is.na(curr_means[,each_group, drop = FALSE]) |
                                          is.na(curr_sds[,each_group, drop = FALSE]) |
                                          is.na(curr_ns[,each_group, drop = FALSE]))
                # count number of comaprisons
                number_of_comparisons_in_row = 0
                for(col1 in seq_len(ncol(comparable_values))[-1]){
                    col2 = col1  - 1
                    number_of_comparisons_in_row = number_of_comparisons_in_row + 
                        comparable_values[col2] & comparable_values[col1]
                }    
                bonferroni_coef = sum(number_of_comparisons_in_row, na.rm = TRUE)
                bonferroni_coef[bonferroni_coef==0] = 1
            } else {
                bonferroni_coef = 1
            } 
            for(col1 in each_group[-1]){
                col2 = col1  - 1
                mean1 = curr_means[[col1]] 
                sd1 = curr_sds[[col1]]
                n1 = curr_ns[[col1]]
                mean2 = curr_means[[col2]] 
                sd2 = curr_sds[[col2]]
                n2 = curr_ns[[col2]]
                pval = compare_means(mean1 = mean1,
                                     mean2 = mean2,
                                     sd1 = sd1,
                                     sd2 = sd2,
                                     base1 = n1,
                                     base2 = n2,
                                     common_base = 0,
                                     var_equal = var_equal
                )
                if_na(pval) = 1
                pval = pmin(pval*bonferroni_coef, 1)
                sig_section[[col1]] = ifelse(pval<sig_level,
                                             # previous value is greater
                                             ifelse(mean2>mean1,
                                                    paste_non_empty(sig_section[[col1]], 
                                                                    sig_labels_previous_column[[1]], 
                                                                    sep = " "),
                                                    # previous value is smaller
                                                    paste_non_empty(sig_section[[col1]], 
                                                                    sig_labels_previous_column[[2]], 
                                                                    sep = " ")
                                             ),
                                             sig_section[[col1]]
                )
            }        
        }
    }
    sig_section
}

########################

section_sig_first_column_means = function(sig_section,
                                          curr_means, 
                                          curr_sds,
                                          curr_ns, 
                                          groups,
                                    sig_labels_first_column,
                                    sig_level, 
                                    bonferroni,
                                    var_equal,
                                    adjust_common_base = FALSE) {
    groups = unlist(groups)
    # col1 - first column
    # col2 - other columns
    col1 = groups[1]
    mean1 = curr_means[[col1]]
    sd1 = curr_sds[[col1]]
    base1 = curr_ns[[col1]]
    if(length(groups)>1){
        if(bonferroni) {
            comparable_values = !(is.na(curr_means[,groups, drop = FALSE]) |
                                      is.na(curr_sds[,groups, drop = FALSE]) |
                                      is.na(curr_ns[,groups, drop = FALSE]))
            # count number of comaprisons
            number_of_comparisons_in_row = comparable_values[,1]
            for(col1 in seq_len(ncol(comparable_values))[-1]){
                number_of_comparisons_in_row = number_of_comparisons_in_row + 
                    comparable_values[1] & comparable_values[col1]
            }    
            bonferroni_coef = sum(number_of_comparisons_in_row, na.rm = TRUE)
            bonferroni_coef[bonferroni_coef==0] = 1
        } else {
            bonferroni_coef = 1
        } 
        for(col2 in groups[-1]){
            mean2 = curr_means[[col2]]
            sd2 = curr_sds[[col2]]
            base2 = curr_ns[[col2]]
            pval = compare_means(mean1, mean2,
                                 sd1, sd2, 
                                 base1, base2,
                                 common_base = base2*adjust_common_base,
                                 var_equal = var_equal)
            if_na(pval) = Inf
            sig_section[[col2]] = ifelse(pval<sig_level,
                                         # previous value is greater
                                         ifelse(prop1>prop2,
                                                paste_non_empty(sig_section[[col2]], 
                                                                sig_labels_first_column[[1]], 
                                                                sep = " "),
                                                # previous value is smaller
                                                paste_non_empty(sig_section[[col2]], 
                                                                sig_labels_first_column[[2]], 
                                                                sep = " ")
                                         ),
                                         sig_section[[col2]]
            )
        }        
    }
    sig_section
}

########################
MEANS_IND = c(TRUE, FALSE, FALSE)
SD_IND =    c(FALSE, TRUE, FALSE)
N_IND =     c(FALSE, FALSE, TRUE)

#' @rdname significance
#' @export
significance_means = function(x, 
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
                                     subtable_marks = c("greater", "both", "less"),
                                     inequality_sign = "both" %in% subtable_marks,
                                     sig_labels = LETTERS,
                                     sig_labels_previous_column = c("v", "^"),
                                     sig_labels_first_column = c("-", "+"),
                                     keep = c("means", "sd", "bases"), 
                                     var_equal = FALSE,
                                     digits = get_expss_digits()
){
    
    stopif((NROW(x) %% 3 !=0) || NROW(x) == 0, 
           "Incorrect table. Table should have rows with means, standard deviations and valid N.")
 
    compare_type = match.arg(compare_type, choices = COMPARE_TYPE, several.ok = TRUE)
    stopif(sum(compare_type %in% c("first_column", "adjusted_first_column"))>1, 
           "mutually exclusive compare types in significance testing:  'first_column' and 'adjusted_first_column'.")
    subtable_marks = match.arg(subtable_marks)
    mark_greater = subtable_marks %in% c("greater", "both")
    mark_less = subtable_marks %in% c("both", "less")
    keep = match.arg(keep, KEEP_STAT, several.ok = TRUE)
    keep_means = "means" %in% keep
    keep_sd = "sd" %in% keep
    keep_bases = "bases" %in% keep
    groups = header_groups(colnames(x))
    if("subtable" %in% compare_type){
        if(!is.null(sig_labels)){
            x = add_sig_labels(x, sig_labels = sig_labels)
        } 
        all_column_labels = get_category_labels(colnames(x))
    }
    # some types (data.table) doesn't support recycling of logicals
    means_ind = rep_len(MEANS_IND, nrow(x))
    sd_ind = rep_len(SD_IND, nrow(x))
    n_ind = rep_len(N_IND, nrow(x))
    all_means = x[means_ind, ,drop = FALSE]
    all_sds = x[sd_ind, ,drop = FALSE]
    all_ns = x[N_IND, ,drop = FALSE]

    recode(all_ns) = lt(min_base) ~ NA
    
    sig_table = x[means_ind, ]
    sig_table[, -1] = ""
    empty_sig_table = sig_table
    if(any(c("first_column", "adjusted_first_column") %in% compare_type)){
        sig_table = section_sig_first_column_means(sig_section = sig_table, 
                                           curr_means = all_means, 
                                           curr_sds = all_sds,
                                           curr_ns = all_ns,
                                           groups = groups,
                                           sig_labels_first_column = sig_labels_first_column,
                                           sig_level = sig_level,
                                           delta_means = delta_means,
                                           bonferroni = bonferroni,
                                           var_equal = var_equal,
                                           adjust_common_base = "adjusted_first_column" %in% compare_type)
    }
    if(any(c("previous_column") %in% compare_type)){
        sig_table = section_sig_previous_column_means(sig_section = sig_table, 
                                              curr_means = all_means, 
                                              curr_sds = all_sds,
                                              curr_ns = all_ns,
                                              groups = groups,
                                              sig_labels_previous_column = sig_labels_previous_column,
                                              sig_level = sig_level,
                                              delta_means = delta_means,
                                              bonferroni = bonferroni,
                                              var_equal = var_equal)
    }
    if("subtable" %in% compare_type){
        prepend = ""
        if(mark_greater){
            if(inequality_sign) {
                prepend = ">"    
            }
            subtable_sig_table = section_sig_means(sig_section = empty_sig_table, 
                                                       curr_means = all_means, 
                                                       curr_sds = all_sds,
                                                       curr_ns = all_ns,
                                                       groups = groups,
                                                       all_column_labels = all_column_labels,
                                                       sig_level = sig_level,
                                                       delta_means = delta_means,
                                                       bonferroni = bonferroni,
                                                       mark_greater = TRUE,
                                                       prepend = prepend,
                                                       var_equal = var_equal)
            for(i in seq_along(sig_table)[-1]){
                sig_table[[i]] = paste_non_empty(sig_table[[i]], 
                                                 subtable_sig_table[[i]],
                                                   sep = " "
                )
            }
        }
        if(mark_less){
            if(inequality_sign) {
                prepend = "<"    
            }
            subtable_sig_table = section_sig_means(sig_section = empty_sig_table, 
                                                   curr_means = all_means, 
                                                   curr_sds = all_sds,
                                                   curr_ns = all_ns,
                                                   groups = groups,
                                                   all_column_labels = all_column_labels,
                                                   sig_level = sig_level,
                                                   delta_means = delta_means,
                                                   bonferroni = bonferroni,
                                                   mark_greater = FALSE,
                                                   prepend = prepend,
                                                   var_equal = var_equal)
            for(i in seq_along(sig_table)[-1]){
                sig_table[[i]] = paste_non_empty(sig_table[[i]], 
                                                 subtable_sig_table[[i]],
                                                 sep = " "
                )
            }
        }
    }
        
    x = round_dataframe(x, digits = digits)
    sig_table_with_rows = x
    sig_table_with_rows[,-1] = ""
    sig_table_with_rows[means_ind, -1] = sig_table[, -1, drop = FALSE]
    if(keep_means){
        x[, -1] = format_to_character(x[, -1], digits = digits)    
        x[, -1] = paste_df_non_empty(
            x[, -1, drop = FALSE], 
            sig_table_with_rows[, -1, drop = FALSE],
            sep = " "
        )
    } else {
        x[means_ind, -1] = sig_table_with_rows[means_ind, -1, drop = FALSE]
    }
    x[means_ind | (keep_sd & sd_ind) | (keep_bases & n_ind), ]
    # class(x) = union("etable", class(x))
    # x
}

########################

section_sig_means = function(sig_section, 
                             curr_means, 
                             curr_sds,
                             curr_ns,
                             groups,
                            all_column_labels, 
                            sig_level, 
                            delta_means,
                            bonferroni,
                            mark_greater, 
                            prepend,
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
                bonferroni_coef = number_of_comparisons_in_row #sum(number_of_comparisons_in_row, na.rm = TRUE)
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
                    if(mark_greater) {
                        comparison = mean1 > mean2
                    } else {
                        comparison = mean2 > mean1
                    }    
                    delta =  abs(mean1 - mean2)
                    sig_section[[col1]] = ifelse(delta>delta_means & 
                                                     comparison & 
                                                     pval<sig_level,
                                                 paste_non_empty(sig_section[[col1]],
                                                                 all_column_labels[[col2]],
                                                                 sep = " "),
                                                 sig_section[[col1]]
                    )
                    sig_section[[col2]] = ifelse(delta>delta_means & 
                                                     !comparison & 
                                                     pval<sig_level,
                                                 paste_non_empty(sig_section[[col2]], 
                                                                 all_column_labels[[col1]], 
                                                                 sep = " "),
                                                 sig_section[[col2]]
                    )
                    
                    
                }                        
            }        
        }
    }
    if(prepend!=""){
        recode(sig_section[,-1]) = neq("") ~ function(x) paste(prepend, x)
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
                                       delta_means,
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
                # count number of comparisons
                number_of_comparisons_in_row = 0
                for(col1 in seq_len(ncol(comparable_values))[-1]){
                    col2 = col1  - 1
                    number_of_comparisons_in_row = number_of_comparisons_in_row + 
                        (comparable_values[ ,col2] & comparable_values[ ,col1])
                }    
                bonferroni_coef = number_of_comparisons_in_row #sum(number_of_comparisons_in_row, na.rm = TRUE)
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
                sig_section[[col1]] = ifelse(abs(mean1 - mean2)>delta_means & pval<sig_level,
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
                                    delta_means,
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
            # count number of comparisons
             bonferroni_coef = rowSums(comparable_values[,-1]) # sum(number_of_comparisons_in_row, na.rm = TRUE)
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
            pval = pmin(pval*bonferroni_coef, 1)
            sig_section[[col2]] = ifelse(abs(mean1 - mean2)>delta_means & pval<sig_level,
                                         # previous value is greater
                                         ifelse(mean1 > mean2,
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
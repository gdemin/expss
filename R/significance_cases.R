#' @export
#' @rdname significance_cpct
significance_cases = function(x, 
                             min_base = 2,
                             keep_cases = TRUE,
                             keep_bases = keep_cases,
                             total_marker = "#",
                             total_row = 1
){
    UseMethod("significance_cases")
}


#' @export
significance_cases.etable = function(x, 
                                    min_base = 2,
                                    keep_cases = TRUE,
                                    keep_bases = keep_cases,
                                    total_marker = "#",
                                    total_row = 1
){
    
    if(NCOL(x)>1) {
        groups = header_groups(colnames(x))
        sections = split_table_by_row_sections(x, total_marker = total_marker, total_row = total_row)
        res = lapply(sections, function(each_section){
            # browser()
            curr_base = extract_total_from_section(each_section, 
                                                   total_marker = total_marker, 
                                                   total_row = total_row)
            recode(curr_base) = lt(min_base) ~ NA
            
            total_rows_indicator = get_total_rows_indicator(each_section, total_marker = total_marker)
            sig_section = each_section[!total_rows_indicator, ]
            curr_cases = each_section[!total_rows_indicator, ]
            curr_cases[,-1] = lapply(curr_cases[,-1], round)
            if_na(curr_cases[,-1]) = 0
            sig_section = section_sig_chisq(sig_section = sig_section, 
                                           curr_props = curr_props, 
                                           curr_base = curr_base,
                                           groups = groups,
                                           sig_level = sig_level)
            
            each_section[,-1] = ""
            each_section[!total_rows_indicator,-1] = sig_section[,-1]
            each_section
        })
        
        res = do.call(add_rows, res)
    } else {
        res = x
        res[, -1] = ""
    }
    total_rows_indicator = get_total_rows_indicator(x, total_marker = total_marker)
    x = round_dataframe(x, digits = digits)
    if(keep_percent){
        x[!total_rows_indicator, ] = format_to_character(x[!total_rows_indicator, ], 
                                                         digits = digits)    
        x[, -1] = paste_df_non_empty(
            x[, -1, drop = FALSE], 
            res[, -1, drop = FALSE],
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

section_sig_prop = function(sig_section, curr_props,  curr_base, groups,
                            all_column_labels, sig_level, delta_cpct, bonferroni) {
    for(each_group in groups){
        if(length(each_group)>1){
            if(bonferroni) {
                invalid_columns = is.na(curr_base[each_group])
                comparable_values = !is.na(curr_props[,each_group, drop = FALSE])
                comparable_values[,invalid_columns] = FALSE
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
                prop1 = curr_props[[col1]]
                base1 = curr_base[[col1]]
                for(col2 in (col1+1):each_group[length(each_group)]){
                    prop2 = curr_props[[col2]]
                    base2 = curr_base[[col2]]
                    pval = compare_proportions(prop1, prop2, 
                                               base1, base2)
                    if_na(pval) = 1
                    pval = pmin(pval*bonferroni_coef, 1)
                    sig_section[[col1]] = ifelse(prop1>prop2 & pval<sig_level & abs(prop1 - prop2)>delta_cpct,
                                                 paste_non_empty(sig_section[[col1]],
                                                                 all_column_labels[[col2]],
                                                                 sep = " "),
                                                 sig_section[[col1]]
                    )
                    sig_section[[col2]] = ifelse(prop2>prop1 & pval<sig_level & abs(prop1 - prop2)>delta_cpct,
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
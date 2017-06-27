MEANS_IND = c(TRUE, FALSE, FALSE)
SD_IND =    c(FALSE, TRUE, FALSE)
N_IND =     c(FALSE, FALSE, TRUE)

#' @rdname significance_cpct
#' @export
significance_means = function(x, 
                             sig_level = 0.05, 
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
    
    sections = split_table_by_row_sections(x, total_marker = total_marker, total_row = total_row)
    res = lapply(sections, function(each_section){
        # browser()
        curr_base = extract_total_from_section(each_section, total_marker = total_marker, total_row = total_row)
        recode(curr_base) = lt(min_base) ~ NA
        
        total_rows_indicator = get_total_rows_indicator(each_section, total_marker = total_marker)
        sig_section = each_section[!total_rows_indicator, ]
        sig_section[, -1] = ""
        curr_props = each_section[!total_rows_indicator, ]
        curr_props[,-1] = curr_props[,-1]/100
        if(na_as_zero){
            if_na(curr_props[,-1]) = 0
        }
        if(any(c("first_column", "adjusted_first_column") %in% compare_type)){
            sig_section = section_sig_first_column(sig_section = sig_section, 
                                                   curr_props = curr_props, 
                                                   curr_base = curr_base,
                                                   groups = groups,
                                                   sig_labels_first_column = sig_labels_first_column,
                                                   sig_level = sig_level,
                                                   bonferroni = bonferroni,
                                                   adjust_common_base = "adjusted_first_column" %in% compare_type)
        }
        if(any(c("previous_column") %in% compare_type)){
            sig_section = section_sig_previous_column(sig_section = sig_section, 
                                                      curr_props = curr_props, 
                                                      curr_base = curr_base,
                                                      groups = groups,
                                                      sig_labels_previous_column = sig_labels_previous_column,
                                                      sig_level = sig_level,
                                                      bonferroni = bonferroni)
        }
        if("subtable" %in% compare_type){
            sig_section = section_sig_prop(sig_section = sig_section, 
                                           curr_props = curr_props, 
                                           curr_base = curr_base,
                                           groups = groups,
                                           all_column_labels = all_column_labels,
                                           sig_level = sig_level,
                                           bonferroni = bonferroni)
        }
        each_section[,-1] = ""
        each_section[!total_rows_indicator,-1] = sig_section[,-1]
        each_section
    })
    
    res = do.call(add_rows, res)
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

section_sig_means = function(sig_section, curr_props,  curr_base, groups,
                            all_column_labels, sig_level, bonferroni) {
    for(each_group in groups){
        if(length(each_group)>1){
            if(bonferroni) {
                valid_columns = !is.na(curr_base[each_group])
                bonferroni_coef = sum(valid_columns)*(sum(valid_columns) - 1)/2*NROW(curr_props)
            } else {
                bonferroni_coef = 1
            }    
            for(col1 in each_group[-length(each_group)]){
                prop1 = curr_props[[col1]]
                base1 = curr_base[[col1]]
                for(col2 in col1:each_group[length(each_group)]){
                    prop2 = curr_props[[col2]]
                    base2 = curr_base[[col2]]
                    pval = compare_proportions(prop1, prop2, 
                                               base1, base2)
                    if_na(pval) = 1
                    pval = pmin(pval*bonferroni_coef, 1)
                    sig_section[[col1]] = ifelse(prop1>prop2 & pval<sig_level,
                                                 paste_non_empty(sig_section[[col1]],
                                                                 all_column_labels[[col2]],
                                                                 sep = " "),
                                                 sig_section[[col1]]
                    )
                    sig_section[[col2]] = ifelse(prop2>prop1 & pval<sig_level,
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

section_sig_previous_column_means = function(sig_section, curr_props,  curr_base, groups,
                                       sig_labels_previous_column, sig_level, bonferroni) {
    for(each_group in groups){
        if(length(each_group)>1){
            # col1 - current column
            # col2 - previous column
            if(bonferroni) {
                valid_columns = !is.na(curr_base[each_group])
                bonferroni_coef = (sum(valid_columns) - 1)*NROW(curr_props)
            } else {
                bonferroni_coef = 1
            } 
            for(col1 in each_group[-1]){
                col2 = col1  - 1
                prop1 = curr_props[[col1]]
                base1 = curr_base[[col1]]
                prop2 = curr_props[[col2]]
                base2 = curr_base[[col2]]
                pval = compare_proportions(prop1, prop2, 
                                           base1, base2)
                if_na(pval) = 1
                pval = pmin(pval*bonferroni_coef, 1)
                sig_section[[col1]] = ifelse(pval<sig_level,
                                             # previous value is greater
                                             ifelse(prop2>prop1,
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

section_sig_first_column_means = function(sig_section, curr_props,  curr_base, groups,
                                    sig_labels_first_column, sig_level, bonferroni,
                                    adjust_common_base = FALSE) {
    groups = unlist(groups)
    # col1 - first column
    # col2 - other columns
    col1 = groups[1]
    prop1 = curr_props[[col1]]
    base1 = curr_base[[col1]]
    if(length(groups)>1 & !is.na(base1)){
        if(bonferroni) {
            valid_columns = !is.na(curr_base)
            bonferroni_coef = (sum(valid_columns) - 1)*NROW(curr_props)
        } else {
            bonferroni_coef = 1
        } 
        for(col2 in groups[-1]){
            prop2 = curr_props[[col2]]
            base2 = curr_base[[col2]][1]
            pval = compare_proportions(prop1, prop2, 
                                       base1, base2,
                                       common_base = base2*adjust_common_base)
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
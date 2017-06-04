#' @rdname significance_cpct
#' @export
significance_means = function(x, 
                             sig_level = 0.05, 
                             min_base = 2,
                             compare_type = "subtable",
                             bonferroni = FALSE,
                             sig_labels = LETTERS,
                             sig_labels_previous_column = c("v", "^"),
                             sig_labels_first_column = c("-", "+"),
                             stat_order = c("mean", "sd", "n"),
                             var_equal = FALSE
){
    UseMethod("significance_means")
}

#' @export
significance_means.etable = function(x, 
                                    sig_level = 0.05, 
                                    min_base = 2,
                                    compare_type = "subtable",
                                    bonferroni = FALSE,
                                    sig_labels = LETTERS,
                                    sig_labels_previous_column = c("v", "^"),
                                    sig_labels_first_column = c("-", "+"),
                                    stat_order = c("mean", "sd", "n"),
                                    var_equal = FALSE
){
    
    compare_type = match.arg(compare_type, choices = PROP_COMPARE_TYPE, several.ok = TRUE)
    expss:::stopif(sum(compare_type %in% c("first_column", "first_column_adjusted"))>1, 
                   "mutually exclusive compare types in significance testing:  'first_column' and 'first_column_adjusted'.")
    
    if("subtable" %in% compare_type){
        if(!is.null(sig_labels)){
            x = add_sig_labels(x, sig_labels = sig_labels)
        }  
        all_column_labels = get_category_labels(colnames(x))
    }
    groups = header_groups(colnames(x))

    res = lapply(sections, function(each_section){
        # browser()
        sig_section = each_section$section
        curr_props = each_section$section
        curr_props[,-1] = curr_props[,-1]/100
        if(na_as_zero){
            if_na(curr_props[,-1]) = 0
        }
        sig_section[, -1] = ""
        curr_base = each_section$total
        recode(curr_base) = lt(min_base) ~ NA
        if(any(c("first_column", "first_column_adjusted") %in% compare_type)){
            sig_section = section_sig_first_column(sig_section = sig_section, 
                                                   curr_props = curr_props, 
                                                   curr_base = curr_base,
                                                   groups = groups,
                                                   sig_labels_first_column = sig_labels_first_column,
                                                   sig_level = sig_level,
                                                   bonferroni = bonferroni,
                                                   adjust_common_base = "first_column_adjusted" %in% compare_type)
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
        sig_section
    })
    
    do.call(add_rows, res)
}

########################


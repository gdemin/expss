#' @export
#' @rdname significance
significance_cases = function(x, 
                              sig_level = 0.05, 
                              min_base = 2,
                              keep = c("cases", "bases"), 
                              total_marker = "#",
                              total_row = 1,
                              digits = get_expss_digits()
){
    UseMethod("significance_cases")
}


#' @export
significance_cases.etable = function(x, 
                                     sig_level = 0.05, 
                                     min_base = 2,
                                     keep = c("cases", "bases"), 
                                     total_marker = "#",
                                     total_row = 1,
                                     digits = get_expss_digits()
){
    
    groups = header_groups(colnames(x))
    sections = split_table_by_row_sections(x, total_marker = total_marker, total_row = total_row)
    keep = match.arg(keep, KEEP_STAT, several.ok = TRUE)
    keep_cases = "cases" %in% keep
    keep_bases = "bases" %in% keep
    res = lapply(sections, function(each_section){
        # browser()
        curr_base = extract_total_from_section(each_section, 
                                               total_marker = total_marker, 
                                               total_row = total_row)
        recode(curr_base) = lt(min_base) ~ NA
        
        total_rows_indicator = get_total_rows_indicator(each_section, total_marker = total_marker)
        curr_cases = each_section[!total_rows_indicator, ]
        curr_cases[,-1] = lapply(curr_cases[,-1], round)
        if_na(curr_cases[,-1]) = 0
        chisq_row = section_sig_chisq(curr_cases = curr_cases, 
                                      curr_base = curr_base,
                                      groups = groups,
                                      sig_level = sig_level)
        
        # we need total only as template so we take first row
        total = each_section[total_rows_indicator, ][1, ]
        chisq_row = make_chisq_row(total, chisq_row, total_marker)
        if(total_rows_indicator[1]){
            #total above
            if(keep_cases){
                chisq_row = rbind(
                    chisq_row,
                    format_to_character(each_section[!total_rows_indicator, ],
                                        digits = digits)    
                )    
            }
            if(keep_bases){
                chisq_row = rbind(
                    format_to_character(each_section[total_rows_indicator, ],
                                        digits = digits),
                    chisq_row
                )      
            }
        } else {
            #total below
            if(keep_cases){
                chisq_row = rbind(
                    format_to_character(each_section[!total_rows_indicator, ],
                                        digits = digits),
                    chisq_row
                )    
            }
            if(keep_bases){
                chisq_row = rbind(
                    chisq_row,
                    format_to_character(each_section[total_rows_indicator, ],
                                        digits = digits)
                )      
            }
        }
        chisq_row
    })
    res = do.call(add_rows, res)
    class(res) = union("etable", class(res))
    
    res
}

########################

make_chisq_row = function(total, chisq_row, total_marker){
    # curr_label = chisq_result_row[[1]][1]
    label = unlist(strsplit(total[[1]], split = total_marker, fixed = TRUE))
    label[length(label)] = "Chi-squared p-value"
    total[[1]] = paste(label, collapse = total_marker)
    total[,-1] = chisq_row[-1]
    total
}



########################

section_sig_chisq = function(curr_cases, curr_base, groups, sig_level) {
    chisq_result_row = curr_base
    chisq_result_row[] = ""
    for(each_group in groups){
        bases = curr_base[each_group]
        cases = curr_cases[, each_group, drop = FALSE]
        cases = as.matrix(cases[,!is.na(bases)])
        first_group_column = each_group[1]
        if(length(cases)>1){
            chisq = suppressWarnings(chisq.test(cases, correct = FALSE))
            pvalue = chisq$p.value
            if_na(pvalue) = 1
            expected = chisq$expected
            if_na(expected) = 0
            df = chisq$parameter
            if(pvalue<sig_level){
                chisq_result_row[first_group_column] = paste0("<", sig_level)
            }
            if(any(expected<5, na.rm = TRUE) && is.finite(df)){
                chisq_result_row[first_group_column] = paste0(
                    chisq_result_row[first_group_column], 
                    " (warn.)"
                )
            }
        }
    }
    chisq_result_row
}
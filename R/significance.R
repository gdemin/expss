PROP_COMPARE_TYPE = c("subtable",
                      "first_column", "first_column_adjusted", 
                      "previous_column")

#' Title
#'
#' @param x gddfg
#' @param sig_level dfgdfg 
#' @param min_base dfgfg
#' @param compare_type dfg
#' @param sig_labels dfg
#' @param sig_labels_previous_column dfgdfg
#' @param sig_labels_first_column dfgdfgfd
#'
#' @return fsdgsdg
#' @export
#'
#' @examples
#' 1
significance_cpct = function(x, 
                             sig_level = 0.05, 
                             min_base = 2,
                             compare_type ="subtable",
                             bonferroni = FALSE,
                             sig_labels = LETTERS,
                             sig_labels_previous_column = c("v", "^"),
                             sig_labels_first_column = c("-", "+"),
                             na_as_zero = FALSE,
                             total_marker = "#",
                             total_row = 1
                             ){
    UseMethod("significance_cpct")
}

#' @export
significance_cpct.etable = function(x, 
                                    sig_level = 0.05, 
                                    min_base = 2,
                                    compare_type = "subtable",
                                    bonferroni = FALSE,
                                    sig_labels = LETTERS,
                                    sig_labels_previous_column = c("v", "^"),
                                    sig_labels_first_column = c("-", "+"),
                                    na_as_zero = FALSE,
                                    total_marker = "#",
                                    total_row = 1
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
    sections = split_table_by_row_sections(x, total_marker = total_marker, total_row = total_row)
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

section_sig_prop = function(sig_section, curr_props,  curr_base, groups,
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
                                                 paste(sig_section[[col1]], all_column_labels[[col2]], sep = " "),
                                                 sig_section[[col1]]
                    )
                    sig_section[[col2]] = ifelse(prop2>prop1 & pval<sig_level,
                                                 paste(sig_section[[col2]], all_column_labels[[col1]], sep = " "),
                                                 sig_section[[col2]]
                    )
                    
                    
                }                        
            }        
        }
    }
    sig_section
}

########################

section_sig_previous_column = function(sig_section, curr_props,  curr_base, groups,
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
                                                    paste(sig_section[[col1]], sig_labels_previous_column[[1]], sep = " "),
                                                    # previous value is smaller
                                                    paste(sig_section[[col1]], sig_labels_previous_column[[2]], sep = " ")
                                             ),
                                             sig_section[[col1]]
                )
            }        
        }
    }
    sig_section
}

########################

section_sig_first_column = function(sig_section, curr_props,  curr_base, groups,
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
                                                paste(sig_section[[col2]], sig_labels_first_column[[1]], sep = " "),
                                                # previous value is smaller
                                                paste(sig_section[[col2]], sig_labels_first_column[[2]], sep = " ")
                                         ),
                                         sig_section[[col2]]
            )
        }        
    }
    sig_section
}

########################

get_category_labels = function(header){
    header = t(split_labels(header, remove_repeated = FALSE)) 
    if(NROW(header)<2){
        return(c(header, recursive = TRUE, use.names = FALSE))
    }
    res = apply(header, 2, function(x){
        x = x %d% c(NA, "", perl("^\\s+$"))
        if(length(x)>0){
            x[length(x)]
        } else {
            ""
        }
    })
    c(res, recursive = TRUE, use.names = FALSE)
}

########################

header_groups = function(header){
    header = header[-1]
    header = t(split_labels(header, remove_repeated = FALSE))   
    if(NCOL(header)<2){
        return(list(numeric(0)))
    }
    if(NROW(header)<2){
        # '+ 1' because of first column with row_labels
        return(list(seq_len(NCOL(header))+1))
    }
    res = expss:::matrix_to_cgroup(header)$n.cgroup
    is_section_header = res %row_in% gt(1)
    if(!any(is_section_header)){
        # '+ 1' because of first column with row_labels
        return(list(seq_len(NCOL(header))+1)) 
    }
    is_section_header = which(is_section_header)
    res = res[is_section_header[length(is_section_header)], ] %d% NA
    res = lapply(res, seq_len)
    # '+ 1' because of first column with row_labels
    res[[1]] = res[[1]] + 1 
    for(each in seq_along(res)[-1]){
        res[[each]] = res[[each]] + res[[each-1]][length(res[[each-1]])] 
    }
    res
    
}


########################

split_table_by_row_sections = function(tbl, total_marker = "#", total_row = 1){
    totals = grepl(total_marker, tbl[[1]], perl = TRUE)
    if_na(totals) = FALSE
    expss:::stopif(!any(totals), "significance testing - total rows not found.")
    total_above = totals[1]
    if(total_above){
        sections = cumsum(totals)
    } else {
        sections = -rev(cumsum(rev(totals)))
    }
    sections = split(tbl, sections)
    res = lapply(sections, function(curr_section) {
        curr_totals = grepl(total_marker, curr_section[[1]], perl = TRUE)
        total = curr_section[curr_totals,, drop = FALSE]
        curr_section = curr_section[!curr_totals, drop = FALSE]
        
        if(is.character(total_row)){
            total = total[grepl(total_row, total[[1]], perl = TRUE), , drop = FALSE]
            expss:::stopif(nrow(total)<1, "significance testing - base not found: ", total_row)
        } else {
            total = total[total_row, , drop = FALSE]
        }
        total[[1]] = NA # it is supposed to be character (row_labels) so we change it
        total = unlist(total[1,])  # [1,] if we by occasion select several rows 
        list(section = curr_section,
             total = total
             )
        
    })
    unname(res)
}


########################

#' @rdname significance_cpct
#' @export
add_sig_labels = function(tbl, sig_labels = LETTERS){
    header = colnames(tbl)
    groups = header_groups(header)   
    for(each_group in groups){
        if(length(each_group)<=length(sig_labels)){
            header[each_group] = paste0(header[each_group], "|", 
                                        sig_labels[each_group - min(each_group)+1])
        } else {
            numbers = seq_len(length(each_group)/length(sig_labels) + 1)
            long_labels = rep(sig_labels, length(numbers))
            numbers = rep(numbers, each = length(sig_labels))
            long_labels = paste0(long_labels, numbers)
            header[each_group] = paste0(header[each_group], "|", 
                                        long_labels[each_group - min(each_group)+1])
            
        }
    }
    expss:::remove_unnecessary_splitters(header)
    colnames(tbl) = header
    tbl
}


########################

#' @rdname significance_cpct
#' @export
compare_proportions = function(prop1, prop2, base1, base2, common_base = 0){
    # ftp://public.dhe.ibm.com/software/analytics/spss/documentation/statistics/20.0/en/client/Manuals/IBM_SPSS_Statistics_Algorithms.pdf
    # IBM SPSS Statistics Algorithms v20, p. 263
    pooled_prop = (prop1*base1 + prop2*base2)/(base1 + base2)
    z_statistic = (prop1 - prop2)/
        sqrt(pooled_prop*(1 - pooled_prop)*(1/base1 + 1/base2 - 2*common_base/base1/base2))
    2*(1 - pnorm(abs(z_statistic)))
} 

########################

#' @rdname significance_cpct
#' @export
compare_means = function(mean1, mean2, sd1, sd2, base1, base2, common_base = 0, var_equal = FALSE){
    # ftp://public.dhe.ibm.com/software/analytics/spss/documentation/statistics/20.0/en/client/Manuals/IBM_SPSS_Statistics_Algorithms.pdf
    # IBM SPSS Statistics Algorithms v20, p. 267
    if(common_base>0 || var_equal){
        pooled_sd = sqrt((sd1*sd1*(base1 - 1) + sd2*sd2*(base2 - 1))/(base1 + base2 - 2))
        t_statistic = (mean1 - mean2)/
            pooled_sd/sqrt(1/base1 + 1/base2 - 2*common_base/base1/base2)
        2*pt(-abs(t_statistic), df = base1 + base2 - common_base - 2)
    } else {
        # R Core Team (2017). R: A language and environment for statistical computing. R Foundation for
        # Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
        # t.test(..., var.equal = FALSE)
        stderr1 = sd1/sqrt(base1)
        stderr2 = sd2/sqrt(base2)
        stderr = sqrt(stderr1^2 + stderr2^2)
        df = stderr^4/(stderr1^4/(base1 - 1) + stderr2^4/(base2 - 1))
        t_statistic = (mean1 - mean2)/stderr
        2 * pt(-abs(t_statistic), df)
    }
} 
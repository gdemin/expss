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
                             sig_level = 0.95, 
                             min_base = 2,
                             compare_type = c("subtable", "subtable_bonferrony",
                                              "first_column", "first_column_adjusted", 
                                              "previous_column"),
                             sig_labels = LETTERS,
                             sig_labels_previous_column = c("-", "+"),
                             sig_labels_first_column = c("-", "+"),
                             na_as_zero = FALSE,
                             total_marker = "#",
                             total_row = 1
                             ){
    UseMethod("significance_cpct")
}

#' @export
significance_cpct.etable = function(x, 
                                    sig_level = 0.95, 
                                    min_base = 2,
                                    compare_type = c("subtable", "subtable_bonferrony",
                                                     "first_column", "first_column_adjusted", 
                                                     "previous_column"),
                                    sig_labels = LETTERS,
                                    sig_labels_previous_column = c("-", "+"),
                                    sig_labels_first_column = c("-", "+"),
                                    na_as_zero = FALSE,
                                    total_marker = "#",
                                    total_row = 1
){
    
    compare_type = match.arg(compare_type, several.ok = TRUE)
    expss:::stopif(sum(compare_type %in% c("subtable", "subtable_bonferrony"))>1, 
           "mutually exclusive compare types in significance testing:  'subtable' and 'subtable_bonferrony'." )
    expss:::stopif(sum(compare_type %in% c("first_column", "first_column_adjusted"))>1, 
           "mutually exclusive compare types in significance testing:  'first_column' and 'first_column_adjusted'.")
    
    if(any(c("subtable", "subtable_bonferrony") %in% compare_type)){
        if(!is.null(sig_labels)){
            x = add_letters(x, labels = sig_labels)
        }  
        all_column_labels = get_category_labels(colnames(x))
        groups = header_groups(colnames(x))
        sections = split_table_by_row_sections(x, total_marker = total_marker)
        res = lapply(sections, function(each_section){
            # browser()
            sig_section = each_section[[1]]
            curr_props = each_section[[1]]
            curr_props[,-1] = curr_props[,-1]/100
            if(na_as_zero){
                if_na(curr_props[,-1]) = 0
            }
            sig_section[, -1] = ""
            curr_base = each_section[[2]]
            if(is.character(total_row)){
                curr_base = curr_base[grepl(total_row, curr_base[[1]], perl = TRUE), , drop = FALSE]
                expss:::stopif(nrow(curr_base)<1, "significance testing - base not found: ", total_row)
            } else {
                curr_base = curr_base[total_row, , drop = FALSE]
            }
            if_na(curr_base) = 0
            for(each_group in groups){
                if(length(each_group)>1){
                    for(col1 in each_group[-length(each_group)]){
                        for(col2 in col1:each_group[length(each_group)]){
                            base1 = curr_base[[col1]][1]
                            base2 = curr_base[[col2]][1]
                            if(base1>min_base & base2>min_base){
                                pval = prop_pvalue(curr_props[[col1]], curr_props[[col2]], 
                                                   curr_base[[col1]][1], curr_base[[col2]][1])
                                if_na(pval) = Inf
                                sig_section[[col1]] = ifelse(curr_props[[col1]]>curr_props[[col2]] & pval<sig_level,
                                                             paste(sig_section[[col1]], all_column_labels[[col2]], sep = " "),
                                                             sig_section[[col1]]
                                                             )
                                sig_section[[col2]] = ifelse(curr_props[[col2]]>curr_props[[col1]] & pval<sig_level,
                                                             paste(sig_section[[col2]], all_column_labels[[col1]], sep = " "),
                                                             sig_section[[col2]]
                                )
                            }
                            
                        }                        
                    }        
                }
            }
            sig_section
        })
    }
    res = do.call(add_rows, res)
    list(x, res)
}

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


split_table_by_row_sections = function(tbl, total_marker = "#"){
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
        list(section = curr_section[!curr_totals, drop = FALSE],
             totals = curr_section[curr_totals, drop = FALSE]
             )
        
    })
    unname(res)
}

add_letters = function(tbl, labels = LETTERS){
    header = colnames(tbl)
    groups = header_groups(header)   
    for(each_group in groups){
        if(length(each_group)<=length(labels)){
            header[each_group] = paste0(header[each_group], "|", 
                                        labels[each_group - min(each_group)+1])
        } else {
            numbers = seq_len(length(each_group)/length(labels) + 1)
            long_labels = rep(labels, length(numbers))
            numbers = rep(numbers, each = length(labels))
            long_labels = paste0(long_labels, numbers)
            header[each_group] = paste0(header[each_group], "|", 
                                        long_labels[each_group - min(each_group)+1])
            
        }
    }
    expss:::remove_unnecessary_splitters(header)
    colnames(tbl) = header
    tbl
}


prop_pvalue = function(prop1, prop2, base1, base2, common_base = 0){
    # ftp://public.dhe.ibm.com/software/analytics/spss/documentation/statistics/20.0/en/client/Manuals/IBM_SPSS_Statistics_Algorithms.pdf
    # IBM SPSS Statistics Algorithms v20, p. 263
    base1 = round(base1)
    base2 = round(base2)
    pooled_prop = (prop1*base1 + prop2*base2)/(base1 + base2)
    z_statistic = (prop1 - prop2)/
        sqrt(pooled_prop*(1 - pooled_prop)*(1/base1 + 1/base2 - 2*common_base/base1/base2))
    2*(1 - pnorm(abs(z_statistic)))
} 
#' @export
#' @rdname significance
significance_cell_chisq = function(x, 
                                   sig_level = 0.05, 
                                   min_base = 2,
                                   subtable_marks = c("both", "greater", "less"),
                                   sig_labels_chisq = c("<", ">"),
                                   correct = TRUE,
                                   keep = c("percent", "bases", "none"), 
                                   row_margin = c("auto", "sum_row", "first_column"),
                                   total_marker = "#",
                                   total_row = 1,
                                   total_column_marker = "#",
                                   digits = get_expss_digits()
){
    UseMethod("significance_cell_chisq")
}

#' @export
significance_cell_chisq.etable = function(x, 
                                          sig_level = 0.05, 
                                          min_base = 2,
                                          subtable_marks = c("both", "greater", "less"),
                                          sig_labels_chisq = c("<", ">"),
                                          correct = TRUE,
                                          keep = c("percent", "bases"), 
                                          row_margin = c("auto", "sum_row", "first_column"),
                                          total_marker = "#",
                                          total_row = 1,
                                          total_column_marker = "#",
                                          digits = get_expss_digits()
){
    stopifnot(
        is.numeric(sig_level) && length(sig_level)==1 && sig_level<=1 && sig_level>=0,
        is.numeric(min_base) && length(min_base)==1 && min_base>=0,
        length(sig_labels_chisq) == 2,
        is.logical(correct) && !is.na(correct),
        is.character(total_marker) && length(total_marker)==1,
        (is.numeric(total_row)||is.character(total_row)) && length(total_row)==1 && total_row>=1L,
        is.numeric(digits) && length(digits)==1 && digits>=0
    )
    subtable_marks = match.arg(subtable_marks)
    keep = match.arg(keep, KEEP_STAT, several.ok = TRUE)
    row_margin = match.arg(row_margin)
    mark_greater = subtable_marks %in% c("greater", "both")
    mark_less = subtable_marks %in% c("both", "less")
    
    keep_percent = "percent" %in% keep
    keep_bases = "bases" %in% keep
    if(NCOL(x)>1){
        groups = header_groups(colnames(x))
        sections = split_table_by_row_sections(x, total_marker = total_marker, total_row = total_row)
        res = lapply(sections, function(each_section){
            curr_base = extract_total_from_section(each_section, total_marker = total_marker, total_row = total_row)
            total_rows_indicator = get_total_rows_indicator(each_section, total_marker = total_marker)
            empty_sig_section = each_section[!total_rows_indicator, ]
            empty_sig_section[, -1] = ""
            sig_section = empty_sig_section
            curr_props = each_section[!total_rows_indicator, ]
            curr_props[,-1] = curr_props[,-1]/100
            if_na(curr_props[,-1]) = 0
            
            subtable_sig_section = section_sig_cell_chisq(sig_section = empty_sig_section, 
                                                          curr_props = curr_props, 
                                                          curr_base = curr_base,
                                                          min_base = min_base,
                                                          groups = groups,
                                                          sig_level = sig_level,
                                                          sig_labels_chisq = sig_labels_chisq,
                                                          mark_greater = mark_greater,
                                                          mark_less = mark_less,
                                                          correct = correct, 
                                                          row_margin = row_margin,
                                                          total_column_marker = total_column_marker
            )
            for(i in seq_along(sig_section)[-1]){
                sig_section[[i]] = paste_non_empty(sig_section[[i]], 
                                                   subtable_sig_section[[i]],
                                                   sep = " "
                )
            }
            
            
            
            each_section[,-1] = ""
            each_section[!total_rows_indicator,-1] = sig_section[,-1]
            each_section
        })
        
        res = do.call(add_rows, res)
    } else {
        res = x
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


section_sig_cell_chisq = function(sig_section, 
                                  curr_props,  
                                  curr_base, 
                                  min_base,
                                  groups,
                                  sig_level, 
                                  sig_labels_chisq,
                                  mark_greater, 
                                  mark_less, 
                                  correct,
                                  row_margin,
                                  total_column_marker) {
    
    if(identical(row_margin, "first_column")){
        first_column = groups[[1]][1]
        total_cpct = curr_props[[first_column]]  
        total_base = curr_base[first_column]
        row_base = total_cpct*total_base
        
        groups[[1]] = groups[[1]][-1]
    }
    for(each_group in groups){
        if(length(each_group)>1){
            col_base = curr_base[each_group]
            curr_cpct = curr_props[,each_group]
            curr_cases = cpct_to_cases(curr_cpct, col_base)
            if (row_margin %in% c("auto", "sum_row")) {
                compute_margin = identical(row_margin, "sum_row")
                if (!compute_margin) {
                    total_column = grep(total_column_marker, colnames(curr_cpct), perl = TRUE)
                    if (length(total_column)>0) {
                        row_base = curr_cases[, total_column[1]] # curr_cases is matrix 
                        total_cpct = curr_cpct[[total_column[1]]]
                        total_base = col_base[total_column[1]]
                        
                        curr_cpct = curr_cpct[,-total_column[1], drop = FALSE] 
                        curr_cases = curr_cases[,-total_column[1], drop = FALSE]
                        col_base = col_base[-total_column[1]] 
                        each_group = each_group[-total_column[1]]
                    } else {
                        compute_margin = TRUE
                    }
                }
                if (compute_margin) {
                    row_base = rowSums(curr_cases, na.rm = TRUE)
                    total_base = sum(col_base, na.rm = TRUE)
                    total_cpct = row_base / total_base
                }
            } 
            ###### 
            recode(col_base) = lt(min_base) ~ 0
            pval = cell_chisq(curr_cases,
                              row_base = row_base,
                              col_base = col_base,
                              total_base = total_base,
                              correct = correct
            )
            if_na(pval) = 1
            greater_than_total = curr_cpct>total_cpct
            if(mark_greater){
                sig_section[,each_group] = ifelse(greater_than_total & pval<sig_level,
                                                  sig_labels_chisq[[2]],
                                                  ""
                )
            }
            if(mark_less){
                sig_section[,each_group] = ifelse(!greater_than_total & pval<sig_level,
                                                  sig_labels_chisq[[1]],
                                                  as.matrix(sig_section[,each_group])
                )
            }
        } 
    }
    
    sig_section
}

cpct_to_cases = function(cpct, col_base){
    cpct = as.matrix(cpct)
    round(t(t(cpct)*col_base))
}

#' @param  cases_matrix numeric matrix with counts size R*C
#' @param  row_base numeric vector with row bases, length R
#' @param  col_base numeric vector with col bases, length C
#' @param  total_base numeric single value, total base
#' @rdname significance
#' @export
cell_chisq = function(cases_matrix, row_base, col_base, total_base, correct){
    cases_matrix = as.matrix(cases_matrix)
    res = cases_matrix
    res[] = NA
    for(i in seq_along(row_base)) for(j in seq_along(col_base)){
        curr_row_base = row_base[i]
        curr_col_base = col_base[j]
        curr_cases = cases_matrix[i, j]
        if(curr_row_base>0 && curr_col_base>0 && curr_row_base<total_base && curr_col_base<total_base){
            tbl = rbind(
                c(curr_cases, curr_row_base - curr_cases),
                c(curr_col_base - curr_cases, total_base - curr_row_base - curr_col_base + curr_cases)
            )
            res[i, j] = chisq.test(tbl, correct = correct)[["p.value"]]
        }
    }
    res
}



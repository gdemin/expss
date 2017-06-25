PROP_COMPARE_TYPE = c("subtable",
                      "first_column", "first_column_adjusted", 
                      "previous_column")

#' Mark significant differences between columns of the table
#'
#' @param x table with proportions and bases - result of \link{cro_cpct} for \code{significance_cpct}
#' @param sig_level significance level - by default it equals to \code{0.05}
#' @param min_base integer number. Significance test will be conducted if both
#'   columns have bases greater than \code{min_base}. By default it equals to \code{2}.
#' @param compare_type Type of compare between columns. By default it is 
#'   \code{subtable} - comparisons will be conducted between columns of each 
#'   subtable. Other possible values are: \code{first_column},
#'   \code{first_column_adjusted} and \code{previous_column}
#' @param bonferroni logical. \code{FALSE} by default. Should we use Bonferrony
#'   adjustment for multiple comparisons?
#' @param sig_labels character vector. Labels for marking differences between columns of subtable.
#' @param sig_labels_previous_column character vector with two elements. Labels
#'   for marking difference with previous column. First mark means 'lower' (by
#'   default it is \code{v}) and the second means greater (\code{^}).
#' @param sig_labels_first_column character vector with two elements. Labels
#'   for marking difference with first column of the table. First mark means 'lower' (by
#'   default it is \code{-}) and the second means 'greater' (\code{+}).
#' @param na_as_zero logical. \code{FALSE} by default. Should we treat \code{NA}'s as zero cases?
#' @param total_marker character. Mark of total rows in table.
#' @param total_row integer number. In case of several totals per subtable it is
#'   number of total row for significance calculation.
#'
#' @return Object of class \code{etable} with marks of significant differences between columns.
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
                             keep_numbers = FALSE,
                             keep_bases = keep_numbers,
                             na_as_zero = FALSE,
                             total_marker = "#",
                             total_row = 1,
                             digits = get_expss_digits()
                             ){
    UseMethod("significance_cpct")
}

paste_non_empty = function(x, y, sep = ""){
    res = paste(x, y, sep = sep)
    recode(res) = (x %in% c("", NA)) ~ y
    recode(res) = (y %in% c("", NA)) ~ x
    res
}

paste_df_non_empty = function(df1, df2, sep = ""){
    for(i in seq_along(df1)){
        max_width = max(nchar( df2[[i]]), na.rm = TRUE)
        df2[[i]] = format(df2[[i]], width = max_width) # , flag = "+"
        df1[[i]] = paste_non_empty(df1[[i]], df2[[i]], sep = sep)
    }
    df1
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
                                    keep_numbers = TRUE,
                                    keep_bases = keep_numbers,
                                    na_as_zero = FALSE,
                                    total_marker = "#",
                                    total_row = 1,
                                    digits = get_expss_digits()
){
    
    if(NCOL(x)<3) return(x)
    compare_type = match.arg(compare_type, choices = PROP_COMPARE_TYPE, several.ok = TRUE)
    stopif(sum(compare_type %in% c("first_column", "first_column_adjusted"))>1, 
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
        each_section[,-1] = ""
        each_section[!total_rows_indicator,-1] = sig_section[,-1]
        each_section
    })
    
    res = do.call(add_rows, res)
    total_rows_indicator = get_total_rows_indicator(x, total_marker = total_marker)
    x = round_dataframe(x, digits = digits)
    if(keep_numbers){
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
    # impossible situation because we doesn't test tables with num. of. cols.<=2  
    # if(NCOL(header)<2){
    #     return(list(numeric(0)))
    # }
    if(NROW(header)<2){
        # '+ 1' because of first column with row_labels
        return(list(seq_len(NCOL(header))+1))
    }
    res = matrix_to_cgroup(header)$n.cgroup
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
    totals = get_total_rows_indicator(tbl, total_marker)
    if_na(totals) = FALSE
    stopif(!any(totals), 
           "significance testing - total rows not found. Incorrect total marker: ","'", total_marker, "'")
    total_above = totals[1]
    if(total_above){
        splitter = c(FALSE, totals[-length(totals)] < totals[-1])
    } else {
        splitter = c(FALSE, totals[-length(totals)] > totals[-1])
    }
    sections = cumsum(splitter)
    unname(split(tbl, sections))
}

extract_total_from_section = function(section, total_marker = "#", total_row = 1){
    curr_totals = get_total_rows_indicator(tbl = section, total_marker = total_marker)
    total = section[curr_totals,, drop = FALSE]
    if(is.character(total_row)){
        total = total[grepl(total_row, total[[1]], perl = TRUE), , drop = FALSE]
        stopif(nrow(total)<1, "significance testing - base not found: ", total_row)
    } else {
        stopif(nrow(total)<total_row, 
               "significance testing - base not found, too large 'total_row': ", total_row)
        total = total[total_row, , drop = FALSE]
    }
    total[[1]] = NA # it is supposed to be character (row_labels) so we change it
    unlist(total[1,])  # [1,] if we by occasion select several rows 
}


get_total_rows_indicator = function(tbl, total_marker = "#"){
    grepl(total_marker, tbl[[1]], perl = TRUE)
}

########################

#' @rdname significance_cpct
#' @export
add_sig_labels = function(tbl, sig_labels = LETTERS){
    header = colnames(tbl)
    groups = header_groups(header)   
    for(each_group in groups){
        if(length(each_group)>1){
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
    }
    remove_unnecessary_splitters(header)
    colnames(tbl) = header
    tbl
}


########################


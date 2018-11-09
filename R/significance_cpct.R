COMPARE_TYPE = c("subtable",
                      "first_column", "adjusted_first_column", 
                      "previous_column")

KEEP_STAT = c("percent", "cases", "means", "bases", "sd", "none")

#' Mark significant differences between columns of the table
#' 
#' \itemize{
#' \item{\code{significance_cpct}}{ conducts z-tests between column percent in
#' the result of \link{cro_cpct}. Results are calculated with the same formula 
#' as in \link[stats]{prop.test} without continuity correction.}
#' \item{\code{significance_means}}{ conducts t-tests between column means in
#' the result of \link{cro_mean_sd_n}. Results are calculated with the same formula 
#' as in \link[stats]{t.test}.}
#' \item{\code{significance_cases}}{ conducts chi-squared tests on the subtable of
#' table with counts in the result of \link{cro_cases}. Results are calculated
#' with the same formula as in \link[stats]{chisq.test} without continuity
#' correction.}} 
#' There are three type of comparisons which can be conducted simultaneously 
#' (argument \code{compare_type}). \code{subtable} provide comparison between
#' all columns inside each subtable. \code{previous_column} is comparison of
#' each column in the subtable with previous column. It is useful if columns are
#' periods or waves of survey. \code{first_column} provide comparison of table
#' first column with all other columns in the table.
#' \code{adjusted_first_column} is comparison with first column but with
#' adjustment for common base. It is useful if first column is total column and
#' other columns are subgroup of this total. Adjustments are made according to
#' algorithm in IBM SPSS Statistics Algorithms v20, p. 263. Note that with these
#' adjustments t-tests between means are made with equal variance assumed (as
#' with \code{var_equal = TRUE}).
#' By now there are no adjustments for multiple-response variables (results of 
#' \link{mrset}) in the table columns so significance tests are rather 
#' approximate for such cases.
#' Also there are functions for significance testing in the sequence of custom
#' tables calculations (see \link{tables}).
#' \itemize{
#' \item{\code{tab_last_sig_cpct}, \code{tab_last_sig_means} and 
#' \code{tab_last_sig_cpct}}{ make the same tests as there analogs mentioned 
#' above. It is recommended to use them after appropriate statistic function: 
#' \link{tab_stat_cpct}, \link{tab_stat_mean_sd_n} and \link{tab_stat_cases}.
#' }
#' \item{\code{tab_significance_options}}{ With this function we can set
#' significance options for entire custom table creation sequence.}
#' \item{\code{tab_last_add_sig_labels}}{ This function applies 
#' \code{add_sig_labels} to last calculated table - it add labels (letters by
#' default) for significance to columns header. It may be useful if you want to
#' combine table with significance with table without it.}
#' \item{\code{tab_last_round}}{ This function rounds numeric columns in the
#' last calculated table to specified number of digits. It is sometimes
#' needed if you want to combine table with significance with table without it.}
#' }
#' @param x table (class \code{etable}): result of \link{cro_cpct} with 
#'   proportions and bases for \code{significance_cpct}, result of 
#'   \link{cro_mean_sd_n} with means, standard deviations and valid N for 
#'   \code{significance_means}, and result of \link{cro_cases} with counts and
#'   bases for \code{significance_cases}.
#' @param sig_level numeric. Significance level - by default it equals to \code{0.05}.
#' @param delta_cpct numeric. Minimal delta between percent for which we mark 
#'   significant differences (in percent points) - by default it equals to zero.
#'   Note that, for example, for minimal 5 percent point difference
#'   \code{delta_cpct} should be equals 5, not 0.05.
#' @param delta_means numeric. Minimal delta between means for which we mark 
#'   significant differences  - by default it equals to zero.
#' @param min_base numeric. Significance test will be conducted if both
#'   columns have bases greater or equal to \code{min_base}. By default it equals to \code{2}.
#' @param compare_type Type of compare between columns. By default it is 
#'   \code{subtable} - comparisons will be conducted between columns of each 
#'   subtable. Other possible values are: \code{first_column}, 
#'   \code{adjusted_first_column} and \code{previous_column}. We can conduct
#'   several tests simultaneously.
#' @param bonferroni logical. \code{FALSE} by default. Should we use Bonferroni
#'   adjustment by number of comparisons in each row? 
#' @param subtable_marks character. One of "greater", "both" or "less". By
#'   deafult we mark only values which are significantly greater than some other
#'   columns. We can change this behavior by setting argument to \code{less} or
#'   \code{both}.
#' @param inequality_sign logical. FALSE if \code{subtable_marks} is "less" or 
#'   "greater". Should we show \code{>} or \code{<} before significance marks of
#'   subtable comparisons.
#' @param sig_labels character vector. Labels for marking differences between
#'   columns of subtable.
#' @param sig_labels_previous_column a character vector with two elements. Labels
#'   for marking difference with previous column. First mark means 'lower' (by
#'   default it is \code{v}) and the second means greater (\code{^}).
#' @param sig_labels_first_column a character vector with two elements. Labels 
#'   for marking difference with first column of the table. First mark means
#'   'lower' (by default it is \code{-}) and the second means 'greater'
#'   (\code{+}).
#' @param keep character. One or more from "percent", "cases", "means", "bases", 
#'   "sd" or "none". This argument determines which statistics will remain in
#'   the table after significance marking.
#' @param na_as_zero logical. \code{FALSE} by default. Should we treat
#'   \code{NA}'s as zero cases?
#' @param total_marker character. Mark of total rows in table.
#' @param total_row integer/character. In case of several totals per subtable it
#'   is number or name of total row for significance calculation.
#' @param var_equal a logical variable indicating whether to treat the two
#'   variances as being equal. For details see \link[stats]{t.test}.
#' @param digits an integer indicating how much digits after decimal separator 
#'   will be shown in the final table.
#' @param data data.frame/intermediate_table for \code{tab_*} functions.
#' @param mode character. One of \code{replace}(default) or \code{append}. In
#'   the first case the previous result in the sequence of table calculation
#'   will be replaced with result of significance testing. In the second case
#'   result of the significance testing will be appended to sequence of table
#'   calculation.
#' @param label character. Label for the statistic in the \code{tab_*}. Ignored
#'   if the \code{mode} is equals to \code{replace}.
#' @return \code{tab_last_*} functions return objects of class 
#'   \code{intermediate_table}. Use \link{tab_pivot} to get final result - 
#'   object of class \code{etable}. Other functions return object of class
#'   \code{etable} with marks of significant differences between columns.
#'   
#' @seealso \link{cro_cpct}, \link{cro_cases}, \link{cro_mean_sd_n}, \link{tables},
#'   \link{compare_proportions}, \link{compare_means}, \link[stats]{prop.test},
#'   \link[stats]{t.test}, \link[stats]{chisq.test}
#' @name significance
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       mpg = "Miles/(US) gallon",
#'                       cyl = "Number of cylinders",
#'                       disp = "Displacement (cu.in.)",
#'                       hp = "Gross horsepower",
#'                       drat = "Rear axle ratio",
#'                       wt = "Weight (lb/1000)",
#'                       qsec = "1/4 mile time",
#'                       vs = "Engine",
#'                       vs = c("V-engine" = 0,
#'                              "Straight engine" = 1),
#'                       am = "Transmission",
#'                       am = c("Automatic" = 0,
#'                              "Manual"=1),
#'                       gear = "Number of forward gears",
#'                       carb = "Number of carburetors"
#' )
#' 
#' mtcars_table = calculate(mtcars,
#'                    cro_cpct(list(cyl, gear),
#'                             list(total(), vs, am))
#'                          )
#' 
#' significance_cpct(mtcars_table)
#' 
#' # comparison with first column
#' significance_cpct(mtcars_table, compare_type = "first_column")
#' 
#' # comparison with first column and inside subtable
#' significance_cpct(mtcars_table, 
#'             compare_type = c("first_column", "subtable"))
#' 
#' # only significance marks
#' significance_cpct(mtcars_table, keep = "none")
#' 
#' # means
#' mtcars_means = calculate(mtcars,
#'                    cro_mean_sd_n(list(mpg, wt, hp),
#'                                  list(total(), vs, cyl))
#'                         )
#'                         
#' significance_means(mtcars_means) 
#' 
#' # mark values which are less and greater
#' significance_means(mtcars_means, subtable_marks = "both")
#' 
#' # chi-squared test
#' mtcars_cases = calculate(mtcars,
#'                    cro_cases(list(cyl, gear),
#'                             list(total(), vs, am))
#'                          )
#'                          
#' significance_cases(mtcars_cases)    
#' 
#' # custom tables with significance
#' mtcars %>% 
#'     tab_significance_options(subtable_marks = "both") %>% 
#'     tab_cells(mpg, hp) %>% 
#'     tab_cols(total(), vs, am) %>% 
#'     tab_stat_mean_sd_n() %>% 
#'     tab_last_sig_means(keep = "means") %>% 
#'     tab_cells(cyl, gear) %>% 
#'     tab_stat_cpct() %>% 
#'     tab_last_sig_cpct() %>% 
#'     tab_pivot()   
#'     
#' # Overcomplicated examples - we move significance marks to
#' # separate columns. Columns with statistics remain numeric    
#' mtcars %>% 
#'     tab_significance_options(keep = "none", 
#'                          sig_labels = NULL, 
#'                          subtable_marks = "both",  
#'                          mode = "append") %>% 
#'     tab_cols(total(), vs, am) %>% 
#'     tab_cells(mpg, hp) %>% 
#'     tab_stat_mean_sd_n() %>% 
#'     tab_last_sig_means() %>% 
#'     tab_last_hstack("inside_columns") %>% 
#'     tab_cells(cyl, gear) %>% 
#'     tab_stat_cpct() %>% 
#'     tab_last_sig_cpct() %>% 
#'     tab_last_hstack("inside_columns") %>% 
#'     tab_pivot(stat_position = "inside_rows") %>% 
#'     drop_empty_columns()       
significance_cpct = function(x, 
                             sig_level = 0.05, 
                             delta_cpct = 0,
                             min_base = 2,
                             compare_type ="subtable",
                             bonferroni = FALSE,
                             subtable_marks = c("greater", "both", "less"),
                             inequality_sign = "both" %in% subtable_marks,
                             sig_labels = LETTERS,
                             sig_labels_previous_column = c("v", "^"),
                             sig_labels_first_column = c("-", "+"),
                             keep = c("percent", "bases"), 
                             na_as_zero = FALSE,
                             total_marker = "#",
                             total_row = 1,
                             digits = get_expss_digits()
                             ){
    UseMethod("significance_cpct")
}


#' @export
significance_cpct.etable = function(x, 
                                    sig_level = 0.05, 
                                    delta_cpct = 0,
                                    min_base = 2,
                                    compare_type = "subtable",
                                    bonferroni = FALSE,
                                    subtable_marks = c("greater", "both", "less"),
                                    inequality_sign = "both" %in% subtable_marks,
                                    sig_labels = LETTERS,
                                    sig_labels_previous_column = c("v", "^"),
                                    sig_labels_first_column = c("-", "+"),
                                    keep = c("percent", "bases"), 
                                    na_as_zero = FALSE,
                                    total_marker = "#",
                                    total_row = 1,
                                    digits = get_expss_digits()
){
    
    
    compare_type = match.arg(compare_type, choices = COMPARE_TYPE, several.ok = TRUE)
    stopif(sum(compare_type %in% c("first_column", "adjusted_first_column"))>1, 
           "mutually exclusive compare types in significance testing:  'first_column' and 'adjusted_first_column'.")
    subtable_marks = match.arg(subtable_marks)
    mark_greater = subtable_marks %in% c("greater", "both")
    mark_less = subtable_marks %in% c("both", "less")
    delta_cpct = delta_cpct/100
    keep = match.arg(keep, KEEP_STAT, several.ok = TRUE)
    keep_percent = "percent" %in% keep
    keep_bases = "bases" %in% keep
    if(NCOL(x)>1){
        groups = header_groups(colnames(x))
        if("subtable" %in% compare_type){
            if(!is.null(sig_labels)){
                x = add_sig_labels(x, sig_labels = sig_labels)
            } 
            all_column_labels = get_category_labels(colnames(x))
        }
        sections = split_table_by_row_sections(x, total_marker = total_marker, total_row = total_row)
        res = lapply(sections, function(each_section){
            curr_base = extract_total_from_section(each_section, total_marker = total_marker, total_row = total_row)
            recode(curr_base) = lt(min_base) ~ NA
            
            total_rows_indicator = get_total_rows_indicator(each_section, total_marker = total_marker)
            empty_sig_section = each_section[!total_rows_indicator, ]
            empty_sig_section[, -1] = ""
            sig_section = empty_sig_section
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
                                                       delta_cpct = delta_cpct,
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
                                                          delta_cpct = delta_cpct,
                                                          bonferroni = bonferroni)
            }
            if("subtable" %in% compare_type){
                prepend = ""
                if(mark_greater){
                    if(inequality_sign) {
                        prepend = ">"    
                    }
                    subtable_sig_section = section_sig_prop(sig_section = empty_sig_section, 
                                                            curr_props = curr_props, 
                                                            curr_base = curr_base,
                                                            groups = groups,
                                                            all_column_labels = all_column_labels,
                                                            sig_level = sig_level,
                                                            delta_cpct = delta_cpct,
                                                            bonferroni = bonferroni,
                                                            mark_greater = TRUE,
                                                            prepend = prepend
                    )
                    for(i in seq_along(sig_section)[-1]){
                        sig_section[[i]] = paste_non_empty(sig_section[[i]], 
                                                           subtable_sig_section[[i]],
                                                           sep = " "
                        )
                    }
                }
                if(mark_less){
                    if(inequality_sign) {
                        prepend = "<"    
                    }
                    subtable_sig_section = section_sig_prop(sig_section = empty_sig_section, 
                                                            curr_props = curr_props, 
                                                            curr_base = curr_base,
                                                            groups = groups,
                                                            all_column_labels = all_column_labels,
                                                            sig_level = sig_level,
                                                            delta_cpct = delta_cpct,
                                                            bonferroni = bonferroni,
                                                            mark_greater = FALSE,
                                                            prepend = prepend
                    )
                    for(i in seq_along(sig_section)[-1]){
                        sig_section[[i]] = paste_non_empty(sig_section[[i]], 
                                                           subtable_sig_section[[i]],
                                                           sep = " "
                        )
                    }
                }
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

########################

#' @rdname significance
#' @export
add_sig_labels = function(x, sig_labels = LETTERS){
    header = colnames(x)
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
    colnames(x) = header
    x
}


########################

section_sig_prop = function(sig_section, curr_props,  curr_base, groups,
                            all_column_labels, sig_level, delta_cpct, bonferroni, 
                            mark_greater, prepend) {
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
                bonferroni_coef = number_of_comparisons_in_row # sum(number_of_comparisons_in_row, na.rm = TRUE)
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
                    if(mark_greater) {
                        comparison = prop1>prop2
                    } else {
                        comparison = prop2>prop1
                    }    
                    delta =  abs(prop1 - prop2)
                    sig_section[[col1]] = ifelse(comparison & pval<sig_level & delta>delta_cpct,
                                                 paste_non_empty(sig_section[[col1]],
                                                                 all_column_labels[[col2]],
                                                                 sep = " "),
                                                 sig_section[[col1]]
                    )
                    sig_section[[col2]] = ifelse(!comparison & pval<sig_level & delta>delta_cpct,
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

section_sig_previous_column = function(sig_section, curr_props,  curr_base, groups,
                                       sig_labels_previous_column, 
                                       sig_level, delta_cpct, bonferroni) {
    for(each_group in groups){
        if(length(each_group)>1){
            # col1 - current column
            # col2 - previous column
            if(bonferroni) {
                invalid_columns = is.na(curr_base[each_group])
                comparable_values = !is.na(curr_props[,each_group, drop = FALSE])
                comparable_values[,invalid_columns] = FALSE
                # count number of comaprisons
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
                prop1 = curr_props[[col1]]
                base1 = curr_base[[col1]]
                prop2 = curr_props[[col2]]
                base2 = curr_base[[col2]]
                pval = compare_proportions(prop1, prop2, 
                                   base1, base2)
                if_na(pval) = 1
                pval = pmin(pval*bonferroni_coef, 1)
                sig_section[[col1]] = ifelse(pval<sig_level & abs(prop1 - prop2)>delta_cpct,
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
                                    sig_labels_first_column, sig_level,
                                    delta_cpct, bonferroni,
                                    adjust_common_base = FALSE) {
    groups = unlist(groups)
    # col1 - first column
    # col2 - other columns
    col1 = groups[1]
    prop1 = curr_props[[col1]]
    base1 = curr_base[[col1]]
    if(length(groups)>1 & !is.na(base1)){
        if(bonferroni) {
            invalid_columns = is.na(curr_base[-1])
            comparable_values = !is.na(curr_props[,-1, drop = FALSE])
            comparable_values[,invalid_columns] = FALSE
            # count number of comaprisons
            valid_values_in_row = rowSums(comparable_values, na.rm = TRUE)
            number_of_comparisons_in_row = valid_values_in_row
            number_of_comparisons_in_row[number_of_comparisons_in_row<0] = 0
            bonferroni_coef = number_of_comparisons_in_row #sum(number_of_comparisons_in_row, na.rm = TRUE)
            bonferroni_coef[bonferroni_coef==0] = 1
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
            pval = pmin(pval*bonferroni_coef, 1)
            sig_section[[col2]] = ifelse(pval<sig_level & abs(prop1 - prop2)>delta_cpct,
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
    # "aaa|bbb|ddd" -> "ddd"
    header = gsub("\\n|\\r", " ", header, perl = TRUE)
    gsub("^.*?\\|([^\\|]*)$", "\\1", header, perl = TRUE) 
}


########################

header_groups = function(header){
    header = header[-1]
    potentially_subgroup = function(x){
        has_levels = grepl("|", x, fixed = TRUE)
        seq_dupl = x[-1L] == x[-length(x)]
        seq_dupl =c(FALSE, seq_dupl) | c(seq_dupl, FALSE)
        has_levels & !seq_dupl
    }
    if(!any(grepl("|", header, fixed = TRUE))){
        # '+ 1' because of first column with row_labels
        return(list(seq_along(header)+1))
    }
    header = gsub("\\n|\\r", " ", header, perl = TRUE)
    to_strip = potentially_subgroup(header) 
    while(any(to_strip)){
        # "aaa|bbb|ddd" -> "aaa|bbb"
        header[to_strip] = gsub("\\|[^\\|]*$", "", header[to_strip], perl = TRUE) 
        to_strip = potentially_subgroup(header) 
    }
    res = rle(header)[["lengths"]]
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
           "significance testing - total rows not found. Incorrect total marker: ","'",
           total_marker, "'")
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
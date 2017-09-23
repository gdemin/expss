TOTAL_STATISTICS = c("u_cases", "w_cases", "u_responses", "w_responses", "u_cpct", "w_cpct",
                     "u_rpct", "w_rpct", "u_tpct", "w_tpct")

#' Cross tabulation with support of labels, weights and multiple response variables.
#' 
#' \itemize{
#' \item{\code{cro}, \code{cro_cases}}{ build a contingency table of the counts.}
#' \item{\code{cro_cpct}, \code{cro_cpct_responses}}{ build a contingency table 
#' of the column percent. These functions give different results only for 
#' multiple response variables. For \code{cro_cpct} base of percent is number 
#' of valid cases. Case is considered as valid if it has at least one non-NA 
#' value. So for multiple response variables sum of percent may be greater than
#' 100. For \code{cro_cpct_responses} base of percent is number of valid 
#' responses. Multiple response variables can have several responses for single 
#' case. Sum of percent of \code{cro_cpct_responses} always equals to 100\%.}
#' \item{\code{cro_rpct}}{ build a contingency table of the row percent. Base
#' for percent is number of valid cases.}
#' \item{\code{cro_tpct}}{ build a contingency table of the table percent. Base
#' for percent is number of valid cases.}
#' \item{\code{total}}{ auxiliary function - creates variables with 1 for valid
#' case of its argument \code{x} and NA in opposite case.}
#' }
#' You can combine tables with \link{add_rows} and \link{merge.etable}. For
#' sorting table see \link{tab_sort_asc}. 
#' To provide multiple-response variables as arguments use \link{mrset} for 
#' multiples with category encoding and \link{mdset} for multiples with 
#' dichotomy (dummy) encoding. To compute statistics with nested 
#' variables/banners use \link{nest}. For more sophisticated interface with
#' modern piping via \code{magrittr} see \link{tables}.
#' 
#' @param cell_vars vector/data.frame/list. Variables on which percentage/cases
#'   will be computed. Use \link{mrset}/\link{mdset} for multiple-response
#'   variables.
#' @param col_vars vector/data.frame/list. Variables which breaks table by
#'   columns. Use \link{mrset}/\link{mdset} for multiple-response variables.
#' @param row_vars vector/data.frame/list. Variables which breaks table by rows.
#'   Use \link{mrset}/\link{mdset} for multiple-response variables.
#' @param weight numeric vector. Optional cases weights. Cases with NA's,
#'   negative and zero weights are removed before calculations.
#' @param subgroup logical vector. You can specify subgroup on which table will be computed. 
#' @param total_label By default "#Total". You can provide several names - each name for
#'   each total statistics.
#' @param total_statistic  By default it is "u_cases" (unweighted cases). 
#'   Possible values are "u_cases", "u_responses", "u_cpct", "u_rpct", "u_tpct",
#'   "w_cases", "w_responses", "w_cpct", "w_rpct", "w_tpct". "u_" means
#'   unweighted statistics and "w_" means weighted statistics.
#' @param total_row_position Position of total row in the resulting table. Can
#'   be one of "below", "above", "none".
#' @param x vector/data.frame of class 'category'/'dichotomy'. 
#' @param label character. Label for total variable. 
#'   
#' @return object of class 'etable'. Basically it's a data.frame but class
#'   is needed for custom methods.
#' @seealso \link{tables}, \link{fre}, \link{cro_fun}.
#'
#' @examples
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       mpg = "Miles/(US) gallon",
#'                       cyl = "Number of cylinders",
#'                       disp = "Displacement (cu.in.)",
#'                       hp = "Gross horsepower",
#'                       drat = "Rear axle ratio",
#'                       wt = "Weight (1000 lbs)",
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
#' calculate(mtcars, cro(am, vs))
#' 
#' # column percent with multiple banners
#' calculate(mtcars, cro_cpct(cyl, list(total(), vs, am)))
#' 
#' # nested banner
#' calculate(mtcars, cro_cpct(cyl, list(total(), vs %nest% am)))
#' 
#' # stacked variables
#' calculate(mtcars, cro(list(cyl, carb), list(total(), vs %nest% am)))
#' 
#' # nested variables
#' calculate(mtcars, cro_cpct(am %nest% cyl, list(total(), vs)))
#'
#' # row variables
#' calculate(mtcars, cro_cpct(cyl, list(total(), vs), row_vars = am))
#' 
#' # several totals above table
#' calculate(mtcars, cro_cpct(cyl, 
#'               list(total(), vs), 
#'               row_vars = am,
#'               total_row_position = "above",
#'               total_label = c("number of cases", "row %"),
#'               total_statistic = c("u_cases", "u_rpct")
#'               ))
#' 
#' # multiple-choice variable
#' # brands - multiple response question
#' # Which brands do you use during last three months? 
#' set.seed(123)
#' brands = data.frame(t(replicate(20,sample(c(1:5,NA),4,replace = FALSE))))
#' # score - evaluation of tested product
#' score = sample(-1:1,20,replace = TRUE)
#' var_lab(brands) = "Used brands"
#' val_lab(brands) = make_labels("
#'                               1 Brand A
#'                               2 Brand B
#'                               3 Brand C
#'                               4 Brand D
#'                               5 Brand E
#'                               ")
#' 
#' var_lab(score) = "Evaluation of tested brand"
#' val_lab(score) = num_lab("
#'                              -1 Dislike it
#'                              0 So-so
#'                              1 Like it    
#'                              ")
#'
#' cro_cpct(mrset(brands), list(total(), score))
#' # responses
#' cro_cpct_responses(mrset(brands), list(total(), score))
#' @export
cro = function(cell_vars, 
               col_vars = total(), 
               row_vars = total(label = ""),
               weight = NULL,
               subgroup = NULL,
               total_label = NULL,
               total_statistic = "u_cases",
               total_row_position = c("below", "above", "none")
){
    
    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    cell_vars = test_for_null_and_make_list(cell_vars, str_cell_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    
    multi_cro(cell_vars = cell_vars, 
              col_vars = col_vars, 
              row_vars = row_vars,
              weight = weight,
              subgroup = subgroup,
              total_label = total_label,
              total_statistic = total_statistic,
              total_row_position = total_row_position,
              stat_type = "cases"
    )    
}


#' @export
#' @rdname cro
cro_cases = cro

#' @export
#' @rdname cro
cro_cpct = function(cell_vars, 
                    col_vars = total(), 
                    row_vars = total(label = ""),
                    weight = NULL,
                    subgroup = NULL,
                    total_label = NULL,
                    total_statistic = "u_cases",
                    total_row_position = c("below", "above", "none")
){
    
    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    cell_vars = test_for_null_and_make_list(cell_vars, str_cell_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    
    multi_cro(cell_vars = cell_vars, 
              col_vars = col_vars, 
              row_vars = row_vars,
              weight = weight,
              subgroup = subgroup,
              total_label = total_label,
              total_statistic = total_statistic,
              total_row_position = total_row_position,
              stat_type = "cpct"
    )    
}

#' @export
#' @rdname cro
cro_rpct = function(cell_vars, 
                    col_vars = total(), 
                    row_vars = total(label = ""),
                    weight = NULL,
                    subgroup = NULL,
                    total_label = NULL,
                    total_statistic = "u_cases",
                    total_row_position = c("below", "above", "none")
){
    
    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    cell_vars = test_for_null_and_make_list(cell_vars, str_cell_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    
    multi_cro(cell_vars = cell_vars, 
              col_vars = col_vars, 
              row_vars = row_vars,
              weight = weight,
              subgroup = subgroup,
              total_label = total_label,
              total_statistic = total_statistic,
              total_row_position = total_row_position,
              stat_type = "rpct"
    )    
}


#' @export
#' @rdname cro
cro_tpct = function(cell_vars, 
                    col_vars = total(), 
                    row_vars = total(label = ""),
                    weight = NULL,
                    subgroup = NULL,
                    total_label = NULL,
                    total_statistic = "u_cases",
                    total_row_position = c("below", "above", "none")
){
    
    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    cell_vars = test_for_null_and_make_list(cell_vars, str_cell_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    
    multi_cro(cell_vars = cell_vars, 
              col_vars = col_vars, 
              row_vars = row_vars,
              weight = weight,
              subgroup = subgroup,
              total_label = total_label,
              total_statistic = total_statistic,
              total_row_position = total_row_position,
              stat_type = "tpct"
    )    
}

#' @export
#' @rdname cro
cro_cpct_responses = function(cell_vars, 
                              col_vars = total(), 
                              row_vars = total(label = ""),
                              weight = NULL,
                              subgroup = NULL,
                              total_label = NULL,
                              total_statistic = "u_responses",
                              total_row_position = c("below", "above", "none")
){
    
    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    cell_vars = test_for_null_and_make_list(cell_vars, str_cell_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    
    multi_cro(cell_vars = cell_vars, 
              col_vars = col_vars, 
              row_vars = row_vars,
              weight = weight,
              subgroup = subgroup,
              total_label = total_label,
              total_statistic = total_statistic,
              total_row_position = total_row_position,
              stat_type = "cpct_responses"
    )    
}


### compute statistics for single cell_var and single col_var
elementary_cro = function(cell_var, 
                          col_var, 
                          row_var, 
                          weight = NULL, 
                          subgroup,
                          total_label,
                          total_statistic,
                          total_row_position = c("below", "above", "none"),
                          stat_type = c("cases", "cpct", "cpct_responses", "rpct", "tpct")
){
    # to pass CRAN check
    row_labels = NULL
    ### preparations
    total_row_position = match.arg(total_row_position)
    unknowns = total_statistic %d% TOTAL_STATISTICS
    stopif(length(unknowns)>0, "unknown total statistics - ", 
           paste(unknowns, collapse = ", "))
    total_statistic = match.arg(total_statistic, TOTAL_STATISTICS, several.ok = TRUE)
    stat_type = match.arg(stat_type)
    max_nrow = max(NROW(cell_var), NROW(col_var), NROW(row_var))
    
    cell_var_lab = var_lab(cell_var)
    cell_val_lab = val_lab(cell_var)
    
    row_var_lab = var_lab(row_var)
    row_val_lab = val_lab(row_var)
    
    col_var_lab = var_lab(col_var)
    col_val_lab = val_lab(col_var)
    
    cell_var = unlab(cell_var)
    row_var = unlab(row_var)
    col_var = unlab(col_var)
    
    
    weight = unlab(if_null(weight, 1))
    weight = set_negative_and_na_to_zero(weight)
    weight = recycle_if_single_row(weight, max_nrow)
    
    valid = valid(cell_var) & valid(col_var) & valid(row_var) & (weight>0) & if_null(subgroup, TRUE)
    
    col_var = recycle_if_single_row(col_var, max_nrow)
    row_var = recycle_if_single_row(row_var, max_nrow)
    cell_var = recycle_if_single_row(cell_var, max_nrow)
    
    if(!all(valid, na.rm = TRUE) || length(valid)==0){
        weight = universal_subset(weight, valid)
        col_var = universal_subset(col_var, valid)
        row_var = universal_subset(row_var, valid)
        cell_var = universal_subset(cell_var, valid) 
    }
    
    raw_data = data.table(cell_var, 
                     col_var,
                     weight,
                     row_var)
    cell_var_names = paste0("cells", seq_len(NCOL(cell_var)))
    col_var_names = paste0("cols", seq_len(NCOL(col_var)))
    setnames(raw_data, c(cell_var_names, col_var_names, "weight", "row_var"))
    
    # statistics
    
    dtable = switch(stat_type,
                    cases = internal_cases(raw_data,
                                           col_var_names = col_var_names,
                                           cell_var_names = cell_var_names,
                                           use_weight = TRUE),
                    cpct = internal_cpct(raw_data,
                                         col_var_names = col_var_names,
                                         cell_var_names = cell_var_names,
                                         use_weight = TRUE),
                    cpct_responses = internal_cpct_responses(raw_data,
                                                             col_var_names = col_var_names,
                                                             cell_var_names = cell_var_names,
                                                             use_weight = TRUE),
                    rpct = internal_rpct(raw_data,
                                         col_var_names = col_var_names,
                                         cell_var_names = cell_var_names,
                                         use_weight = TRUE),
                    tpct = internal_tpct(raw_data,
                                         col_var_names = col_var_names,
                                         cell_var_names = cell_var_names,
                                         use_weight = TRUE)
    )
    
    dtable[, cell_var := set_var_lab(cell_var, cell_var_lab)]
    dtable[, cell_var := set_val_lab(cell_var, cell_val_lab)]
    dtable[, col_var := set_var_lab(col_var, col_var_lab)]
    dtable[, col_var := set_val_lab(col_var, col_val_lab)]
    dtable[, row_var := set_var_lab(row_var, row_var_lab)]
    dtable[, row_var := set_val_lab(row_var, row_val_lab)]
    ### make rectangular table  
    res = long_datatable_to_table(dtable, rows = c("row_var", "cell_var"), 
                                  columns = "col_var", values = "value")
    
    
    colnames(res)[2] = "row_labels"
    
    if(total_row_position!="none"){
        total_rows = make_total_rows(
            raw_data = raw_data,
            col_var_names = col_var_names,
            total_statistic = total_statistic,
            total_label = total_label
        )   
        total_rows[, col_var := set_var_lab(col_var, col_var_lab)]
        total_rows[, col_var := set_val_lab(col_var, col_val_lab)]
        total_rows[, row_var := set_var_lab(row_var, row_var_lab)]
        total_rows[, row_var := set_val_lab(row_var, row_val_lab)]
        # total_rows[, row_labels := factor(row_labels, levels = unique(row_labels))]
        total_rows = long_datatable_to_table(total_rows, 
                                      rows = c("row_var", "row_labels"), 
                                      columns = "col_var", 
                                      values = "value")
        # total_rows = total_rows[, -"item"]
        res = add_total_to_table(res, total_rows = total_rows, total_row_position = total_row_position)
    }    
    
    
    res[, row_labels := paste0(row_var_lab, "|", row_var, "|", cell_var_lab, "|", row_labels)]
    res[, row_var:=NULL ]
    setnames(res, c(colnames(res)[1] , paste0(col_var_lab, "|", colnames(res)[-1])))
    
    res[ , row_labels := remove_unnecessary_splitters(row_labels)] 
    res[ , row_labels := make_items_unique(row_labels)] 
    setnames(res, remove_unnecessary_splitters(colnames(res)))
    res = as.dtfrm(res)
    class(res) = union("etable", class(res))
    res
}

########################
internal_cases = function(raw_data, col_var_names, cell_var_names = NULL, use_weight){
    # to pass CRAN check
    weight = NULL
    value = NULL
    col_var = NULL
    cell_var = NULL
    row_var = NULL
    # columns = c("row_var", col_var_names, cell_var_names)
    # all_attr = lapply(columns, 
    #                   function(curr) var_attr(raw_data[[curr]]))
    # for(each in columns)
    if(is.null(cell_var_names)){
        res = lapply(col_var_names, function(each_col){
            by_str = paste0("row_var,", each_col)
            if(use_weight){
                dres = raw_data[, list(value = sum(weight, na.rm = TRUE)), by = by_str] 
            } else {
                dres = raw_data[, list(value = .N), by = by_str]                 
            }
        })
        res = rbindlist(res, use.names = FALSE, fill = FALSE)
        setnames(res, c("row_var", "col_var", "value"))
        res = res[, list(value = sum(value, na.rm = TRUE)), by = "row_var,col_var"]
        res = res[!is.na(col_var), ]
    } else {
        
        res = lapply(cell_var_names, function(each_cell) { 
            res = lapply(col_var_names, function(each_col){
                by_str = paste0("row_var,",each_cell, ",", each_col)
                if(use_weight){
                    dres = raw_data[, list(value = sum(weight, na.rm = TRUE)), 
                                    by = by_str] 
                } else {
                    dres = raw_data[, list(value = .N), 
                                    by = by_str]                 
                }
            
                
            })
            res = rbindlist(res, use.names = FALSE, fill = FALSE)
        })
        res = rbindlist(res, use.names = FALSE, fill = FALSE)
        setnames(res, c("row_var", "cell_var", "col_var", "value"))
        res = res[, list(value = sum(value, na.rm = TRUE)), by = "row_var,col_var,cell_var"]
        res = res[!is.na(col_var) & !is.na(cell_var), ]
    }
    res[, value := as.double(value)]
    res
}

######################
internal_cpct = function(raw_data, col_var_names, cell_var_names = NULL, use_weight){
    # to pass CRAN check
    value = NULL
    dtable = internal_cases(raw_data, 
                            col_var_names = col_var_names,
                            cell_var_names = cell_var_names,
                            use_weight = use_weight)
    dtotal = internal_cases(raw_data, 
                            col_var_names = col_var_names,
                            use_weight = use_weight)
    
    setnames(dtotal, "value", "total")
    dtable = dtotal[dtable, on = c("row_var","col_var"), nomatch = NA]
    dtable[, value := value/total*100]    
}

########################
internal_cpct_responses = function(raw_data, col_var_names, cell_var_names = NULL, use_weight){
    # to pass CRAN check
    value = NULL
    dtable = internal_cases(raw_data, 
                            col_var_names = col_var_names,
                            cell_var_names = cell_var_names,
                            use_weight = use_weight)
    dtable[, value := value/sum(value, na.rm = TRUE)*100, by = "row_var,col_var"]    
}

#######################
internal_rpct = function(raw_data, col_var_names, cell_var_names = NULL, use_weight){
    # to pass CRAN check
    weight = NULL
    value = NULL
    dtable = internal_cases(raw_data, 
                            col_var_names = col_var_names,
                            cell_var_names = cell_var_names,
                            use_weight = use_weight)
    if(is.null(cell_var_names)){
        if(use_weight){
            row_total = raw_data[, list(total = as.double(sum(weight, na.rm = TRUE))), by = "row_var" ]
            
        } else {
            row_total = raw_data[, list(total = as.double(.N)), by = "row_var" ]
        } 
        dtable = row_total[dtable, on = "row_var", nomatch = NA]
    } else {
        row_total = internal_cases(raw_data, col_var_names = cell_var_names,
                                   use_weight = use_weight)
        setnames(row_total, "col_var", "cell_var")
        setnames(row_total, "value", "total")
        dtable = row_total[dtable, on = c("row_var","cell_var"), nomatch = NA]
    }
    
    
    dtable[, value := value/total*100]    
}

#######################
internal_tpct = function(raw_data, col_var_names, cell_var_names = NULL, use_weight){
    # to pass CRAN check
    weight = NULL
    value = NULL

    dtable = internal_cases(raw_data, 
                            col_var_names = col_var_names,
                            cell_var_names = cell_var_names,
                            use_weight = use_weight)
    if(use_weight){
        dtotal = raw_data[, list(total = as.double(sum(weight, na.rm = TRUE))), by = "row_var" ]
        
    } else {
        dtotal = raw_data[, list(total = as.double(.N)), by = "row_var" ]
    }
    dtable = dtotal[dtable, on = "row_var", nomatch = NA]
    dtable[, value := value/total*100]  
}



#################################
########################

# used only for total 
internal_responses = function(raw_data, col_var_names, use_weight){
    # to pass CRAN check
    value = NULL
    cell_var_names = setdiff(colnames(raw_data), c(col_var_names, "weight", "row_var"))
    res = internal_cases(raw_data, 
                         col_var_names = col_var_names,
                         cell_var_names = cell_var_names,
                         use_weight = use_weight
    )
    res[, list(value = sum(value, na.rm = TRUE)), by = "row_var,col_var"]
}

###########################

make_total_rows = function(raw_data, col_var_names, 
                           total_statistic, total_label){
    # to pass CRAN check
    row_labels = NULL
    total_statistic_label = gsub("^u_", " ", total_statistic, perl = TRUE)
    total_statistic_label = gsub("^w_", " wtd. ", total_statistic_label, perl = TRUE)
    # total_statistic_label = paste0(" ", total_statistic_label, ")")
    if(is.null(total_label)){
        total_label = paste0("#Total", total_statistic_label)    
    } else {
        if(length(total_label) < length(total_statistic)) {
            total_label = paste0(total_label, total_statistic_label)
        }    
    }
    total_label = make_items_unique(total_label)
    for(item in seq_along(total_label)){
        total_label[item] = add_first_symbol_to_total_label(total_label[item])
    }
    total_row = lapply(seq_along(total_statistic), function(item){
        curr_statistic = total_statistic[[item]]
        use_weight = substr(curr_statistic, 1,2) == "w_"
        curr_statistic = gsub("^(u|w)_", "", curr_statistic, perl = TRUE)
        dtotal = switch(curr_statistic, 
                        cases = internal_cases(raw_data = raw_data, 
                                               col_var_names = col_var_names, 
                                               use_weight = use_weight),
                        responses = internal_responses(raw_data = raw_data, 
                                                       col_var_names = col_var_names, 
                                                       use_weight = use_weight),
                        cpct = internal_cpct(raw_data = raw_data, 
                                             col_var_names = col_var_names, 
                                             use_weight = use_weight),
                        rpct = internal_rpct(raw_data = raw_data, 
                                             col_var_names = col_var_names, 
                                             use_weight = use_weight),
                        tpct = internal_tpct(raw_data = raw_data, 
                                             col_var_names = col_var_names, 
                                             use_weight = use_weight)
        )
        #dtotal[, item := item]
        if("total" %in% colnames(dtotal)){
            dtotal[ , total:=NULL]
        }
        curr_lab = total_label[item]
        dtotal[, row_labels := curr_lab] 
        dtotal
    })
    total_row = rbindlist(total_row, fill = FALSE, use.names = FALSE)
    labs = setNames(seq_along(total_label), total_label)
    total_row[ ,row_labels := factor(row_labels, levels = total_label)] 
    total_row
}

add_total_to_table = function(res, total_rows, total_row_position){
    # to pass CRAN check
    ..index__  = NULL
    # we need total inside each group of row_var
    res[ , ..index__ :=2]
    if(total_row_position=="above"){
        total_rows[, ..index__ := 1] 
    } else {
        total_rows[, ..index__ := 3]
    }
    res = rbind(res, total_rows, fill = TRUE, use.names = TRUE)
    setkeyv(res, c("row_var", "..index__"), verbose = FALSE)
    res[, ..index__:=NULL]
    res
}

#################################

multi_cro = function(cell_vars, 
                     col_vars,
                     row_vars,
                     weight, 
                     subgroup,
                     total_label,
                     total_statistic,
                     total_row_position = c("below", "above", "none"),
                     stat_type){
    cell_vars = flat_list(dichotomy_to_category_encoding(cell_vars), flat_df = FALSE)
    col_vars = flat_list(dichotomy_to_category_encoding(col_vars), flat_df = FALSE)
    row_vars = flat_list(multiples_to_single_columns_with_dummy_encoding(row_vars), flat_df = TRUE)
    stopif(!is.null(subgroup) && !is.logical(subgroup), "'subgroup' should be logical.")
    check_sizes("'cro'", cell_vars, col_vars, weight, subgroup)
    res = lapply(row_vars, function(each_row_var) {
        res = lapply(cell_vars, function(each_cell_var){
            all_col_vars = lapply(col_vars, function(each_col_var){
                elementary_cro(cell_var = each_cell_var, 
                               col_var = each_col_var, 
                               row_var = each_row_var,
                               weight = weight,
                               subgroup = subgroup,
                               total_label = total_label,
                               total_statistic = total_statistic,
                               total_row_position = total_row_position,
                               stat_type = stat_type
                )    
            })
            Reduce(merge, all_col_vars)
            
        })
        res = do.call(add_rows, res)
    })
    res = do.call(add_rows, res)
    rownames(res) = NULL
    res
}

######################################################


#' @export
#' @rdname cro
total = function(x = 1, label = "#Total"){
    UseMethod("total")
}    

#' @export
total.default = function(x = 1, label = "#Total"){
    res = valid(x)
    res[!res] = NA
    res = as.numeric(res)
    varlab = var_lab(x) 
    if(!is.null(varlab) && varlab!=""){
        label = paste0(varlab, "|", label)
    }
    var_lab(res) = ""
    val_lab(res) = setNames(1, label)
    res
}

#' @export
total.dichotomy = function(x = 1, label = "#Total"){
    res = valid(x)
    res[!res] = NA
    res = as.numeric(res)
    vallab = unlist(lapply(seq_along(x), function(i){
        varlab = var_lab(x[[i]])
        if(!is.null(varlab) && varlab!=""){
            varlab
        } else {
            colnames(x)[i]
        }
    }))
    if(length(vallab)>0){
        varlab = common_label(vallab)
    }
    if(length(varlab)>0 && varlab!=""){
        label = paste0(varlab, "|", label)
    }
    var_lab(res) = ""
    val_lab(res) = setNames(1, label)
    res
}


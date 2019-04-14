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
#' \item{\code{calc_cro_*}}{ are the same as above but evaluate their arguments
#' in the context of the first argument \code{data}.}
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
#' @param data data.frame in which context all other arguments will be evaluated
#'   (for \code{calc_cro_*}).
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
#' calc_cro(mtcars, am, vs) # the same result
#' 
#' # column percent with multiple banners
#' calculate(mtcars, cro_cpct(cyl, list(total(), vs, am)))
#' calc_cro_cpct(mtcars, cyl, list(total(), vs, am)) # the same result
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
               row_vars = NULL,
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
    if(!is.null(row_vars)) row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    
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
                    row_vars = NULL,
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
    if(!is.null(row_vars)) row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    
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
                    row_vars = NULL,
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
    if(!is.null(row_vars)) row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    
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
                    row_vars = NULL,
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
    if(!is.null(row_vars)) row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    
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
                              row_vars = NULL,
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
    if(!is.null(row_vars)) row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    
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

#########################################

#' @export
#' @rdname cro
calc_cro = function(data,
                    cell_vars, 
                    col_vars = total(), 
                    row_vars = NULL,
                    weight = NULL,
                    subgroup = NULL,
                    total_label = NULL,
                    total_statistic = "u_cases",
                    total_row_position = c("below", "above", "none")
){
    expr = substitute(cro_cases(cell_vars = cell_vars, 
                                col_vars = col_vars, 
                                row_vars = row_vars,
                                weight = weight,
                                subgroup = subgroup,
                                total_label = total_label,
                                total_statistic = total_statistic,
                                total_row_position = total_row_position)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}

#' @export
#' @rdname cro
calc_cro_cases = calc_cro

########################
#' @export
#' @rdname cro
calc_cro_cpct = function(data,
                    cell_vars, 
                    col_vars = total(), 
                    row_vars = NULL,
                    weight = NULL,
                    subgroup = NULL,
                    total_label = NULL,
                    total_statistic = "u_cases",
                    total_row_position = c("below", "above", "none")
){
    expr = substitute(cro_cpct(cell_vars = cell_vars, 
                                col_vars = col_vars, 
                                row_vars = row_vars,
                                weight = weight,
                                subgroup = subgroup,
                                total_label = total_label,
                                total_statistic = total_statistic,
                                total_row_position = total_row_position)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}

#' @export
#' @rdname cro
calc_cro_rpct = function(data,
                         cell_vars, 
                         col_vars = total(), 
                         row_vars = NULL,
                         weight = NULL,
                         subgroup = NULL,
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none")
){
    expr = substitute(cro_rpct(cell_vars = cell_vars, 
                               col_vars = col_vars, 
                               row_vars = row_vars,
                               weight = weight,
                               subgroup = subgroup,
                               total_label = total_label,
                               total_statistic = total_statistic,
                               total_row_position = total_row_position)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}

#' @export
#' @rdname cro
calc_cro_tpct = function(data,
                         cell_vars, 
                         col_vars = total(), 
                         row_vars = NULL,
                         weight = NULL,
                         subgroup = NULL,
                         total_label = NULL,
                         total_statistic = "u_cases",
                         total_row_position = c("below", "above", "none")
){
    expr = substitute(cro_tpct(cell_vars = cell_vars, 
                               col_vars = col_vars, 
                               row_vars = row_vars,
                               weight = weight,
                               subgroup = subgroup,
                               total_label = total_label,
                               total_statistic = total_statistic,
                               total_row_position = total_row_position)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}


#' @export
#' @rdname cro
calc_cro_cpct_responses = function(data,
                         cell_vars, 
                         col_vars = total(), 
                         row_vars = NULL,
                         weight = NULL,
                         subgroup = NULL,
                         total_label = NULL,
                         total_statistic = "u_responses",
                         total_row_position = c("below", "above", "none")
){
    expr = substitute(cro_cpct_responses(cell_vars = cell_vars, 
                                         col_vars = col_vars, 
                                         row_vars = row_vars,
                                         weight = weight,
                                         subgroup = subgroup,
                                         total_label = total_label,
                                         total_statistic = total_statistic,
                                         total_row_position = total_row_position)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}

########################
make_datatable_for_cro = function(cell_var, 
                                  col_var, 
                                  row_var, 
                                  weight, 
                                  subgroup){
    
    max_nrow = max(NROW(cell_var), NROW(col_var), NROW(row_var), NROW(weight))
    if(is.null(subgroup)){
        non_empty_rows = valid(cell_var) & valid(col_var) 
    } else {
        non_empty_rows = valid(cell_var) & valid(col_var)  & subgroup & !is.na(subgroup)
    }
    
    col_var = recycle_if_single_row(col_var, max_nrow)
    cell_var = recycle_if_single_row(cell_var, max_nrow)
    
    cell_var = unlab(cell_var)
    col_var = unlab(col_var)
    
    use_row_var = !is.null(row_var)
    
    if(use_row_var){
        non_empty_rows = valid(row_var) & non_empty_rows
        row_var = recycle_if_single_row(row_var, max_nrow)
        row_var = unlab(row_var)
    }
    
    
    use_weight = !is.null(weight)
    if(use_weight) {
        weight = unlab(weight)
        weight = set_negative_and_na_to_zero(weight)
        weight = recycle_if_single_row(weight, max_nrow)
        non_empty_rows = non_empty_rows & (weight>0)
    }
    
    
    if(!all(non_empty_rows, na.rm = TRUE) || length(non_empty_rows)==0){
        if(use_weight) weight = universal_subset(weight, non_empty_rows)
        col_var = universal_subset(col_var, non_empty_rows)
        cell_var = universal_subset(cell_var, non_empty_rows) 
        if(use_row_var) row_var = universal_subset(row_var, non_empty_rows)
    }
    
    cell_var_names = paste0("cells", seq_len(NCOL(cell_var)))
    col_var_names = paste0("cols", seq_len(NCOL(col_var)))
    if(use_weight){
        if(use_row_var){
            raw_data = data.table(cell_var, 
                                  col_var,
                                  weight,
                                  row_var)
            
            setnames(raw_data, c(cell_var_names, col_var_names, "weight", "row_var"))
        } else {
            raw_data = data.table(cell_var, 
                                  col_var,
                                  weight)
            
            setnames(raw_data, c(cell_var_names, col_var_names, "weight"))    
        }
    } else {
        if(use_row_var){
            raw_data = data.table(cell_var, 
                                  col_var,
                                  row_var)
            
            setnames(raw_data, c(cell_var_names, col_var_names, "row_var"))    
        } else {
            raw_data = data.table(cell_var, 
                                  col_var)
            
            setnames(raw_data, c(cell_var_names, col_var_names))     
            
        }
    }
    raw_data
}

extract_cell_names = function(raw_data){
    grep("cell", colnames(raw_data),  value = TRUE, fixed = TRUE)
}

extract_col_names = function(raw_data){
    grep("col", colnames(raw_data), value = TRUE, fixed = TRUE)
}

has_row_var = function(raw_data){
    "row_var" %in% colnames(raw_data)
}

has_weight = function(raw_data){
    "weight" %in% colnames(raw_data)
}

### compute statistics for single cell_var and single col_var
### by now it is absolutely awfull code which likes spaghetti
### it is consequency of optimiztion which lead to four time performance 
### improvement in some frequent cases
### need to rewrite
elementary_cro = function(cell_var, 
                          col_var, 
                          row_var, 
                          weight, 
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
   
    cell_var_lab = var_lab(cell_var)
    cell_val_lab = val_lab(cell_var)
    
    col_var_lab = var_lab(col_var)
    col_val_lab = val_lab(col_var)
    
    row_var_lab = var_lab(row_var)
    row_val_lab = val_lab(row_var)
    
    raw_data = make_datatable_for_cro(cell_var = cell_var,
                                      col_var = col_var,
                                      row_var = row_var,
                                      weight = weight,
                                      subgroup = subgroup
                                      )
    col_var_names = extract_col_names(raw_data)
    cell_var_names = extract_cell_names(raw_data)
    use_weight = has_weight(raw_data)
    use_row_var = has_row_var(raw_data)
    ##### cases
    
    cases = internal_cases(raw_data,
                           col_var_names = col_var_names,
                           cell_var_names = cell_var_names,
                           use_weight = use_weight)
    
    ###### margins ########
    # stat_type == "cpct" || ("w_cases", "u_cases", "w_tpct", "u_tpct", "w_rpct", "u_rpct")
    column_margin  = calculate_column_margin(raw_data, 
                                             cases,
                                             stat_type,
                                             total_statistic, 
                                             use_weight)
    
    ###########################
    ## stat_type = "rpct"
    row_margin  = calculate_row_margin(raw_data, 
                                   cases,
                                   stat_type,
                                   total_statistic, 
                                   use_weight)
    
    ### margin_responses ####
    # w_responses | u_responses

    response_column_margin = calculate_response_column_margin(raw_data, 
                                                              cases,
                                                              stat_type,
                                                              total_statistic, 
                                                              use_weight)
    
    ##############
    ### stat_type == "tpct" ||  "w_tpct", "u_tpct", "w_rpct", "u_rpct"
    total_margin = calculate_total_margin(raw_data, 
                           cases,
                           stat_type,
                           total_statistic, 
                           use_weight)

    #######
    
    if(stat_type == "cases"){
        dtable = cases
    } else {
        dtable = calculate_percent(cases, 
                                   margin = switch(stat_type,
                                                   cpct = column_margin[["w"]],
                                                   tpct = total_margin[["w"]],
                                                   cpct_responses = response_column_margin[["w"]],
                                                   rpct = row_margin[["w"]]
                                                   ),
                                   stat_type = stat_type)
    }    
        
    ################
    
    dtable[, cell_var := set_var_lab(cell_var, cell_var_lab)]
    dtable[, cell_var := set_val_lab(cell_var, cell_val_lab)]
    dtable[, col_var := set_var_lab(col_var, col_var_lab)]
    dtable[, col_var := set_val_lab(col_var, col_val_lab)]
    if(use_row_var){
        dtable[, row_var := set_var_lab(row_var, row_var_lab)]
        dtable[, row_var := set_val_lab(row_var, row_val_lab)]
    }
    ### make rectangular table  
    if(use_row_var){
        row_var_name = "row_var"
    } else {
        row_var_name = NULL 
    }
    res = long_datatable_to_table(dtable, rows = c(row_var_name, "cell_var"), 
                                  columns = "col_var", values = "value")
    

    colnames(res)[1 + use_row_var] = "row_labels"

    if(total_row_position!="none"){
        total_rows = make_total_rows(
            need_row_var = has_row_var(cases),
            column_margin = column_margin,
            total_margin = total_margin,
            response_column_margin = response_column_margin,
            row_margin = row_margin,
            total_statistic = total_statistic,
            total_label = total_label
        )   
        total_rows[, col_var := set_var_lab(col_var, col_var_lab)]
        total_rows[, col_var := set_val_lab(col_var, col_val_lab)]
        if(use_row_var){
            total_rows[, row_var := set_var_lab(row_var, row_var_lab)]
            total_rows[, row_var := set_val_lab(row_var, row_val_lab)]
        }
        # total_rows[, row_labels := factor(row_labels, levels = unique(row_labels))]
        total_rows = long_datatable_to_table(total_rows, 
                                      rows = c(row_var_name, "row_labels"), 
                                      columns = "col_var", 
                                      values = "total")
        # total_rows = total_rows[, -"item"]
        res = add_total_to_table(res, total_rows = total_rows, total_row_position = total_row_position)
    }    
    
    if(use_row_var){
        res[, row_labels := paste0(row_var_lab, "|", row_var, "|", cell_var_lab, "|", row_labels)]
        res[, row_var:=NULL ]
    } else {
        res[, row_labels := paste0(cell_var_lab, "|", row_labels)]    
    }
    setnames(res, c(colnames(res)[1] , paste0(col_var_lab, "|", colnames(res)[-1])))
    
    res[ , row_labels := remove_unnecessary_splitters(row_labels)] 
    res[ , row_labels := make_items_unique(row_labels)] 
    setnames(res, remove_unnecessary_splitters(colnames(res)))
    res = as.sheet(res)
    class(res) = union("etable", class(res))
    res
}

#########
margin_from_raw = function(raw_data, margin = c("columns", "rows", "total"), use_weight){
    margin = match.arg(margin)
    aggr_names = switch(margin,
                        columns = extract_col_names(raw_data),
                        rows =  extract_cell_names(raw_data),        
                        total = NULL
    )
    dtotal = internal_cases(raw_data,
                            col_var_names = aggr_names,
                            cell_var_names = NULL,
                            use_weight = use_weight)
    if(margin=="rows"){
        setnames(dtotal, "col_var", "cell_var")
    }
    setnames(dtotal, "value", "total")
    dtotal
}
#########
margin_from_cases = function(cases, margin = c("columns", "rows", "total")){
    # to pass CRAN check
    value = NULL
    
    margin = match.arg(margin)
    if(has_row_var(cases)) {
        row_var_name = "row_var"
    } else {
        row_var_name = NULL 
    }
    margin = switch(margin,
                    columns =  "col_var",
                    rows =  "cell_var",
                    total =  NULL
    )
    by_str = paste(c(row_var_name, margin), collapse = ",")    
    dtotal = cases[, list(total = sum(value, na.rm = TRUE)), by = by_str]
    dtotal
}

# return list with weighted and unweighted column margin
# if we don't need some margins there was NULL instead of this margin
calculate_column_margin = function(raw_data, cases, stat_type, total_statistic, use_weight){
    if((stat_type == "cpct") || 
       any(total_statistic %in% c("w_cases", "u_cases", "w_tpct", "u_tpct", "w_rpct", "u_rpct"))
    ) {
        need_unweighted =  use_weight && any(total_statistic %in% c("u_cases", "u_tpct", "u_rpct"))
        cell_var_names = extract_cell_names(raw_data)
        if(!use_weight || (stat_type=="cpct") || any(total_statistic %in% c("w_cases", "w_tpct", "w_rpct"))){
            if(length(cell_var_names)>1){
                w_margin = margin_from_raw(raw_data = raw_data, 
                                           margin = "column",
                                           use_weight = use_weight
                )
            } else {
                w_margin = margin_from_cases(cases = cases, 
                                             margin = "column"
                )
            }
        } else {
            w_margin = NULL
        }
        if(need_unweighted){
            u_margin = margin_from_raw(raw_data = raw_data, 
                                       margin = "column",
                                       use_weight = FALSE
            )
        } else {
            u_margin = w_margin
        }
        list("w" = w_margin, "u" = u_margin)
    }  else {
        list("w" = NULL, "u" = NULL)
    }
    
}

# return list with weighted and unweighted column margin
# if we don't need some margins there was NULL instead of this margin
calculate_row_margin = function(raw_data, 
                                cases,
                                stat_type,
                                total_statistic, 
                                use_weight){
    
    margin = list(w = NULL, u = NULL)
    if(stat_type %in% c("rpct")){
        col_var_names = extract_col_names(raw_data)
        if(length(col_var_names)>1){
            margin$w = margin_from_raw(raw_data = raw_data, 
                                       margin = "row",
                                       use_weight = use_weight
            )       
        } else {
            margin$w = margin_from_cases(cases = cases, 
                                         margin = "row"
            )
        }
    } 
    margin
}

# return list with weighted and unweighted column margin
# if we don't need some margins there was NULL instead of this margin
calculate_response_column_margin = function(raw_data, 
                                            cases,
                                            stat_type,
                                            total_statistic, 
                                            use_weight){
    # to pass CRAN check
    value = NULL
    if(stat_type=="cpct_responses" || any(total_statistic %in% c("u_responses", "w_responses"))){  
        if(has_row_var(raw_data)){
            by_str = "row_var,col_var"
        } else {
            by_str = "col_var"
        }
        if(!use_weight || any(total_statistic %in% c("w_responses")) || stat_type=="cpct_responses" ){
            w_margin = cases[, list(total = sum(value, na.rm = TRUE)), by = by_str]
        } else {
            w_margin = NULL
        }
        if(use_weight && any(total_statistic %in% c("u_responses"))){
            u_cases = internal_cases(raw_data,
                                     col_var_names = extract_col_names(raw_data),
                                     cell_var_names = extract_cell_names(raw_data),
                                     use_weight = FALSE)
            u_margin = u_cases[, list(total = sum(value, na.rm = TRUE)), by = by_str] 
        } else {
            u_margin = w_margin
        }
        list(w = w_margin, u = u_margin)
    } else {
        list(w = NULL, u = NULL)
    }
}

calculate_total_margin = function(raw_data, 
                                  cases,
                                  stat_type,
                                  total_statistic, 
                                  use_weight){
    
    if((stat_type == "tpct") || 
       any(total_statistic %in% c("w_tpct", "u_tpct", "w_rpct", "u_rpct"))
    ) {
        need_unweighted =  use_weight && any(total_statistic %in% c("u_tpct", "u_rpct"))
        col_var_names = extract_col_names(raw_data)
        cell_var_names = extract_cell_names(raw_data)
        if(!use_weight || (stat_type=="tpct") || any(total_statistic %in% c("w_tpct", "w_rpct"))){
            if(length(cell_var_names)>1 || length(col_var_names)>1){
                w_margin = margin_from_raw(raw_data = raw_data, 
                                           margin = "total",
                                           use_weight = use_weight
                )
            } else {
                w_margin = margin_from_cases(cases = cases, 
                                             margin = "total"
                )
            }
        } else {
            w_margin = NULL
        }
        if(need_unweighted){
            u_margin = margin_from_raw(raw_data = raw_data, 
                                       margin = "total",
                                       use_weight = FALSE
            )
        } else {
            u_margin = w_margin
        }
        list("w" = w_margin, "u" = u_margin)
    }  else {
        list("w" = NULL, "u" = NULL)
    }
}

########################
# argument - list of data.tables possibly nested (no more than 2 levels)
# all data.tables will be combine in single data.table and then aggregated 
# by all variables except 'value'
# names of columns may be different
rbindlist_and_aggregate = function(list_of_datatables){
    value = NULL
    if(length(list_of_datatables)==1){
        if(is.data.table(list_of_datatables[[1]])){
            return(list_of_datatables[[1]])
        }
        if(length(list_of_datatables[[1]])==1){
            return(list_of_datatables[[1]][[1]])
        }
    }    
    if(!is.data.table(list_of_datatables[[1]])){
       list_of_datatables = unlist(list_of_datatables, recursive = FALSE, use.names = FALSE)    
    }
    res = rbindlist(list_of_datatables, use.names = FALSE, fill = FALSE)
    by_str = paste(colnames(res)[!(colnames(res) %in% "value")], collapse = ",")
    res[, list(value = sum(value, na.rm = TRUE)), by = by_str]
     
}


internal_cases = function(raw_data, col_var_names, cell_var_names = NULL, use_weight){
    # to pass CRAN check
    weight = NULL
    value = NULL
    col_var = NULL
    cell_var = NULL
    row_var = NULL
    if(has_row_var(raw_data)){
        row_var_name = "row_var"
    } else {
        row_var_name = NULL
    }
    if(is.null(cell_var_names)) cell_var_names = list(NULL) 
    if(is.null(col_var_names)) col_var_names = list(NULL) 
    res = lapply(cell_var_names, function(each_cell) { 
        res = lapply(col_var_names, function(each_col){
            by_str = paste(c(row_var_name, each_cell, each_col), collapse = ",")
            if(use_weight){
                dres = raw_data[, list(value = sum(weight, na.rm = TRUE)), 
                                by = by_str] 
            } else {
                dres = raw_data[, list(value = .N), 
                                by = by_str]                 
            }
        })
    })
    res = rbindlist_and_aggregate(res) 
    if(is.null(cell_var_names[[1]])) {
        cell_var_names = NULL
    } else {
        cell_var_names = "cell_var"
    }
    if(is.null(col_var_names[[1]])) {
        col_var_names = NULL
    } else {
        col_var_names = "col_var"
    }
    setnames(res, c(row_var_name, cell_var_names, col_var_names, "value"))    

    complete = complete.cases(res[,-"value"])
    if(!all(complete)){
        res = res[complete, ]
    }
    res[, value := as.double(value)]
    res
}


calculate_percent = function(cases, margin, stat_type){
    # to pass CRAN check
    value = NULL
    res = data.table::copy(cases)
    if(stat_type == "tpct"){
        if(has_row_var(res)){
            res = margin[res, on = "row_var", nomatch = NA]
            res = res[, value := value/total*100][, -"total"]    
        } else {
            res[, value:=value/margin[[1]]*100]    
        }
    } else {
        by_vec = intersect(colnames(res), colnames(margin))
        res = margin[res, on = by_vec, nomatch = NA]
        res = res[, value := value/total*100][, -"total"]   
    }
    res
}

###########################

make_total_rows = function(need_row_var,
                           column_margin,
                           total_margin,
                           response_column_margin,
                           row_margin,
                           total_statistic = total_statistic,
                           total_label = total_label){
    # "u_cases", "u_responses", "u_cpct", "u_rpct", "u_tpct",
    #   "w_cases", "w_responses", "w_cpct", "w_rpct", "w_tpct"
    
    # to pass CRAN check
    row_labels = NULL
    value = NULL
    ### labels
    
    total_statistic_label = gsub("^u_", " ", total_statistic, perl = TRUE)
    total_statistic_label = gsub("^w_", " wtd. ", total_statistic_label, perl = TRUE)
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
     ############
    total_row = lapply(seq_along(total_statistic), function(item){
        curr_statistic = total_statistic[[item]]
        weight = substr(curr_statistic, 1,1)  
        curr_statistic = gsub("^(u|w)_", "", curr_statistic, perl = TRUE)
        
        dtotal = switch(curr_statistic, 
                        cases = data.table::copy(column_margin[[weight]]),
                        responses = data.table::copy(response_column_margin[[weight]]),
                        cpct = {
                            res = data.table::copy(column_margin[[weight]])
                            res = res[, total:= as.double(100*(!is.na(total) | NA))]
                            res
                        },
                        rpct = {
                            res = data.table::copy(column_margin[[weight]])
                            setnames(res, "total", "value")
                            if(need_row_var){
                                res = total_margin[[weight]][res, on = c("row_var")]
                                res[, total:= value/total*100, by = "row_var"]
                            } else {
                                res[, total:= value/total_margin[[weight]][[1]]*100]                                
                            }    
                            res[,-"value"]
                        },
                        tpct = {
                            res = data.table::copy(column_margin[[weight]])
                            setnames(res, "total", "value")
                            if(need_row_var){
                                res = total_margin[[weight]][res, on = c("row_var")]
                                res[, total:= value/total*100, by = "row_var"]
                            } else {
                                res[, total:= value/total_margin[[weight]][[1]]*100]                                
                            }    
                            res[,-"value"]
                        }
        )
        curr_lab = total_label[item]
        dtotal[, row_labels := curr_lab] 
        dtotal
    })
    if(length(total_statistic)>1){
        # restore factor levels
        old_levels = lapply(total_row[[1]], levels)
        total_row = rbindlist(total_row, fill = FALSE, use.names = TRUE)
        
        # workaround for new behavior of data.table - rbind drop levels so we restore them
        for(i in seq_along(total_row)){
            if(!is.null(old_levels[[i]])){
                levels(total_row[[i]]) = old_levels[[i]]
            }
        }
        
    } else {
        total_row = total_row[[1]]
    }
    total_row[ , row_labels := factor(row_labels, levels = total_label)] 
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
    if("row_var" %in% colnames(res)){
        setkeyv(res, c("row_var", "..index__"), verbose = FALSE)
    } else {
        setkeyv(res, c("..index__"), verbose = FALSE)    
    }
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
    if(!is.null(row_vars)) {
        row_vars = flat_list(multiples_to_single_columns_with_dummy_encoding(row_vars), flat_df = TRUE)
    } else {
        row_vars = list(NULL)
    }
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


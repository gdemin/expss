TOTAL_STATISTICS = c("u_cases", "w_cases", "u_responses", "w_responses", "u_cpct", "w_cpct",
                     "u_rpct", "w_rpct", "u_tpct", "w_tpct")

#' Cross tabulation with support of labels, weights and multiple response variables.
#' 
#' \itemize{
#' \item{\code{cro}}{ returns data.frame with counts (possibly weighted) with column and
#' row totals.}
#' \item{\code{cro_pct}, \code{cro_cpct}, \code{cro_rpct}}{ return data.frame with 
#' table/column/row percent with column and row totals. There are always 
#' weighted counts instead of margin with 100\%. Empty labels/factor levels are 
#' removed from results of these functions. Base for multiple response (x is 
#' data.frame) percent is number of valid cases (not sum of responses) so sum of
#' percent may be greater than 100. Case is considered as valid if it has at
#' least one non-NA value.}
#' \item{\code{cro_mean}, \code{cro_sum}, \code{cro_median}}{ return data.frame with 
#' mean/sum/median. Empty labels/factor levels are 
#' removed from results of these functions. NA's are always omitted.}
#' \item{\code{cro_fun}, \code{cro_fun_df}}{ return data.frame with custom 
#' summary statistics defined by 'fun' argument. Empty labels/factor levels in 
#' predictor are removed from results of these functions. NA's treatment depends
#' on your 'fun' behavior. To use weight you should have 'weight' argument in 
#' 'fun' and some logic for its proccessing inside.\code{cro_fun} applies 'fun'
#' on each column in 'x' separately, \code{cro_fun_df} gives to 'fun' x as a
#' whole data.frame. So \code{cro_fun(iris[, -5], iris$Species, fun = mean)}
#' gives the same result as \code{cro_fun_df(iris[, -5], iris$Species, fun =
#' colMeans)}. For \code{cro_fun_df} names of 'x' will converted to labels if
#' they are available before 'fun' is applied. You should take care to return
#' from 'fun' rectangular object with appropriate row/column names - they will
#' be used in final result as labels.}
#' }
#' 
#' @param x vector/data.frame. data.frames are considered as multiple response
#'   variables.
#' @param cell_vars vector. By now multiple-response predictor is not supported.
#' @param col_vars vector. By now multiple-response predictor is not supported.
#' @param weight numeric vector. Optional case weights. NA's and negative weights
#'   treated as zero weights.
#' @param total_row_position Position ot total row in the resulting table. Can
#'   be one of "below", "above", "none".
#' @param weighted_total  By default it is "unweighted". Can be "unweighted" or/and "weighted". You can show weighted total/unweighted total or both.
#' @param total_row_title By default "#Total". You can provide two titles - for weighted and unqeighted totals.
#' @param fun custom summary function. It should always return
#'   scalar/vector/matrix of the same size.
#' @param ... further arguments for \code{fun}   
#'
#' @return object of class 'etable'. Basically it's a data.frame but class
#'   is needed for custom methods.
#'
#' @examples
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       mpg = "Miles/(US) gallon|Mean",
#'                       cyl = "Number of cylinders",
#'                       disp = "Displacement (cu.in.)|Mean",
#'                       hp = "Gross horsepower|Mean",
#'                       drat = "Rear axle ratio",
#'                       wt = "Weight (lb/1000)",
#'                       qsec = "1/4 mile time|Mean",
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
#' # multiple banners
#' calculate(mtcars, cro(cyl, list("#Total", vs, am, carb)))
#' 
#' # column percent
#' calculate(mtcars, cro_cpct(cyl, list("#Total", vs, am, carb)))
#' 
#' # nested banner
#' calculate(mtcars, cro(cyl, list("#Total", vs %nest% am, carb)))
#' 
#' # stacked variables
#' calculate(mtcars, cro(list(cyl, carb), list("#Total", vs %nest% am)))
#' 
#' # nested variables
#' calculate(mtcars, cro(am %nest% cyl, list("#Total", vs, carb)))
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
#' val_lab(score) = make_labels("
#'                              -1 Dislike it
#'                              0 So-so
#'                              1 Like it    
#'                              ")
#' 
#' cro(mrset(brands), list("#Total", score))
#' cro_cpct(mrset(brands), list("#Total", score))
#' 
#' # 'cro_mean'
#' 
#' data(iris)
#' cro_mean(iris[, -5], iris$Species)
#' 
#' # 'cro_fun'
#' 
#' data(mtcars)
#' mtcars = modify(mtcars,{
#'     var_lab(vs) = "Engine"
#'     val_lab(vs) = c("V-engine" = 0, 
#'                     "Straight engine" = 1) 
#'     var_lab(hp) = "Gross horsepower"
#'     var_lab(mpg) = "Miles/(US) gallon"
#' })
#' 
#' # Label for 'disp' forgotten intentionally
#' with(mtcars, cro_fun(data.frame(hp, mpg, disp), vs, summary))
#' 
#' # or, the same with transposed summary
#' with(mtcars, cro_fun(data.frame(hp, mpg, disp), vs, function(x) t(summary(x))))
#' 
#' # very artificial example
#' a = c(1,1,1, 1, 1)
#' b = c(0, 1, 2, 2, NA)
#' weight = c(0, 0, 1, 1, 1)
#' cro_fun(b, a, weight = weight, 
#'      fun = function(x, weight, na.rm){
#'                  weighted.mean(x, w = weight, na.rm = na.rm)
#'              }, 
#'      na.rm = TRUE)
#' 
#' 
#' # comparison 'cro_fun' and 'cro_fun_df'
#' 
#' data(iris)
#' cro_fun(iris[, -5], iris$Species, fun = mean)
#' # same result
#' cro_fun_df(iris[, -5], iris$Species, fun = colMeans)  
#' 
#' # usage for 'cro_fun_df' which is not possible for 'cro_fun'
#' # calculate correlations of variables with Sepal.Length inside each group
#' cro_fun_df(iris[,-5], iris$Species, fun = function(x) cor(x)[,1])
#' 
#' # or, pairwise correlations inside groups
#' cro_fun_df(iris[,-5], iris$Species, fun = cor)
#' @export
cro = function(cell_vars, 
               col_vars = total(), 
               row_vars = total(label = ""),
               weight = NULL,
               subgroup = NULL,
               total_label = "#Total",
               total_statistic = "u_cases",
               total_row_position = c("below", "above", "none")
               ){
    
    str_cell_vars = deparse(substitute(cell_vars))
    str_col_vars = deparse(substitute(col_vars))
    str_row_vars = deparse(substitute(row_vars))
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
cro_cpct = function(cell_vars, 
                    col_vars = total(), 
                    row_vars = total(label = ""),
                    weight = NULL,
                    subgroup = NULL,
                    total_label = "#Total",
                    total_statistic = "u_cases",
                    total_row_position = c("below", "above", "none")
                    ){
    
    str_cell_vars = deparse(substitute(cell_vars))
    str_col_vars = deparse(substitute(col_vars))
    str_row_vars = deparse(substitute(row_vars))
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
                    total_label = "#Total",
                    total_statistic = "u_cases",
                    total_row_position = c("below", "above", "none")
                    ){
    
    str_cell_vars = deparse(substitute(cell_vars))
    str_col_vars = deparse(substitute(col_vars))
    str_row_vars = deparse(substitute(row_vars))
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
                    total_label = "#Total",
                    total_statistic = "u_cases",
                    total_row_position = c("below", "above", "none")
                    ){
    
    str_cell_vars = deparse(substitute(cell_vars))
    str_col_vars = deparse(substitute(col_vars))
    str_row_vars = deparse(substitute(row_vars))
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
                              total_label = "#Total",
                              total_statistic = "u_responses",
                              total_row_position = c("below", "above", "none")
                              ){
    
    str_cell_vars = deparse(substitute(cell_vars))
    str_col_vars = deparse(substitute(col_vars))
    str_row_vars = deparse(substitute(row_vars))
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
    
    ### preparations
    total_row_position = match.arg(total_row_position)
    total_statistic = match.arg(total_statistic, TOTAL_STATISTICS, several.ok = TRUE)
    stat_type = match.arg(stat_type)
    max_nrow = max(NROW(cell_var), NROW(col_var))
    
    weight = if_null(weight, 1)
    weight = set_negative_and_na_to_zero(weight)
    weight = recycle_if_single_row(weight, max_nrow)
    
    valid = valid(cell_var) & valid(col_var) & valid(row_var) & (weight>0) & if_null(subgroup, TRUE)

    weight = universal_subset(weight, valid)

    col_var = recycle_if_single_row(col_var, max_nrow)
    col_var = universal_subset(col_var, valid)
    
    row_var = recycle_if_single_row(row_var, max_nrow)
    row_var = universal_subset(row_var, valid)

    cell_var = recycle_if_single_row(cell_var, max_nrow)
    cell_var = universal_subset(cell_var, valid)

    cell_var_lab = var_lab(cell_var)
    cell_val_lab = val_lab(cell_var)
    
    row_var_lab = var_lab(row_var)
    
    col_var_lab = var_lab(col_var)
    col_val_lab = val_lab(col_var)
    

    raw_data = cbind(as.data.table(cell_var), 
                     as.data.table(col_var), 
                     data.table(weight),
                     data.table(row_var))
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
    ### make rectangular table  
    res = long_datatable_to_table(dtable, rows = c("row_var", "cell_var"), columns = "col_var", value = "value")
    
    
    colnames(res)[2] = "row_labels"
    
    if(total_row_position!="none"){
        res = add_total_to_table(
            res = res, 
            raw_data = raw_data,
            col_var_names = col_var_names,
            total_row_position = total_row_position,
            total_statistic = total_statistic,
            total_label = total_label
        )    
    }    

    
    res[, row_labels := paste0(row_var_lab, "|", row_var, "|", cell_var_lab, "|", row_labels)]
    res[["row_var"]] = NULL
    colnames(res)[-1] = paste0(col_var_lab, "|", colnames(res)[-1]) 

    res[ , row_labels := remove_unnecessary_splitters(row_labels)] 
    res[ , row_labels := make_items_unique(row_labels)] 
    setnames(res, remove_unnecessary_splitters(colnames(res)))
    res = as.dtfrm(res)
    class(res) = union("etable", class(res))
    res
}

######################
internal_cpct = function(raw_data, col_var_names, cell_var_names = NULL, use_weight){
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
    dtable = internal_cases(raw_data, 
                            col_var_names = col_var_names,
                            cell_var_names = cell_var_names,
                            use_weight = use_weight)
    dtable[, value := value/sum(value, na.rm = TRUE)*100, by = "row_var,col_var"]    
}

#######################
internal_rpct = function(raw_data, col_var_names, cell_var_names = NULL, use_weight){
    dtable = internal_cases(raw_data, 
                            col_var_names = col_var_names,
                            cell_var_names = cell_var_names,
                            use_weight = use_weight)
    row_total = internal_cases(raw_data, col_var_names = cell_var_names,
                               use_weight = use_weight)
    setnames(row_total, "col_var", "cell_var")
    setnames(row_total, "value", "total")
    dtable = row_total[dtable, on = c("row_var","cell_var"), nomatch = NA]
    dtable[, value := value/total*100]    
}

#######################
internal_tpct = function(raw_data, col_var_names, cell_var_names = NULL, use_weight){
    dtable = internal_cases(raw_data, 
                            col_var_names = col_var_names,
                            cell_var_names = cell_var_names,
                            use_weight = use_weight)
    if(use_weight){
        dtotal = raw_data[, list(total = sum(weight, na.rm = TRUE)), by = "row_var" ]

    } else {
        dtotal = raw_data[, list(total = .N), by = "row_var" ]
    }
    dtable = dtotal[dtable, on = "row_var", nomatch = NA]
    dtable[, value := value/total*100]  
}

########################
internal_cases = function(raw_data, col_var_names, cell_var_names = NULL, use_weight){

    # dtable[, col_names] = unlab(dtable[, col_names, with = FALSE])

    if(is.null(cell_var_names)){
        res = lapply(col_var_names, function(each_col){
            by_str = paste0("row_var,", each_col)
            if(use_weight){
                dres = raw_data[, list(value = sum(weight, na.rm = TRUE)), by = by_str] 
            } else {
                dres = raw_data[, list(value = .N), by = by_str]                 
            }
            setnames(dres, each_col, "col_var")
            
        })
        res = rbindlist(res, use.names = TRUE, fill = TRUE)
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
                setnames(dres, each_col, "col_var")
                setnames(dres, each_cell, "cell_var")
                
            })
            res = rbindlist(res, use.names = TRUE, fill = TRUE)
        })
        res = rbindlist(res, use.names = TRUE, fill = TRUE)
        res = res[, list(value = sum(value, na.rm = TRUE)), by = "row_var,col_var,cell_var"]
        res = res[!is.na(col_var) & !is.na(cell_var), ]
    }
    res[, value := as.double(value)]
    res
}

#################################
########################

# used only for total 
internal_responses = function(raw_data, col_var_names, use_weight){
    
    # dtable[, col_names] = unlab(dtable[, col_names, with = FALSE])
    cell_var_names = setdiff(colnames(raw_data), c(col_var_names, "weight", "row_var"))
    res = internal_cases(raw_data, 
                         col_var_names = col_var_names,
                         cell_var_names = cell_var_names,
                         use_weight = use_weight
                         )
    res[, list(value = sum(value, na.rm = TRUE)), by = "row_var,col_var"]
}

###########################

add_total_to_table = function(res, raw_data, col_var_names, 
                              total_row_position, total_statistic, total_label){
    if(length(total_label) < length(total_statistic)) {
        total_label = rep(total_label, length(total_statistic))
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
        dtotal[, row_labels := ""] 
        row = long_datatable_to_table(dtotal, 
                                rows = c("row_var", "row_labels"), 
                                columns = "col_var", 
                                value = "value")
        row[["row_labels"]] = add_first_symbol_to_total_label(total_label[item])
        row
    })
    total_row = do.call(add_rows, total_row)
    if(total_row_position=="above"){
        res = add_rows(total_row, res)
    } else {
        res = add_rows(res, total_row)
    }
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




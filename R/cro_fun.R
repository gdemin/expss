#' Cross-tabulation with custom summary function.
#' 
#' \itemize{
#' \item{\code{cro_mean}, \code{cro_sum}, \code{cro_median}}{ calculate 
#' mean/sum/median by groups. NA's are always omitted.}
#' \item{\code{cro_mean_sd_n}}{ calculates mean, standard deviation and N
#' simultaneously. Mainly intended for usage with \link{significance_means}.}
#' \item{\code{cro_pearson}, \code{cro_spearman}}{ calculate correlation of 
#' first variable in each data.frame in \code{cell_vars} with other variables. 
#' NA's are removed pairwise.}
#' \item{\code{cro_fun}, \code{cro_fun_df}}{ return table with custom summary 
#' statistics defined by \code{fun} argument. NA's treatment depends on your 
#' \code{fun} behavior. To use weight you should have formal \code{weight} 
#' argument in \code{fun} and some logic for its processing inside. Several 
#' functions with weight support are provided - see \link{w_mean}. 
#' \code{cro_fun} applies \code{fun} on each variable in \code{cell_vars} 
#' separately, \code{cro_fun_df} gives to \code{fun} each data.frame in 
#' \code{cell_vars} as a whole. So \code{cro_fun(iris[, -5], iris$Species, fun =
#' mean)} gives the same result as \code{cro_fun_df(iris[, -5], iris$Species, 
#' fun = colMeans)}. For \code{cro_fun_df} names of \code{cell_vars} will 
#' converted to labels if they are available before the \code{fun} will be applied. 
#' Generally it is recommended that \code{fun} will always return object of the 
#' same form. Row names/vector names of \code{fun} result will appear in the row
#' labels of the table and column names/names of list will appear in the column 
#' labels. If your \code{fun} returns data.frame/matrix/list with element named
#' 'row_labels' then this element will be used as row labels. And it will have
#' precedence over rownames.}
#' \item{\code{calc_cro_*}}{ are the same as above but evaluate their arguments
#' in the context of the first argument \code{data}.}
#' \item{\code{combine_functions}}{ is auxiliary function for combining several 
#' functions into one function for usage with \code{cro_fun}/\code{cro_fun_df}.
#' Names of arguments will be used as statistic labels. By default, results of
#' each function are combined with \link{c}. But you can provide your own method
#' function with \code{method} argument. It will be applied as in the expression
#' \code{do.call(method, list_of_functions_results)}. Particular useful method
#' is \code{list}. When it used then statistic labels will appear in the column
#' labels. See examples. Also you may be interested in \code{data.frame}, 
#' \code{rbind}, \code{cbind} methods.}}
#' 
#' @param cell_vars vector/data.frame/list. Variables on which summary function
#'   will be computed. 
#' @param col_vars vector/data.frame/list. Variables which breaks table by
#'   columns. Use \link{mrset}/\link{mdset} for multiple-response variables.
#' @param row_vars vector/data.frame/list. Variables which breaks table by rows.
#'   Use \link{mrset}/\link{mdset} for multiple-response variables.
#' @param weight numeric vector. Optional cases weights. Cases with NA's,
#'   negative and zero weights are removed before calculations.
#' @param subgroup logical vector. You can specify subgroup on which table will be computed. 
#' @param fun custom summary function. Generally it is recommended that 
#'   \code{fun} will always return object of the same form. Rownames/vector 
#'   names of \code{fun} result will appear in the row labels of the table and 
#'   column names/names of list will appear in the column labels. To use weight 
#'   you should have formal \code{weight} argument in \code{fun} and some logic 
#'   for its processing inside. For \code{cro_fun_df} \code{fun} will receive 
#'   \link[data.table]{data.table} with all names converted to variable labels
#'   (if labels exists). So it is not recommended to rely on original variables
#'   names in your \code{fun}.
#' @param data data.frame in which context all other arguments will be evaluated
#'   (for \code{calc_cro_*}).
#' @param ... further arguments for \code{fun}  in 
#'   \code{cro_fun}/\code{cro_fun_df} or functions for \code{combine_functions}.
#'   Ignored in \code{cro_fun}/\code{cro_fun_df} if \code{unsafe} is TRUE.
#' @param weighted_valid_n logical. Should we show weighted valid N in
#'   \code{cro_mean_sd_n}? By default it is FALSE.
#' @param labels character vector of length 3. Labels for mean, standard
#'   deviation and valid N in \code{cro_mean_sd_n}.
#' @param method function which will combine results of multiple functions in
#'   \code{combine_functions}. It will be applied as in the expression 
#'   \code{do.call(method, list_of_functions_results)}. By default it is
#'   \code{c}.
#' @param unsafe logical/character If not FALSE than \code{fun} will be 
#'   evaluated as is. It can lead to significant increase in the performance. 
#'   But there are some limitations. For \code{cro_fun} it means that your 
#'   function \code{fun} should return vector. If length of this vector is
#'   greater than one than you should provide with \code{unsafe} argument vector
#'   of unique labels for each element of this vector. There will be no attempts
#'   to automatically make labels for the results of \code{fun}. For 
#'   \code{cro_fun_df} your function should return vector or list/data.frame
#'   (optionally with 'row_labels' element - statistic labels). If \code{unsafe}
#'   is TRUE or not logical then further arguments (\code{...}) for \code{fun}
#'   will be ignored.
#'  
#'
#' @return object of class 'etable'. Basically it's a data.frame but class
#'   is needed for custom methods.
#' @seealso \link{tables}, \link{fre}, \link{cro}.
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
#' 
#' # Simple example - there is special shortcut for it - 'cro_mean'
#' calculate(mtcars, cro_fun(list(mpg, disp, hp, wt, qsec), 
#'                                col_vars = list(total(), am), 
#'                                row_vars = vs, 
#'                                fun = mean)
#' )
#' 
#' # the same result
#' calc_cro_fun(mtcars, list(mpg, disp, hp, wt, qsec), 
#'                      col_vars = list(total(), am), 
#'                      row_vars = vs, 
#'                      fun = mean
#' ) 
#' 
#' # The same example with 'subgroup'
#' calculate(mtcars, cro_fun(list(mpg, disp, hp, wt, qsec), 
#'                                col_vars = list(total(), am), 
#'                                row_vars = vs,
#'                                subgroup = vs == 0, 
#'                                fun = mean)
#' )
#'                                 
#' # 'combine_functions' usage  
#' calculate(mtcars, cro_fun(list(mpg, disp, hp, wt, qsec), 
#'                           col_vars = list(total(), am), 
#'                           row_vars = vs, 
#'                           fun = combine_functions(Mean = mean, 
#'                                                   'Std. dev.' = sd,
#'                                                   'Valid N' = valid_n)
#' ))  
#' # 'combine_functions' usage - statistic labels in columns
#' calculate(mtcars, cro_fun(list(mpg, disp, hp, wt, qsec), 
#'                           col_vars = list(total(), am), 
#'                           row_vars = vs, 
#'                           fun = combine_functions(Mean = mean, 
#'                                                   'Std. dev.' = sd,
#'                                                   'Valid N' = valid_n,
#'                                                   method = list
#'                                                   )
#' )) 
#' 
#' # 'summary' function
#' calculate(mtcars, cro_fun(list(mpg, disp, hp, wt, qsec), 
#'                           col_vars = list(total(), am), 
#'                           row_vars = list(total(), vs), 
#'                           fun = summary
#' ))  
#'                           
#' # comparison 'cro_fun' and 'cro_fun_df'
#' calculate(mtcars, cro_fun(
#'                        sheet(mpg, disp, hp, wt, qsec), 
#'                        col_vars = am,
#'                        fun = mean
#'                        )
#' )
#' 
#' # same result
#' calculate(mtcars, cro_fun_df(
#'                        sheet(mpg, disp, hp, wt, qsec), 
#'                        col_vars = am, 
#'                        fun = colMeans
#'                        )
#' ) 
#' 
#' # usage for 'cro_fun_df' which is not possible for 'cro_fun'
#' # linear regression by groups
#' calculate(mtcars, cro_fun_df(
#'                       sheet(mpg, disp, hp, wt, qsec), 
#'                       col_vars = am,
#'                       fun = function(x){
#'                             frm = reformulate(".", response = names(x)[1])
#'                             model = lm(frm, data = x)
#'                             sheet(
#'                                 'Coef. estimate' = coef(model), 
#'                                  confint(model)
#'                                  )
#'                       }
#' ))
#' @export
cro_fun = function(cell_vars, 
                   col_vars = total(), 
                   row_vars = total(label = ""),
                   weight = NULL,
                   subgroup = NULL,
                   fun, 
                   ...,
                   unsafe = FALSE){
    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    
    cell_vars = test_for_null_and_make_dataframe(cell_vars, str_cell_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    
    fun = match.fun(fun)
    if(!is.null(weight)){
        stopif(!("weight" %in% names(formals(fun))),
               "`weight` is provided but `fun` doesn't have formal `weight` argument.")
    }
    
    
    row_vars = flat_list(multiples_to_single_columns_with_dummy_encoding(row_vars), flat_df = TRUE)
    col_vars = flat_list(multiples_to_single_columns_with_dummy_encoding(col_vars), flat_df = TRUE)
    stopif(!is.null(subgroup) && !is.logical(subgroup), "'subgroup' should be logical.")
    
    check_sizes("'cro_fun'", cell_vars, row_vars, col_vars, weight, subgroup)
    
    if(is.logical(unsafe) && !unsafe){
        cell_vars = make_labels_from_names(cell_vars)
        fun = make_function_for_cro(fun, ..., need_weight = !is.null(weight))
    } else {
        cell_vars = unlab(names2labels(cell_vars))
    }
    
    res = lapply(row_vars, function(each_row_var){
        all_col_vars = lapply(col_vars, function(each_col_var){
            dtable = elementary_cro_fun_df(cell_var = cell_vars,
                                           row_var = each_row_var, 
                                           col_var = each_col_var, 
                                           weight = weight,
                                           subgroup = subgroup,
                                           fun = fun,
                                           use_lapply = unsafe
            )    
            row_var_lab = var_lab(dtable[["..row_var__"]])
            col_var_lab = var_lab(dtable[["..col_var__"]])
            ### make rectangular table  
            res = long_datatable_to_table(dtable, rows = c("..row_var__", "row_labels"), 
                                          columns = "..col_var__", 
                                          values = colnames(dtable) %d% c("..row_var__", "row_labels", "..col_var__")
            )
            format_table(res, 
                         row_var_lab = row_var_lab, 
                         col_var_lab = col_var_lab)  
        })
        Reduce(merge, all_col_vars)
    })
    
    res = do.call(add_rows, res)
    rownames(res) = NULL
    res
}



### compute statistics for single row_var and single col_var
elementary_cro_fun_df = function(cell_var, 
                                 col_var, 
                                 weight,
                                 fun,
                                 row_var,
                          subgroup, 
                          use_lapply = FALSE
                          ){
    # to pass CRAN check
    ..weight__ = NULL
    row_labels = NULL
    ### calculate vector of valid cases
    if(is.null(subgroup)){
      valid = valid(col_var) & valid(row_var)
    } else {
      valid = valid(col_var) & valid(row_var) & subgroup & !is.na(subgroup)
    }
    max_nrow = max(NROW(cell_var), NROW(col_var), NROW(row_var), NROW(weight))
    
    ## if any of vars is zero-length then we made all vars zero-length
    min_nrow = min(NROW(cell_var), NROW(col_var), NROW(row_var))
    if(any(min_nrow==0)) max_nrow = 0
    
    ##### prepare weight #####
    if(!is.null(weight)){
        weight = set_negative_and_na_to_zero(weight)
        weight = recycle_if_single_row(weight, max_nrow)
        valid = valid & (weight>0)
        weight = weight[valid]
    }
    

    ### recycle variables of length 1

    cell_var = recycle_if_single_row(cell_var, max_nrow)
    col_var = recycle_if_single_row(col_var, max_nrow)
    row_var = recycle_if_single_row(row_var, max_nrow)
    
    ### drop non-valid cases 
    if(!all(valid, na.rm = TRUE) || length(valid)==0){
        cell_var = universal_subset(cell_var, valid, drop = FALSE)
        col_var = universal_subset(col_var, valid)
        row_var = universal_subset(row_var, valid, drop = FALSE)
    } else {
        col_var = drop(col_var)
    }
    ###################

    ### pack data.table #####
    if(NCOL(cell_var)>1){
        colnames(cell_var) =  make_items_unique(colnames(cell_var))
    } 
    
    if(is.null(weight)){
        raw_data = data.table(..row_var__ = row_var, ..col_var__ = col_var, cell_var)
    } else {
        raw_data = data.table(..row_var__ = row_var, ..col_var__ = col_var, ..weight__ = weight, cell_var)
    }

    
    # statistics
    by_string = "..row_var__,..col_var__"
    if(is.logical(use_lapply) && !use_lapply){
        if(is.null(weight)){
            dtable = raw_data[ , fun(.SD), by = by_string]
        } else {
            dtable = raw_data[ , fun(.SD, weight = ..weight__), by = by_string, .SDcols = -"..weight__"]
        }
    } else {
        if(is.null(weight)){
            dtable = raw_data[ , lapply(.SD, fun), by = by_string]
        } else {
            dtable = raw_data[ , lapply(.SD[, -"..weight__"], fun, weight = ..weight__), by = by_string]
        }
        dtable = melt(dtable, id.vars = c("..row_var__", "..col_var__"), 
                      variable.name = "row_labels",
                      na.rm = FALSE, variable.factor = TRUE, 
                      value.factor = FALSE, 
                      verbose = FALSE)
        if(!isTRUE(use_lapply)){
            labels_length = length(use_lapply)
            stopif(labels_length==0 || (NROW(dtable) %% labels_length)!=0, 
                   "'cro_fun'/'cro_fun_df' - length of labels provided with 'unsafe' is incorrect: ", 
                   labels_length)
            stopif(anyDuplicated(use_lapply),  "'cro_fun'/'cro_fun_df' - duplicated labels provided with 'unsafe': ", 
                   paste(unique(use_lapply[duplicated(use_lapply)])), collapse = ", ")
            if(NROW(dtable)==0 && length(levels(dtable[["row_labels"]]))>0) {
              # fix bug with unneseccary line: 
              #            cro_mean_sd_n(1:4, total(), row_vars = set_val_lab(rep(NA, 4), c(a=1, b =2, c= 3, d= 4)))
              new_levels = expand.grid(use_lapply, levels(dtable[["row_labels"]]))
              levels(dtable[["row_labels"]])  = paste(new_levels[[2]], new_levels[[1]], sep = "|")
            } else {
              dtable[ , row_labels := paste(row_labels, use_lapply, sep = "|")]
            }
        }
        
    }    
    if(("row_labels" %in% colnames(dtable)) && !is.factor(dtable[["row_labels"]])){ 
        dtable[ , row_labels := fctr(row_labels, levels = unique(row_labels),
                          prepend_var_lab = FALSE)]
    }
    dtable
}    

    

    
########
  
format_table = function(wide_datable, row_var_lab, col_var_lab){  
    # to pass CRAN check
    row_labels = NULL
    ..row_var__ = NULL

    wide_datable[ , row_labels  := as.character(row_labels)] 
    wide_datable[ , row_labels  := paste0(..row_var__, "|", row_labels)]  
    wide_datable[["..row_var__"]] = NULL
    
    wide_datable[, row_labels := paste0(row_var_lab, "|", row_labels)]
    colnames(wide_datable)[-1] = paste0(col_var_lab, "|", colnames(wide_datable)[-1]) 

    wide_datable[ , row_labels := remove_unnecessary_splitters(row_labels)] 
    wide_datable[ , row_labels := make_items_unique(row_labels)] 
    colnames(wide_datable) = remove_unnecessary_splitters(colnames(wide_datable)) 
    wide_datable = as.sheet(wide_datable)
    class(wide_datable) = union("etable", class(wide_datable))
    wide_datable
}

#######
    
make_function_for_cro_df = function(fun, ..., need_weight = TRUE){
    # to pass CRAN check
    row_labels = NULL
    
    force(fun)
    force(need_weight)
    if(need_weight){
        function(x, weight = weight){
            res = fun(x, ..., weight = weight)
            res = make_dataframe_with_row_labels(res)
            # we need convert to factor to keep all row_labels as levels in case of aggregation 
            # of data with zero rows
            if(nrow(x)==0){
                res$row_labels = fctr(res[["row_labels"]], levels = unique(res[["row_labels"]]),
                                                 prepend_var_lab = FALSE)
            }
            res
        }
    } else {
        function(x){
            res = fun(x, ...)
            res = make_dataframe_with_row_labels(res)
            # we need convert to factor to keep all row_labels as levels in case of aggregation 
            # of data with zero rows
            if(nrow(x)==0){
                res$row_labels = fctr(res[["row_labels"]], levels = unique(res[["row_labels"]]),
                                      prepend_var_lab = FALSE)
            }
            res
        }        
    }
}

###############
make_function_for_cro = function(fun, ..., need_weight = TRUE){
    # to pass CRAN check
    row_labels = NULL
    force(fun)
    force(need_weight)
    if(need_weight){
        function(x, weight = weight){
            res = lapply(x, function(column) {
                varlab = var_lab(column)
                each_res = fun(unvr(column), ..., weight = weight)
                each_res = make_dataframe_with_row_labels(each_res)
                if(!is.null(varlab)){
                    each_res[["row_labels"]] = paste0(varlab, "|", each_res[["row_labels"]]) 
                }
                each_res
            })
            res = as.sheet(rbindlist(res, use.names = TRUE, fill = TRUE))
            # we need convert to factor to keep all row_labels as levels in case of aggregation 
            # of data with zero rows
            res$row_labels = make_items_unique(res[["row_labels"]])
            if(nrow(x)==0){
                res$row_labels = fctr(res[["row_labels"]], levels = unique(res[["row_labels"]]),
                                      prepend_var_lab = FALSE)
            }
            res
        }
    } else {
        function(x){
            res = lapply(x, function(column) {
                varlab = var_lab(column)
                each_res = fun(unvr(column), ...)
                each_res = make_dataframe_with_row_labels(each_res)
                if(!is.null(varlab)){
                    each_res[["row_labels"]] = paste0(varlab, "|", each_res[["row_labels"]])
                }
                each_res
            })
            res = as.sheet(rbindlist(res, use.names = TRUE, fill = TRUE))
            # we need convert to factor to keep all row_labels as levels in case of aggregation 
            # of data with zero rows
            res$row_labels = make_items_unique(res[["row_labels"]])
            if(nrow(x)==0){
                res$row_labels = fctr(res[["row_labels"]], levels = unique(res[["row_labels"]]),
                                      prepend_var_lab = FALSE)
            }
            res
        }        
    }
}

#############
make_dataframe_with_row_labels = function(res){
    if(is.table(res)){
        ### block for 'summary' and 'table'
        dm_names = dimnames(res)
        if(is.null(dm_names)){
            dm_names[[1]] = names(res)
        }
        new_df = matrix(NA, nrow= NROW(res), ncol = NCOL(res))
        new_df[] = res
        new_df = as.sheet(new_df)
        rownames(new_df) = NULL
        
        row_labels = dm_names[[1]]
        if(length(dm_names)>1){
            new_df = setNames(new_df, dm_names[[2]])
        } else {
            new_df = setNames(new_df, rep("|", NCOL(new_df)))
        }
        res = new_df
    } else {
        ####
        if(is.matrix(res) || is_list(res)) res = as.sheet(res)
        ####
        if(is.data.frame(res)) {
            if("row_labels" %in% names(res)){
                res[["row_labels"]] = make_items_unique(res[["row_labels"]])
            } else {
                row_labels = rownames(res)
                if(!is.null(row_labels) && length(row_labels)==1 && row_labels[1]==1){
                    row_labels = ""
                }
                res[["row_labels"]] = make_items_unique(row_labels)
            }
            return(res)
        } else {
        #### usual vectors and unknowns    
            row_labels = names(res)
            res = setNames(sheet(res), rep("|", NCOL(res)))
        } 
    }
    
    #######
    if(is.null(row_labels)){
        if(nrow(res)>1){
            row_labels = seq_len(nrow(res)) 
        } else {
            row_labels = rep("", nrow(res)) # for empty results   
        }
    } 
    row_labels = make_items_unique(as.character(row_labels))
    res[["row_labels"]] = row_labels
    res
}




#######

#' @export
#' @rdname cro_fun
cro_fun_df = function(cell_vars, 
                      col_vars = total(), 
                      row_vars = total(label = ""),
                      weight = NULL,
                      subgroup = NULL,
                      fun, 
                      ...,
                      unsafe = FALSE){
    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    str_col_vars = expr_to_character(substitute(col_vars))

    cell_vars = test_for_null_and_make_list(cell_vars, str_cell_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    
    fun = match.fun(fun)
    if(!is.null(weight)){
        stopif(!("weight" %in% names(formals(fun))),
               "`weight` is provided but `fun` doesn't have formal `weight` argument.")
    }
    if(is.logical(unsafe) && !unsafe) {
        fun = make_function_for_cro_df(fun, ..., need_weight = !is.null(weight))
    }
    cell_vars = make_labels_from_names(cell_vars)
    cell_vars = flat_list(cell_vars, flat_df = FALSE)
    row_vars = flat_list(multiples_to_single_columns_with_dummy_encoding(row_vars), flat_df = TRUE)
    col_vars = flat_list(multiples_to_single_columns_with_dummy_encoding(col_vars), flat_df = TRUE)
    
    stopif(!is.null(subgroup) && !is.logical(subgroup), "'subgroup' should be logical.")
    check_sizes("'cro_fun_df'", cell_vars, row_vars, col_vars, weight, subgroup)
    
    res = lapply(row_vars, function(each_row_var){
        all_cell_vars = lapply(cell_vars, function(each_cell_var){
            all_col_vars = lapply(col_vars, function(each_col_var){
                dtable = elementary_cro_fun_df(cell_var = names2labels(each_cell_var),
                                      row_var = each_row_var, 
                                      col_var = each_col_var, 
                                      weight = weight,
                                      subgroup = subgroup,
                                      fun = fun
                )    
                row_var_lab = var_lab(dtable[["..row_var__"]])
                col_var_lab = var_lab(dtable[["..col_var__"]])
                ### make rectangular table  
                res = long_datatable_to_table(dtable, rows = c("..row_var__", "row_labels"), 
                                              columns = "..col_var__", 
                                              values = colnames(dtable) %d% c("..row_var__", "row_labels", "..col_var__")
                )
                format_table(res, 
                             row_var_lab = row_var_lab, 
                             col_var_lab = col_var_lab)
            })
            Reduce(merge, all_col_vars)
        })
        do.call(add_rows, all_cell_vars) 
        
    })
    res = do.call(add_rows, res)
    rownames(res) = NULL
    res
}




######################################################


#' @export
#' @rdname cro_fun
cro_mean = function(cell_vars, 
                    col_vars = total(), 
                    row_vars = total(label = ""),
                    weight = NULL,
                    subgroup = NULL
){
    
    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    
    cell_vars = test_for_null_and_make_dataframe(cell_vars, str_cell_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    
    cro_fun(cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars, 
            weight = weight,
            subgroup = subgroup,
            # here we heavy rely on the fact that weight already checked on NA and negative values
            fun = function(x, weight = NULL) matrixStats::weightedMean(x, w = weight, na.rm = TRUE),
            unsafe = TRUE
    )
}



#' @export
#' @rdname cro_fun
cro_mean_sd_n = function(cell_vars, 
                         col_vars = total(), 
                         row_vars = total(label = ""),
                         weight = NULL,
                         subgroup = NULL,
                         weighted_valid_n = FALSE,
                         labels = NULL
                         
){
  
  str_cell_vars = expr_to_character(substitute(cell_vars))
  str_row_vars = expr_to_character(substitute(row_vars))
  str_col_vars = expr_to_character(substitute(col_vars))
  
  cell_vars = test_for_null_and_make_dataframe(cell_vars, str_cell_vars)
  row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
  col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
  if(is.null(labels)) {
    labels = c("Mean", "Std. dev.", 
               ifelse(weighted_valid_n, "Valid N", "Unw. valid N"))    
  } else {
    stopif(anyDuplicated(labels), "'cro_mean_sd_n' - all 'labels' should be unduplicated.")
    stopif(length(labels)!=3, "'cro_mean_sd_n' - 'labels' should be vector of three elements.")
  }
  if(weighted_valid_n){
    # here we heavy rely on the fact that weight already checked on NA and negative values
    fun = function(x, weight = NULL) {
      if(is.logical(x)){
        x = as.numeric(x)
      }
      c(
        {
          res = matrixStats::weightedMean(x, w = weight, na.rm = TRUE)
          if(is.nan(res)) res = NA
          res
        },
        weight_helper(matrixStats::weightedSd)(x, weight, na.rm = TRUE), 
        {
          validn = if(is.null(weight)) sum(!is.na(x)) else sum(weight[!is.na(x)], na.rm = TRUE)
          if(!is.na(validn) && validn==0) validn = NA
          validn
        }
      )
    }
  } else {
    fun = function(x, weight = NULL) {
      if(is.logical(x)){
        x = as.numeric(x)
      }
      c(
        {
          res = matrixStats::weightedMean(x, w = weight, na.rm = TRUE)
          if(is.nan(res)) res = NA
          res
        },
        weight_helper(matrixStats::weightedSd)(x, weight, na.rm = TRUE),
        {
          validn = sum(!is.na(x))
          if(!is.na(validn) && validn==0) validn = NA
          validn
        }
      )
    }
  }
  cro_fun(cell_vars = cell_vars, 
          col_vars = col_vars, 
          row_vars = row_vars, 
          weight = weight,
          subgroup = subgroup,
          fun = fun,
          unsafe = labels
  )
}


#' @export
#' @rdname cro_fun
cro_sum = function(cell_vars, 
                   col_vars = total(), 
                   row_vars = total(label = ""),
                   weight = NULL,
                   subgroup = NULL
){

    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    
    cell_vars = test_for_null_and_make_dataframe(cell_vars, str_cell_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    
    cro_fun(cell_vars = cell_vars, 
                         col_vars = col_vars, 
                         row_vars = row_vars, 
                         weight = weight,
                         subgroup = subgroup,
                         fun = if(is.null(weight)){
                             function(x) sum(x, na.rm = TRUE)
                         } else {
                             function(x, weight = NULL) sum(x*weight, na.rm = TRUE)
                         },
                         unsafe = TRUE
    )
}

#' @export
#' @rdname cro_fun
cro_median = function(cell_vars, 
                      col_vars = total(), 
                      row_vars = total(label = ""),
                      weight = NULL,
                      subgroup = NULL
){

    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    
    cell_vars = test_for_null_and_make_dataframe(cell_vars, str_cell_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    
    cro_fun(cell_vars = cell_vars, 
                         col_vars = col_vars, 
                         row_vars = row_vars, 
                         weight = weight,
                         subgroup = subgroup,
                         fun = function(x, weight = NULL){
                             matrixStats::weightedMedian(x = x, 
                                                         w = weight, 
                                                         na.rm = TRUE, 
                                                         interpolate = TRUE, 
                                                         ties = "weighted")
                         },
                         unsafe = TRUE
    )
}


###############################################
###############################################
###############################################

#' @export
#' @rdname cro_fun
cro_pearson = function(cell_vars, 
                         col_vars = total(), 
                         row_vars = total(label = ""),
                         weight = NULL,
                         subgroup = NULL
){
    fun = function(x, weight = NULL){
        w_pearson(x, weight = weight)[ , 1]
    }    
    
    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    
    cell_vars = test_for_null_and_make_list(cell_vars, str_cell_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    
    cro_fun_df(cell_vars = cell_vars, 
               col_vars = col_vars, 
               row_vars = row_vars, 
               weight = weight,
               subgroup = subgroup,
               fun = fun
    )
}


#' @export
#' @rdname cro_fun
cro_spearman = function(cell_vars, 
                           col_vars = total(), 
                           row_vars = total(label = ""),
                           weight = NULL,
                           subgroup = NULL
){
    fun = function(x, weight = NULL){
        w_spearman(x, weight = weight)[ , 1]
    }    
    
    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    
    cell_vars = test_for_null_and_make_list(cell_vars, str_cell_vars)
    row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    
    cro_fun_df(cell_vars = cell_vars, 
               col_vars = col_vars, 
               row_vars = row_vars, 
               weight = weight,
               subgroup = subgroup,
               fun = fun
    )
}

##########################

#' @export
#' @rdname cro_fun    
calc_cro_fun = function(data,
                        cell_vars, 
                        col_vars = total(), 
                        row_vars = total(label = ""),
                        weight = NULL,
                        subgroup = NULL,
                        fun, 
                        ...,
                        unsafe = FALSE){
    expr = substitute(cro_fun(
        cell_vars = cell_vars, 
        col_vars = col_vars, 
        row_vars = row_vars,
        weight = weight,
        subgroup = subgroup,
        fun = fun, 
        ...,
        unsafe = unsafe)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())
}

###########################

#' @export
#' @rdname cro_fun
calc_cro_fun_df = function(data,
                      cell_vars, 
                      col_vars = total(), 
                      row_vars = total(label = ""),
                      weight = NULL,
                      subgroup = NULL,
                      fun, 
                      ...,
                      unsafe = FALSE){
    expr = substitute(cro_fun_df(
        cell_vars = cell_vars, 
        col_vars = col_vars, 
        row_vars = row_vars,
        weight = weight,
        subgroup = subgroup,
        fun = fun, 
        ...,
        unsafe = unsafe)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())    
}

###########################

#' @export
#' @rdname cro_fun
calc_cro_mean = function(data,
                         cell_vars, 
                         col_vars = total(), 
                         row_vars = total(label = ""),
                         weight = NULL,
                         subgroup = NULL
){
    expr = substitute(
        cro_mean(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())      
}

#########################

#' @export
#' @rdname cro_fun
calc_cro_mean_sd_n = function(data,
                              cell_vars, 
                              col_vars = total(), 
                              row_vars = total(label = ""),
                              weight = NULL,
                              subgroup = NULL,
                              weighted_valid_n = FALSE,
                              labels = NULL
                              
){
    expr = substitute(
        cro_mean_sd_n(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup,
            weighted_valid_n =  weighted_valid_n,
            labels = labels)
    )
    calculate_internal(data, expr = expr, parent = parent.frame()) 
}

###########################

#' @export
#' @rdname cro_fun
calc_cro_sum = function(data,
                        cell_vars, 
                        col_vars = total(), 
                        row_vars = total(label = ""),
                        weight = NULL,
                        subgroup = NULL
){
    expr = substitute(
        cro_sum(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())      
}    

###########################

#' @export
#' @rdname cro_fun
calc_cro_median = function(data,
                           cell_vars, 
                           col_vars = total(), 
                           row_vars = total(label = ""),
                           weight = NULL,
                           subgroup = NULL
){
    expr = substitute(
        cro_median(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup)
    )
    calculate_internal(data, expr = expr, parent = parent.frame())      
}

############################

#' @export
#' @rdname cro_fun
calc_cro_pearson = function(data,
                            cell_vars, 
                            col_vars = total(), 
                            row_vars = total(label = ""),
                            weight = NULL,
                            subgroup = NULL
){
    expr = substitute(
        cro_pearson(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup)
    )
    calculate_internal(data, expr = expr, parent = parent.frame()) 
}

############################

#' @export
#' @rdname cro_fun
calc_cro_spearman = function(data,
                             cell_vars, 
                             col_vars = total(), 
                             row_vars = total(label = ""),
                             weight = NULL,
                             subgroup = NULL
){
    expr = substitute(
        cro_spearman(
            cell_vars = cell_vars, 
            col_vars = col_vars, 
            row_vars = row_vars,
            weight = weight,
            subgroup = subgroup)
    )
    calculate_internal(data, expr = expr, parent = parent.frame()) 
}

############################

#' @export
#' @rdname cro_fun
combine_functions = function(..., method = c){
    method = match.fun(method)
    possible_names = unlist(lapply(as.list(substitute(list(...)))[-1], expr_to_character))
    args = list(...)
    arg_names =names(args)
    if(is.null(arg_names)) {
        names(args) = possible_names
    } else {
        names(args)[arg_names==""] = possible_names[arg_names==""]
    }  
    function(x, weight = NULL){
        if(!is.null(weight)){
            for(each in seq_along(args)){
                stopif(!("weight" %in% names(formals(args[[each]]))),
                       paste0("`weight` is provided but function`", names(args)[each],
                              "` doesn't have formal `weight` argument."))
            }    
            res = lapply(args, function(f) f(x, weight = weight))
        } else {
            res = lapply(args, function(f) f(x))    
        }
        
        do.call(method, res)
    }
}


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
#' @param row_vars vector. By now multiple-response predictor is not supported.
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
cro_fun = function(cell_vars, 
                   col_vars = total(), 
                   row_vars = total(label = ""),
                   weight = NULL,
                   subgroup = NULL,
                   fun, 
                   ...){
    str_cell_vars = deparse(substitute(cell_vars))
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    
    stopif(is.null(cell_vars), 
           paste0("'", str_cell_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    
    fun = match.fun(fun)
    if(!is.null(weight)){
        stopif(!("weight" %in% names(formals(fun))),
               "`weight` is provided but `fun` doesn't have formal `weight` argument.")
    }
    fun = make_function_for_cro(fun, ..., need_weight = !is.null(weight))
    
    if(!is_list(row_vars)){
        row_vars = add_missing_var_lab(row_vars, str_row_vars)
        row_vars = list(row_vars)
    }
    if(!is_list(col_vars)){
        col_vars = add_missing_var_lab(col_vars, str_col_vars)
        col_vars = list(col_vars)
    }
    row_vars = flat_list(dichotomy_to_category_encoding(row_vars), flat_df = FALSE)
    col_vars = flat_list(multiples_to_single_columns_with_dummy_encoding(col_vars), flat_df = TRUE)
    stopif(!is.null(subgroup) && !is.logical(subgroup), "'subgroup' should be logical.")
    
    
    if(!is_list(cell_vars)){
        cell_vars = add_missing_var_lab(cell_vars, str_cell_vars)
        if(!is.data.frame(cell_vars)){
            cell_vars = as.dtfrm(cell_vars)    
        }
        check_sizes("'cro_fun'", cell_vars, row_vars, col_vars, weight, subgroup)
    } else {
        cell_vars = flat_list(cell_vars, flat_df = TRUE)
        check_sizes("'cro_fun'", cell_vars, row_vars, col_vars, weight, subgroup)
        cell_vars = as.dtfrm(cell_vars)  
    }
    
    cell_vars = make_labels_from_names(cell_vars)
    
    
    
    res = lapply(row_vars, function(each_row_var){
        all_col_vars = lapply(col_vars, function(each_col_var){
            elementary_cro_fun_df(cell_var = cell_vars,
                                  row_var = each_row_var, 
                                  col_var = each_col_var, 
                                  weight = weight,
                                  subgroup = subgroup,
                                  prepend_var_lab = TRUE,
                                  fun = fun
            )    
        })
        Reduce(merge, all_col_vars)
    })
    
    res = do.call(add_rows, res)
    res
}


### compute statistics for single row_var and single col_var
elementary_cro_fun_df = function(cell_var, 
                                 col_var, 
                                 weight,
                                 fun,
                                 row_var, 
                          prepend_var_lab,
                          subgroup
                          ){
    
    ### calculate vector of valid cases

    valid = valid(col_var) & valid(row_var)
    if(!is.null(subgroup)) {
        valid = valid & subgroup
    }
    max_nrow = max(NROW(cell_var), NROW(col_var), NROW(row_var))
    
    ## if any of vars is zero-length then we made all vars zero-length
    min_nrow = min(NROW(cell_var), NROW(col_var), NROW(row_var))
    if(any(min_nrow==0)) max_nrow = 0
    
    if(!is.null(weight)){
        weight = set_negative_and_na_to_zero(weight)
        if(length(weight)==1) weight = rep(weight, max_nrow)
        valid = valid & (weight>0)
        weight = weight[valid]
    }
    

    ### recycle variables of length 1

    if(NROW(cell_var)==1){
        if(is.matrix(cell_var) || is.data.frame(cell_var)){
            cell_var =  cell_var[rep(1, max_nrow), ]
        } else {
            cell_var = rep(cell_var, max_nrow)
        }
    }
    
    if(length(col_var)==1) col_var = rep(col_var, max_nrow)

    if(NROW(row_var)==1){
        if(is.matrix(row_var) || is.data.frame(row_var)){
            row_var =  row_var[rep(1, max_nrow), ]
        } else {
            row_var = rep(row_var, max_nrow)
        }
    }
    
    ### drop non-valid cases 
    
    if(is.matrix(cell_var) || is.data.frame(cell_var)){
        cell_var =  cell_var[valid, , drop = FALSE]
    } else {
        cell_var = cell_var[valid]
    }
    col_var = col_var[valid]
    row_var = convert_multicolumn_object_to_vector(row_var)
    row_var = row_var[valid]
    
    ########

    row_var_lab = var_lab(row_var)
    col_var_lab = var_lab(col_var)
    
    ### pack data.table #####
    
    if(is.null(weight)){
        raw_data = data.table(..row_var__ = row_var, ..col_var__ = col_var, cell_var)
    } else {
        raw_data = data.table(..row_var__ = row_var, ..col_var__ = col_var, ..weight__ = weight, cell_var)
    }
    raw_data = raw_data[!is.na(row_var), ]
    
    
    # statistics
    by_string = "..row_var__,..col_var__"
    if(is.null(weight)){
        dtable = raw_data[ , fun(.SD), by = by_string]
    } else {
        dtable = raw_data[ , fun(.SD, weight = ..weight__), by = by_string, .SDcols = -"..weight__"]
    }
    
    
    ### make rectangular table  
    res = long_datatable_to_table(dtable, rows = c("..row_var__", "row_labels"), 
                                  columns = "..col_var__", 
                                  value = colnames(dtable) %d% c("..row_var__", "row_labels", "..col_var__")
                                  )
    

    res[ , row_labels  := as.character(row_labels)] 
    res[ , row_labels  := paste0(..row_var__, "|", row_labels)]  
    res[["..row_var__"]] = NULL
    
    if(prepend_var_lab){
        res[, row_labels := paste0(row_var_lab, "|", row_labels)]
        colnames(res)[-1] = paste0(col_var_lab, "|", colnames(res)[-1]) 
    }
    
    
    res[ , row_labels := remove_unnecessary_splitters(row_labels)] 
    res[ , row_labels := make_items_unique(row_labels)] 
    colnames(res) = remove_unnecessary_splitters(colnames(res)) 
    res = as.dtfrm(res)
    class(res) = union("etable", class(res))
    res
}

make_function_for_cro_df = function(fun, ..., need_weight = TRUE){
    force(fun)
    force(need_weight)
    if(need_weight){
        function(x, weight = weight){
            res = fun(x, ..., weight = weight)
            res = make_dataframe_with_row_labels(res)
            # we need convert to factor to keep order of row_labels
            as.list(res[, row_labels := factor(row_labels, levels = unique(row_labels))])
        }
    } else {
        function(x){
            res = fun(x, ...)
            res = make_dataframe_with_row_labels(res)
            # we need convert to factor to keep order of row_labels
            as.list(res[, row_labels := factor(row_labels, levels = unique(row_labels))])
        }        
    }
}

make_function_for_cro = function(fun, ..., need_weight = TRUE){
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
            res = rbindlist(res, use.names = TRUE, fill = TRUE)
            # we need convert to factor to keep order of row_labels
            as.list(res[, row_labels := factor(row_labels, levels = unique(row_labels))])
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
            res = rbindlist(res, use.names = TRUE, fill = TRUE)
            # we need convert to factor to keep order of row_labels
            as.list(res[, row_labels := factor(row_labels, levels = unique(row_labels))])
        }        
    }
}

#############
make_dataframe_with_row_labels = function(res){
    if(is.table(res)){
        dm_names = dimnames(res)
        if(is.null(dm_names)){
            dm_names[[1]] = names(res)
        }
        new_df = matrix(NA, nrow= NROW(res), ncol = NCOL(res))
        new_df[] = res
        new_df = as.dtfrm(new_df)
        rownames(new_df) = NULL
        
        row_labels = dm_names[[1]]
        if(length(dm_names)>1){
            new_df = setNames(new_df, dm_names[[2]])
        }
        res = new_df
    } else {
        if(is.matrix(res) || is_list(res)) res = as.dtfrm(res)
        if(is.data.frame(res)) {
            if("row_labels" %in% colnames(res)){
                row_labels = res[["row_labels"]]    
                res[["row_labels"]] = NULL
            } else {
                row_labels = rownames(res)
                if(!is.null(row_labels) && length(row_labels)==1 && row_labels[1]==1){
                    row_labels = ""
                }
            }
        } else {
            row_labels = names(res)
            res = setNames(dtfrm(res), "|")
        } 
    }
    if(is.null(row_labels)){
        if(nrow(res)>1){
            row_labels = seq_len(nrow(res)) 
        } else {
            row_labels = rep("", nrow(res)) # for empty results   
        }
    } 
    row_labels = make_items_unique(as.character(row_labels))
    data.table(row_labels = row_labels, res)

}

######################## 

add_missing_var_lab = function(x, str_lab){
    if(!is.matrix(x) && !is.data.frame(x) && !is.list(x)){
        if(is.null(var_lab(x))){
            var_lab(x) = str_lab
        } 
    }
    x
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
                      ...){
    str_cell_vars = deparse(substitute(cell_vars))
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    
    stopif(is.null(cell_vars), 
           paste0("'", str_cell_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    
    fun = match.fun(fun)
    if(!is.null(weight)){
        stopif(!("weight" %in% names(formals(fun))),
               "`weight` is provided but `fun` doesn't have formal `weight` argument.")
    }
    fun = make_function_for_cro_df(fun, ..., need_weight = !is.null(weight))
    
    if(!is_list(cell_vars)){
        cell_vars = add_missing_var_lab(cell_vars, str_cell_vars)
        cell_vars = list(cell_vars)
    }
    cell_vars = make_labels_from_names(cell_vars)
    if(!is_list(row_vars)){
        row_vars = add_missing_var_lab(row_vars, str_row_vars)
        row_vars = list(row_vars)
    }
    if(!is_list(col_vars)){
        col_vars = add_missing_var_lab(col_vars, str_col_vars)
        col_vars = list(col_vars)
    }
    cell_vars = flat_list(cell_vars, flat_df = FALSE)
    row_vars = flat_list(dichotomy_to_category_encoding(row_vars), flat_df = FALSE)
    col_vars = flat_list(multiples_to_single_columns_with_dummy_encoding(col_vars), flat_df = TRUE)
    stopif(!is.null(subgroup) && !is.logical(subgroup), "'subgroup' should be logical.")
    check_sizes("'cro_fun_df'", cell_vars, row_vars, col_vars, weight, subgroup)
    
    res = lapply(row_vars, function(each_row_var){
        all_cell_vars = lapply(cell_vars, function(each_cell_var){
            all_col_vars = lapply(col_vars, function(each_col_var){
                elementary_cro_fun_df(cell_var = names2labels(each_cell_var),
                                      row_var = each_row_var, 
                                      col_var = each_col_var, 
                                      weight = weight,
                                      subgroup = subgroup,
                                      prepend_var_lab = TRUE,
                                      fun = fun
                )    
            })
            Reduce(merge, all_col_vars)
        })
        do.call(add_rows, all_cell_vars) 
        
    })
    res = do.call(add_rows, res)
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
    
    fun = function(x, weight = NULL){
        res = vapply(x, FUN = w_mean, FUN.VALUE = numeric(1), weight = weight, USE.NAMES = FALSE)
        list(row_labels = names(x), "|" = res)
    }
    
    str_cell_vars = deparse(substitute(cell_vars))
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    
    stopif(is.null(cell_vars), 
           paste0("'", str_cell_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    
    if(!is_list(cell_vars)){
        cell_vars = add_missing_var_lab(cell_vars, str_cell_vars)
        cell_vars = list(cell_vars)
    }    
    if(!is_list(row_vars)){
        row_vars = add_missing_var_lab(row_vars, str_row_vars)
        row_vars = list(row_vars)
    }
    if(!is_list(col_vars)){
        col_vars = add_missing_var_lab(col_vars, str_col_vars)
        col_vars = list(col_vars)
    }
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
cro_sum = function(cell_vars, 
                   col_vars = total(), 
                   row_vars = total(label = ""),
                   weight = NULL,
                   subgroup = NULL
){
    fun = function(x, weight = NULL){
        res = vapply(x, FUN = w_sum, FUN.VALUE = numeric(1), weight = weight, USE.NAMES = FALSE)
        list(row_labels = names(x), "|" = res)
    }
    str_cell_vars = deparse(substitute(cell_vars))
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    
    stopif(is.null(cell_vars), 
           paste0("'", str_cell_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    
    if(!is_list(cell_vars)){
        cell_vars = add_missing_var_lab(cell_vars, str_cell_vars)
        cell_vars = list(cell_vars)
    }    
    if(!is_list(row_vars)){
        row_vars = add_missing_var_lab(row_vars, str_row_vars)
        row_vars = list(row_vars)
    }
    if(!is_list(col_vars)){
        col_vars = add_missing_var_lab(col_vars, str_col_vars)
        col_vars = list(col_vars)
    }
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
cro_median = function(cell_vars, 
                      col_vars = total(), 
                      row_vars = total(label = ""),
                      weight = NULL,
                      subgroup = NULL
){
    fun = function(x, weight = NULL){
        res = vapply(x, FUN = w_median, FUN.VALUE = numeric(1), weight = weight, USE.NAMES = FALSE)
        list(row_labels = names(x), "|" = res)
    }
    str_cell_vars = deparse(substitute(cell_vars))
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    
    stopif(is.null(cell_vars), 
           paste0("'", str_cell_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    
    if(!is_list(cell_vars)){
        cell_vars = add_missing_var_lab(cell_vars, str_cell_vars)
        cell_vars = list(cell_vars)
    }    
    if(!is_list(row_vars)){
        row_vars = add_missing_var_lab(row_vars, str_row_vars)
        row_vars = list(row_vars)
    }
    if(!is_list(col_vars)){
        col_vars = add_missing_var_lab(col_vars, str_col_vars)
        col_vars = list(col_vars)
    }
    cro_fun_df(cell_vars = cell_vars, 
               col_vars = col_vars, 
               row_vars = row_vars, 
               weight = weight,
               subgroup = subgroup,
               fun = fun
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
    str_cell_vars = deparse(substitute(cell_vars))
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    
    stopif(is.null(cell_vars), 
           paste0("'", str_cell_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    
    if(!is_list(cell_vars)){
        cell_vars = add_missing_var_lab(cell_vars, str_cell_vars)
        cell_vars = list(cell_vars)
    }    
    if(!is_list(row_vars)){
        row_vars = add_missing_var_lab(row_vars, str_row_vars)
        row_vars = list(row_vars)
    }
    if(!is_list(col_vars)){
        col_vars = add_missing_var_lab(col_vars, str_col_vars)
        col_vars = list(col_vars)
    }
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
    str_cell_vars = deparse(substitute(cell_vars))
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    
    stopif(is.null(cell_vars), 
           paste0("'", str_cell_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    
    if(!is_list(cell_vars)){
        cell_vars = add_missing_var_lab(cell_vars, str_cell_vars)
        cell_vars = list(cell_vars)
    }    
    if(!is_list(row_vars)){
        row_vars = add_missing_var_lab(row_vars, str_row_vars)
        row_vars = list(row_vars)
    }
    if(!is_list(col_vars)){
        col_vars = add_missing_var_lab(col_vars, str_col_vars)
        col_vars = list(col_vars)
    }
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
total = function(x = 1, label = "#Total"){
    res = valid(x)
    res[!res] = NA
    res = as.numeric(res)
    var_lab(res) = ""
    val_lab(res) = setNames(1, label)
    res
}

#' @export
#' @rdname cro_fun
combine_functions = function(..., method = c){
    method = match.fun(method)
    possible_names = unlist(lapply(as.list(substitute(list(...)))[-1], as.character))
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
        }
        res = lapply(args, function(f) f(x))
        do.call(method, res)
    }
}


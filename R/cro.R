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
#' @param prepend_var_lab logical. Should we prepend variable label before value
#'   labels? By default it is TRUE.
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
cro = function(row_vars, 
               col_vars = "#Total", 
               weight = NULL, 
               subgroup = NULL,
               total_title = "#Total",
               total = "unweighted",
               total_row_position = c("below", "above", "none"),
               prepend_var_lab = TRUE){
    
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    multi_cro(row_vars = row_vars, 
              col_vars = col_vars, 
              weight = weight,
              total_title = total_title,
              total = total,
              total_row_position = total_row_position,
              subgroup = subgroup,
              prepend_var_lab = prepend_var_lab,
              stat_type = "count"
    )    
}




#' @export
#' @rdname cro
cro_cpct = function(row_vars, 
                    col_vars = "#Total", 
                    weight = NULL, 
                    subgroup = NULL,
                    total_title = "#Total",
                    total = "unweighted",
                    total_row_position = c("below", "above", "none"),
                    prepend_var_lab = TRUE){
    
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    multi_cro(row_vars = row_vars, 
              col_vars = col_vars, 
              weight = weight,
              total_title = total_title,
              total = total,
              total_row_position = total_row_position,
              subgroup = subgroup,
              prepend_var_lab = prepend_var_lab,
              stat_type = "cpct"
    )    
}

#' @export
#' @rdname cro
cro_rpct = function(row_vars, 
                    col_vars = "#Total", 
                    weight = NULL, 
                    subgroup = NULL,
                    total_title = "#Total",
                    total = "unweighted",
                    total_row_position = c("below", "above", "none"),
                    prepend_var_lab = TRUE){
    
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    multi_cro(row_vars = row_vars, 
              col_vars = col_vars, 
              weight = weight,
              total_title = total_title,
              total = total,
              total_row_position = total_row_position,
              subgroup = subgroup,
              prepend_var_lab = prepend_var_lab,
              stat_type = "rpct"
    )    
}


#' @export
#' @rdname cro
cro_tpct = function(row_vars, 
                    col_vars = "#Total", 
                    weight = NULL, 
                    subgroup = NULL,
                    total_title = "#Total",
                    total = "unweighted",
                    total_row_position = c("below", "above", "none"),
                    prepend_var_lab = TRUE){
    
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    multi_cro(row_vars = row_vars, 
              col_vars = col_vars, 
              weight = weight,
              total_title = total_title,
              total = total,
              total_row_position = total_row_position,
              subgroup = subgroup,
              prepend_var_lab = prepend_var_lab,
              stat_type = "tpct"
    )    
}

#' @export
#' @rdname cro
cro_cpct_responses = function(row_vars, 
                              col_vars = "#Total", 
                              weight = NULL, 
                              subgroup = NULL,
                              total_title = "#Total",
                              total = "unweighted",
                              total_row_position = c("below", "above", "none"),
                              prepend_var_lab = TRUE){
    
    str_row_vars = deparse(substitute(row_vars))
    str_col_vars = deparse(substitute(col_vars))
    stopif(is.null(row_vars), 
           paste0("'", str_row_vars,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(col_vars), 
           paste0("'", str_col_vars,"' is NULL. Possibly variable doesn't exist."))
    multi_cro(row_vars = row_vars, 
              col_vars = col_vars, 
              weight = weight,
              total_title = total_title,
              total = total,
              total_row_position = total_row_position,
              subgroup = subgroup,
              prepend_var_lab = prepend_var_lab,
              stat_type = "cpct_responses"
    )    
}


### compute statistics for single row_var and single col_var
elementary_cro = function(row_var, col_var, weight = NULL, 
                          total_title = "#Total",
                          total = "unweighted",
                          total_row_position = c("below", "above", "none"),
                          prepend_var_lab = FALSE,
                          subgroup = NULL,
                          stat_type = c("count", "cpct", "cpct_responses", "rpct", "tpct")
                          ){
    
    ### preparations
    total_row_position = match.arg(total_row_position)
    total = match.arg(total, c("unweighted", "weighted"), several.ok = TRUE)
    stat_type = match.arg(stat_type)
    max_nrow = max(NROW(row_var), NROW(col_var))
    
    weight = if_null(weight, 1)
    weight = set_negative_and_na_to_zero(weight)
    if(length(weight)==1) weight = rep(weight, max_nrow)
    
    valid = valid(row_var) & valid(col_var) & (weight>0)
    
    if(!is.null(subgroup)) {
        valid = valid & subgroup
    }
    
    
    
    weight = weight[valid]
    
    if(length(col_var)==1) col_var = rep(col_var, max_nrow)
    col_var = col_var[valid]
    
    if(NROW(row_var)==1){
        if(is.matrix(row_var) || is.data.frame(row_var)){
            row_var =  row_var[rep(1, max_nrow), ]
        } else {
            row_var = rep(row_var, max_nrow)
        }
    }
    
    row_var = convert_multicolumn_object_to_vector(row_var)
    row_var = row_var[valid]

    row_var_lab = var_lab(row_var)
    col_var_lab = var_lab(col_var)
    
    raw_data = data.table(row_var = row_var, col_var = col_var, weight = weight)
    raw_data = raw_data[!is.na(row_var), ]
    
    # statistics
    
    dtable = raw_data[, list(value = sum(weight, na.rm = TRUE)), by = "col_var,row_var"]
    
    dtotal = data.table(col_var = col_var, weight = weight)
    dtotal = dtotal[, list(weighted_total = sum(weight, na.rm = TRUE), 
                               total = .N), by = "col_var"]

    if(stat_type=="cpct" || stat_type=="cpct_responses"){
        dtable = dtotal[dtable, on = "col_var", nomatch = NA]
        dtable[, value := value/weighted_total*100]
        if(stat_type == "cpct_responses"){
            dtable[, value := value/sum(value, na.rm = TRUE)*100, by = "col_var"]
        }
    }
    if(stat_type=="tpct"){
        dtable = dtotal[dtable, on = "col_var", nomatch = NA]
        dtable[, value := value/sum(weight, na.rm = TRUE)*100]
    }
    if(stat_type=="rpct"){
        row_total = raw_data[, list(weighted_total = sum(weight, na.rm = TRUE)), by = "row_var"]
        dtable = row_total[dtable, on = "row_var", nomatch = NA]
        dtable[, value := value/weighted_total*100]
    }

    ### make rectangular table  
    res = long_datatable_to_table(dtable, rows = "row_var", columns = "col_var", value = "value")
    colnames(res)[1] = "row_labels"
    
    if(total_row_position!="none"){
        res = add_total_to_table(
            res = res, 
            dtotal = dtotal,
            total_row_position = total_row_position,
            total = total,
            total_title = total_title
        )    
    }    

    rownames(res) = NULL
    
    if(prepend_var_lab){
        res[[1]] = paste0(row_var_lab, "|", res[[1]])
        colnames(res)[-1] = paste0(col_var_lab, "|", colnames(res)[-1]) 
    }
    
    
    res[[1]] = remove_unnecessary_splitters(res[[1]]) 
    res[[1]] = make_items_unique(res[[1]])
    colnames(res) = remove_unnecessary_splitters(colnames(res)) 
    class(res) = union("etable", class(res))
    res
}

###########################

add_total_to_table = function(res, dtotal, total_row_position, total, total_title){
    if(length(total)==0) total = "unweighted"
    if(length(total_title) == 0) total_title = "#Total"
    if(length(total_title) < length(total)) total_title = rep(total_title, length(total))
   
    total =  c("unweighted" = "total", "weighted" = "weighted_total")[total]
    total_row = lapply(seq_along(total), function(item){
        dtotal[, title := ""]  
        row = long_datatable_to_table(dtotal, 
                                rows = "title", 
                                columns = "col_var", 
                                value = total[item])
        colnames(row)[1] = "row_labels"
        row[[1]] = add_first_symbol_to_total_title(total_title[item])
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

multi_cro = function(row_vars, 
               col_vars = "#Total", 
               weight = NULL, 
               subgroup = NULL,
               total_title = "#Total",
               total = "unweighted",
               total_row_position = c("below", "above", "none"),
               prepend_var_lab = TRUE,
               stat_type){
    if(!is_list(row_vars)){
        row_vars = list(row_vars)
    }
    if(!is_list(col_vars)){
        col_vars = list(col_vars)
    }
    row_vars = flat_list(dichotomy_to_category_encoding(row_vars), flat_df = FALSE)
    col_vars = flat_list(multiples_to_single_columns_with_dummy_encoding(col_vars), flat_df = TRUE)
    stopif(!is.null(subgroup) && !is.logical(subgroup), "'subgroup' should be logical.")
    check_sizes("'cro'", row_vars, col_vars, weight, subgroup)
    res = lapply(row_vars, function(each_row_var){
        all_col_vars = lapply(col_vars, function(each_col_var){
            elementary_cro(row_var = each_row_var, 
                           col_var = each_col_var, 
                           weight = weight,
                           total_title = total_title,
                           total = total,
                           total_row_position = total_row_position,
                           subgroup = subgroup,
                           prepend_var_lab = prepend_var_lab,
                           stat_type = stat_type
            )    
        })
        Reduce(merge, all_col_vars)
        
    })
    res = do.call(add_rows, res)
    res
}

######################################################


#' @export
#' @rdname cro
cro_mean = function(x, predictor, weight = NULL){
    str_x = deparse(substitute(x))
    str_predictor = deparse(substitute(predictor))
    x = check_cro_arguments(x = x, 
                        str_x = str_x, 
                        predictor = predictor, 
                        str_predictor = str_predictor, 
                        weight = weight, 
                        fun = NULL)

    na_if(x) = is.na(predictor)
    cro_fun(x = x, predictor = predictor, weight = weight, fun = w_mean)
}

#' @export
#' @rdname cro
cro_sum = function(x, predictor, weight = NULL){
    str_x = deparse(substitute(x))
    str_predictor = deparse(substitute(predictor))
    x = check_cro_arguments(x = x, 
                        str_x = str_x, 
                        predictor = predictor, 
                        str_predictor = str_predictor, 
                        weight = weight, 
                        fun = NULL)

    na_if(x) = is.na(predictor)
    cro_fun(x = x, predictor = predictor, weight = weight, fun = function(x, weight = NULL, na.rm){
        if(all(is.na(x))){
            NA
        } else {
            w_sum(x, weight = weight)    
        }   
    })
}

#' @export
#' @rdname cro
cro_median = function(x, predictor, weight = NULL){
    str_x = deparse(substitute(x))
    str_predictor = deparse(substitute(predictor))
    x = check_cro_arguments(x = x, 
                        str_x = str_x, 
                        predictor = predictor, 
                        str_predictor = str_predictor, 
                        weight = weight, 
                        fun = NULL)

    na_if(x) = is.na(predictor)
    cro_fun(x = x, predictor = predictor, weight = weight, fun = w_median)
}



#' @export
#' @rdname cro
cro_fun = function(x, predictor, fun, ..., weight = NULL){
    str_x = deparse(substitute(x))
    str_predictor = deparse(substitute(predictor))
    fun = match.fun(fun)
    x = check_cro_arguments(x = x, 
                        str_x = str_x, 
                        predictor = predictor, 
                        str_predictor = str_predictor, 
                        weight = weight, 
                        fun = fun)
    predictor = prepare_predictor(predictor, NROW(x))
    for(each in seq_along(x)){
        if(is.factor(x[[each]])) x[[each]] = as.labelled(x[[each]])
    }
    
    if (!is.null(weight)) {
        stopif(!("weight" %in% names(formals(fun))),
               "`weight` is provided but `fun` doesn't have formal `weight` argument.")
        # change negative and NA weights to 0 
        if(length(weight)==1) weight = rep(weight, NROW(x))
        if_val(weight) = list(lo %thru% 0 ~ 0, NA ~ 0)
        splitted_weight = split(weight, predictor, drop = TRUE)
        column_total = lapply(x, FUN = function(each) fun(each, weight = weight, ...))
        
    } else {
        column_total = lapply(x, FUN = fun, ...)
        
    }
    column_total = lapply(column_total, function(each) prepare_result(list("#Total" = each)))
    column_total = do.call(rbind, column_total)
    if (colnames(column_total)[1] == "#stat") column_total = column_total[,-1, drop = FALSE]
    labels = vapply(x, function(each) {
        varlab = var_lab(each)
        if(is.null(varlab)) varlab = NA_character_
        varlab
    }, 
    FUN.VALUE = character(1)
    )
    if_na(labels) = colnames(x)
    
    predictor = to_fac(unvr(predictor))
    if(is.null(weight)){
        result = lapply(x, function(col){
            res = lapply(split(col, predictor, drop = TRUE), FUN = fun, ...)
            prepare_result(res)
        })
    } else {
        result = lapply(x, function(col){
            splitted_col = split(col, predictor, drop = TRUE)
            res = lapply(seq_along(splitted_col), 
                         function(each) 
                             fun(splitted_col[[each]],
                                 weight = splitted_weight[[each]],
                                 ...)
            )
            names(res) = names(splitted_col)
            prepare_result(res)
        }) 
    }
    single_nrow = NROW(result[[1]])
    if (single_nrow>1) labels = rep(labels, each = single_nrow)
    res = do.call(rbind, result)
    res = data.frame(" " = labels, res, column_total, stringsAsFactors = FALSE, check.names = FALSE)
    class(res) = union("etable", class(res))
    rownames(res) = NULL
    res
    
}

### compute statistics for single row_var and single col_var
elementary_cro_fun_df = function(cell_var, col_var, weight = NULL,
                                 fun,
                                 row_var, 
                          prepend_var_lab = FALSE,
                          subgroup = NULL){
    
    ### calculate vector of valid cases

    valid = valid(col_var) & valid(row_var)
    if(!is.null(subgroup)) {
        valid = valid & subgroup
    }
    max_nrow = max(NROW(cell_var), NROW(col_var), NROW(row_var))
    
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
    # cell_var_lab = var_lab(cell_var)
    row_var_lab = var_lab(row_var)
    col_var_lab = var_lab(col_var)
    
    ### pack data.table #####
    cell_var = names2labels(cell_var)
    if(is.null(weight)){
        raw_data = data.table(..row_var__ = row_var, ..col_var__ = col_var, cell_var)
    } else {
        raw_data = data.table(..row_var__ = row_var, ..col_var__ = col_var, ..weight__ = weight, cell_var)
    }
    raw_data = raw_data[!is.na(row_var), ]
    
    
    # we need at least one row because with zero rows all rows from 'fun' will be ignored
    # if(nrow(raw_data)==0){
    #     raw_data = rbind(raw_data, data.table(..row_var__ = NA), fill = TRUE, use.names = TRUE)
    # } 
    
    # statistics
    by_string = "..row_var__,..col_var__"

    if(is.null(weight)){
        dtable = raw_data[ , fun(.SD), by = by_string]
    } else {
        dtable = raw_data[ , fun(.SD[,-"..weight__"], weight = ..weight__), by = by_string]
    }
    
    ### make rectangular table  
    res = long_datatable_to_table(dtable, rows = c("..row_var__", "row_labels"), 
                                  columns = "..col_var__", 
                                  value = colnames(dtable) %d% c("..row_var__", "row_labels", "..col_var__")
                                  )
    res[["row_labels"]] = as.character(res[["row_labels"]])
    res[["row_labels"]] = paste0(res[["..row_var__"]], "|", res[["row_labels"]])
    res[["..row_var__"]] = NULL
    

    if(prepend_var_lab){
        res[["row_labels"]] = paste0(row_var_lab, "|", res[[1]])
        colnames(res)[-1] = paste0(col_var_lab, "|", colnames(res)[-1]) 
    }
    
    
    res[["row_labels"]] = remove_unnecessary_splitters(res[["row_labels"]]) 
    res[["row_labels"]] = make_items_unique(res[[1]])
    colnames(res) = remove_unnecessary_splitters(colnames(res)) 
    class(res) = union("etable", class(res))
    res
}

make_function_for_cro_df = function(fun, ..., need_weight = TRUE){
    force(fun)
    force(need_weight)
    if(need_weight){
        function(x, ..., weight = weight){
            res = fun(x, ..., weight = weight)
            make_dataframe_with_row_labels(res)
        }
    } else {
        function(x, ...){
            res = fun(x, ...)
            make_dataframe_with_row_labels(res)
        }        
    }
}

#############
make_dataframe_with_row_labels = function(res){
    if(is.table(res)){
        dm_names = dimnames(res)
        new_df = matrix(NA, nrow= NROW(res), ncol = NCOL(res))
        new_df[] = res
        new_df = as.dtfrm(new_df)
        rownames(new_df) = NULL
        
        row_labels = dm_names[[1]]
        if(length(dm_names)>1){
            new_df = setNames(new_df, dm_names[[2]])
        }
        res = new_df
    }
    if(is.matrix(res) || is_list(res)) res = as.dtfrm(res)
    if(is.data.frame(res)) {
        if("row_labels" %in% colnames(res)){
            row_labels = res[["row_labels"]]    
            res[["row_labels"]] = NULL
        } else {
            row_labels = rownames(res)
        }
    } else {
        row_labels = names(res)
        res = setNames(dtfrm(res), "|")
    } 
    
    if(is.null(row_labels)){
        if(nrow(res)>1){
            row_labels = seq_len(nrow(res)) 
        } else {
            row_labels = rep("", nrow(res)) # for empty results   
        }
    } 
    row_labels = make_items_unique(as.character(row_labels))
    # we need convert to factor to keep order of row_labels
    row_labels = factor(row_labels, levels = row_labels)
    dtfrm(row_labels = row_labels, res)
}

######################## 

add_missing_var_lab = function(x, str_lab){
    if(!is.matrix(x) && !is.data.frame(x) && !is.list(x)){
        if(is.null(var_lab(x)) || var_lab(x)==""){
            var_lab(x) = str_lab
        } 
    }
    x
}


#######

#' @export
#' @rdname cro
cro_fun_df = function(cell_vars, 
                      col_vars = "#Total", 
                      row_vars = "", 
                      weight = NULL,
                      subgroup = NULL,
                      fun, ...,
                      prepend_var_lab = TRUE){
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
        # row_vars = add_missing_var_lab(row_vars, str_row_vars)
        row_vars = list(row_vars)
    }
    if(!is_list(col_vars)){
        # col_vars = add_missing_var_lab(col_vars, str_col_vars)
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
                elementary_cro_fun_df(cell_var = each_cell_var,
                                      row_var = each_row_var, 
                                      col_var = each_col_var, 
                                      weight = weight,
                                      subgroup = subgroup,
                                      prepend_var_lab = prepend_var_lab,
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





prepare_dataframe = function(x, possible_name){
    if (is.matrix(x)) {
        varlab0 = var_lab(x)
        x = as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
        if(!is.null(varlab0)) colnames(x) = paste(varlab0, LABELS_SEP, colnames(x))
    } else {
        if (is.list(x)){
            if(is.null(names(x))){
                names(x) = paste0("V", seq_along(x))
            }
            x = as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE) 
            
        } else {
            
            x = as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE) 
            colnames(x) = possible_name    
        }
    }
    x
}


check_cro_arguments = function(x, str_x, predictor, str_predictor, weight, fun = NULL){
    stopif(is.null(x), 
           paste0("'", str_x,"' is NULL. Possibly variable doesn't exist."))
    stopif(is.null(predictor), 
           paste0("'", str_predictor,"' is NULL. Possibly variable doesn't exist."))
    if(!is.data.frame(x)){
        x = prepare_dataframe(x, str_x)
    }
    stopif(NCOL(predictor)>1, "'predictor' should have only one column.")
    stopif(NROW(x)!=NROW(predictor) & NROW(predictor)!=1, 
           "'predictor' should have the same number of rows as 'x' or length 1.")
    stopif(!is.null(weight) && (NROW(x)!=length(weight)) && (length(weight)!=1), 
           "'weight' should have the same number of rows as 'x' or length 1.")
    stopif(!is.null(fun) && !is.null(weight) &&  !("weight" %in% names(formals(fun))),
           "`weight` is provided but `fun` doesn't have formal `weight` argument.")
    x
    
}


# used only in cro_fun
prepare_result = function(list_of_results){
    nrows = unique(vapply(list_of_results, FUN = NROW, FUN.VALUE = numeric(1)))
    
    ncols = unique(vapply(list_of_results, FUN = NCOL, FUN.VALUE = numeric(1)))
    stopif(length(nrows)!=1,
           "Different number of rows of 'fun' result. 'fun' should always return result with same number of rows.")
    stopif(length(ncols)!=1,
           "Different number of columns of 'fun' result. 'fun' should always return result with same number of columns.")
    if(is.matrix(list_of_results[[1]]) || is.data.frame(list_of_results[[1]])) {
        possible_rownames = rownames(list_of_results[[1]])
    } else {
        possible_rownames = names(list_of_results[[1]])
    }  
    if(is.matrix(list_of_results[[1]]) || is.data.frame(list_of_results[[1]])) {
        possible_colnames = colnames(list_of_results[[1]])
    } else {
        possible_colnames = NULL
    } 
    cln = names(list_of_results)
    if(ncols>1) cln = rep(cln, each = ncols)
    if(!is.null(possible_colnames)) cln = paste(cln, possible_colnames, sep = LABELS_SEP)
    res = do.call(cbind, list_of_results)
    colnames(res) = cln 
    if(!is.null(possible_rownames)) {
        res = data.frame("#stat" = possible_rownames, res, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
        res = data.frame(res, stringsAsFactors = FALSE, check.names = FALSE)
    }    
    res
}

prepare_predictor = function(predictor, nrows){
    if(is.matrix(predictor) || is.data.frame(predictor)){
        varlab = var_lab(predictor)
        vallab = val_lab(predictor)
        predictor = c(predictor, recursive = TRUE)
        var_lab(predictor) = varlab
        val_lab(predictor) = vallab
    }
    if(NROW(predictor)==1) {
            predictor = rep(predictor, nrows)
    }
    predictor 
}


###############################################
###############################################
###############################################

#' @export
#' @rdname cro
table_pearson = function(cell_vars, 
                         col_vars = "#Total", 
                         row_vars = "", 
                         weight = NULL,
                         subgroup = NULL
){
    cor_fun = function(x, weight = NULL){
        w_pearson(x, weight = weight)[ , 1]
    }
    cro_fun_df(cell_vars = cell_vars, 
               col_vars = col_vars, 
               row_vars = row_vars, 
               weight = weight,
               subgroup = subgroup,
               fun = cor_fun
    )
}

#' @export
#' @rdname cro
table_spearman = function(cell_vars, 
                          col_vars = "#Total", 
                          row_vars = "", 
                          weight = NULL,
                          subgroup = NULL
){
    cor_fun = function(x, weight = NULL){
        w_spearman(x, weight = weight)[ , 1]
    } 
    cro_fun_df(cell_vars = cell_vars, 
               col_vars = col_vars, 
               row_vars = row_vars, 
               weight = weight,
               subgroup = subgroup,
               fun = cor_fun
    )
}
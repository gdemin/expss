#' Simple frequencies and crosstabs with support of labels, weights and multiple response variables.
#' 
#' \itemize{
#' \item{\code{fre}}{ returns data.frame with six columns: labels or values, counts, 
#' valid percent (excluding NA), percent (with NA), percent of responses(for 
#' single-column \code{x} it equals to valid percent) and cumulative percent of 
#' responses.}
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
#' @param total_row_position sdsdds
#' @param total_row_title sdsdd
#' @param weighted_total dssds
#' @param drop_unused_labels logical. Should we drop unused value labels?
#'   Default is TRUE.
#' @param prepend_var_lab logical. Should we prepend variable label before value
#'   labels? By default we will add variable labels to value labels only if
#'   \code{x} or predictor is list (several variables).
#' @param fun custom summary function. It should always return
#'   scalar/vector/matrix of the same size.
#' @param ... further arguments for \code{fun}   
#'
#' @return object of class 'etable'. Basically it's a data.frame but class
#'   is needed for custom methods.
#'
#' @examples
#' data(mtcars)
#' mtcars = modify(mtcars,{
#'     var_lab(vs) = "Engine"
#'     val_lab(vs) = c("V-engine" = 0, 
#'                     "Straight engine" = 1) 
#'     var_lab(am) = "Transmission"
#'     val_lab(am) = c(automatic = 0, 
#'                     manual=1)
#' })
#' 
#' fre(mtcars$vs)
#' with(mtcars, cro(am, vs))
#' with(mtcars, cro_cpct(am, vs))
#' 
#' # multiple-choiсe variable
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
#' fre(brands)
#' cro(brands, score)
#' cro_cpct(brands, score)
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
fre = function(x, weight = NULL, drop_unused_labels = TRUE, prepend_var_lab = FALSE){
    UseMethod("fre")
}

#' @export
fre.list = function(x, weight = NULL, drop_unused_labels = TRUE, prepend_var_lab = TRUE){
    x = flat_list(x, flat_df = FALSE)
    res = lapply(x, fre, 
                 weight = weight, 
                 drop_unused_labels = drop_unused_labels, 
                 prepend_var_lab = prepend_var_lab
                 )
    do.call(add_rows, res)
}

#' @export
fre.default = function(x, weight = NULL, drop_unused_labels = TRUE, prepend_var_lab = FALSE){
    str_x = deparse(substitute(x))
    if(is.null(x)){
        stop(paste0(str_x," is NULL. Possibly variable doesn't exist."))
    }
    
    check_sizes("fre", x, weight)
    
    if(is.dichotomy(x)){
        x = as.category(x)
    }

    weight = if_null(weight, 1)
    weight = set_negative_and_na_to_zero(weight)
    if(length(weight)==1) weight = rep(weight, NROW(x))
    
    
    valid = valid(x) # are there any not-NA in row?
    valid = valid & (weight>0)
    
    not_nas = sum(weight*(valid), na.rm = TRUE) 
    nas = sum(weight*(!valid), na.rm = TRUE) 
    
    x = convert_multicolumn_object_to_vector(x)
    
    x = x[valid]
    weight = weight[valid]
    
    varlab = var_lab(x)
    dtable = data.table(x = x, weight = weight)

    dtable = dtable[!is.na(x), ]
    dtable = dtable[, list(weight = sum(weight, na.rm = TRUE)), by = "x"]

    dtable[, count:=1]

    ### 'dcast' just for generating absent labels    
    res = long_datatable_to_table(dtable, rows = "x", columns = "count", value = "weight")
   
    colnames(res) = c("labels", "res")
    rownames(res) = NULL
    res = res[!is.na(res$res) | !drop_unused_labels, ]
    res[["labels"]] = as.character(res[["labels"]])
    # percent without missing
    if(not_nas>0) {
        res$valid_percent =  res$res/not_nas*100    
    } else {
        res$valid_percent = rep(0, nrow(res))
    }    
    # percent with missings
    base = not_nas + nas
    if(base>0) {
        res$percent =  res[[2]]/base*100    
    } else {
        res$percent =  rep(0, nrow(res))
    }  
    # response percent - differs from valid percent only for multiples
    r_base = sum(res$valid_percent, na.rm = TRUE)
    if(r_base>0) {
        res$rpercent =  res$valid_percent/r_base*100    
    } else {
        res$rpercent =  rep(0, nrow(res))
    }  
    total = sum(res[[2]], na.rm = TRUE)
    dfs_total = dtfrm(
        labels = "#Total",
        res = not_nas,
        valid_percent = ifelse(total>0, 100,0), 
        percent = ifelse(base>0, not_nas/base*100, 0),
        rpercent = ifelse(total>0, 100, 0),
        cum = NA
    )
    res$cum = cumsum(if_na(res$rpercent, 0))
    dfs_na = dtfrm(labels = "<NA>", 
                        res = nas, 
                        valid_percent = NA, 
                        percent = ifelse(base>0, nas/base*100, 0),
                        rpercent = NA,
                        cum = NA
    )
    res = rbind(res, dfs_total, dfs_na)
    rownames(res) = NULL
    
    if(prepend_var_lab){
        first_column_label = "" 
        res[[1]] = paste0(if_null(varlab, ""), "|", res[[1]])
    } else {
        first_column_label = if_null(varlab, str_x)
    }
    res[[1]] = remove_unnecessary_splitters(res[[1]]) 
    colnames(res) = c(first_column_label, 
                      "Count", "Valid percent", 
                      "Percent", "Responses, %",
                      "Cumulative responses, %")
    
    class(res) = union("etable", class(res))
    res
    
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
    
    weight = if_null(weight, 1)
    weight = set_negative_and_na_to_zero(weight)
    if(length(weight)==1) weight = rep(weight, NROW(row_var))
    
    valid = valid(row_var) & valid(col_var) & (weight>0)
    
    if(!is.null(subgroup)) {
        valid = valid & subgroup
    }
    
    max_nrow = max(NROW(row_var), NROW(col_var))
    
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
    check_sizes("cro", row_vars, col_vars, weight, subgroup)
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
#' @rdname fre
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
#' @rdname fre
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
#' @rdname fre
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
#' @rdname fre
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
#' @rdname fre
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




#' @export
#' @rdname fre
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
#' @rdname fre
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
#' @rdname fre
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
#' @rdname fre
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

#' @export
#' @rdname fre
cro_fun_df = function(x, predictor, fun, ..., weight = NULL){
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
    x = names2labels(x)
    if (!is.null(weight)) {
       
        # change negative and NA weights to 0 
        if(length(weight)==1) weight = rep(weight, NROW(x))
        if_val(weight) = list(lo %thru% 0 ~ 0, NA ~ 0)
        splitted_weight = split(weight, predictor, drop = TRUE)
        column_total = fun(x, weight = weight, ...)
        
    } else {
        column_total = fun(x, ...)
        
    }
    column_total = lapply(list(column_total), function(each) prepare_result(list("#Total" = each)))
    column_total = do.call(rbind, column_total)
    if (colnames(column_total)[1] == "#stat") column_total = column_total[,-1, drop = FALSE]
    predictor = to_fac(unvr(predictor))
    if(is.null(weight)){
        res = lapply(split(x, predictor, drop = TRUE), FUN = fun, ...)
        result = prepare_result(res)
        
    } else {
        
        splitted_x = split(x, predictor, drop = TRUE)
        res = lapply(seq_along(splitted_x),
                     function(each) 
                         fun(splitted_x[[each]],
                             weight = splitted_weight[[each]],
                             ...)
        )
        names(res) = names(splitted_x)
        result = prepare_result(res)
    }
    if_val(colnames(result)) = c('#stat' ~ ' ')
    res = data.frame(result, column_total, stringsAsFactors = FALSE, check.names = FALSE)
    rownames(res) = NULL
    class(res) = union("etable", class(res))
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
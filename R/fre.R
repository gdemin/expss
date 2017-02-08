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
#' @param predictor vector. By now multiple-response predictor is not supported.
#' @param weight numeric vector. Optional case weights. NA's and negative weights
#'   treated as zero weights.
#' @param fun custom summary function. It should always return
#'   scalar/vector/matrix of the same size.
#' @param ... further arguments for \code{fun}   
#'
#' @return object of class 'simple_table'/'summary_table'. Basically it's a data.frame but class
#'   is needed for custom print method.
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
#' # multiple-choise variable
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
fre = function(x, weight = NULL){
    if(is.null(x)){
        str_x = deparse(substitute(x))
        stop(paste0(str_x," is NULL. Possibly variable doesn't exist."))
    }
    raw = elementary_freq(x = x, weight = weight)
    not_nas = raw$not_nas
    nas = raw$nas
    res = raw$freq
    res = res[!is.na(res$res), ]
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
    total = sum_col(res[[2]])
    dfs_total = data.frame(
        labels = "#Total",
        res = not_nas,
        valid_percent = ifelse(total>0, 100,0), 
        percent = ifelse(base>0, not_nas/base*100, 0),
        rpercent = ifelse(total>0, 100, 0),
        cum = NA,
        stringsAsFactors = FALSE,
        check.names = FALSE
    )
    res$cum = cumsum(res$rpercent)
    dfs_na = data.frame(labels = "<NA>", 
                        res = nas, 
                        valid_percent = NA, 
                        percent = ifelse(base>0, nas/base*100, 0),
                        rpercent = NA,
                        cum = NA,
                        stringsAsFactors = FALSE,
                        check.names = FALSE
                        )
    res = rbind(res, dfs_total, dfs_na)
    rownames(res) = NULL
    
    varlab = var_lab(x)
    if (is.null(varlab)){
        varlab = deparse(substitute(x))
    }

    colnames(res) = c(varlab, "Count", "Valid percent", "Percent", "Responses, %", "Cumulative responses, %")
    
    class(res) = union("simple_table", class(res))
    res
    
}



elementary_freq = function(x, predictor = NULL, weight = NULL){
    stopif(!is.null(weight) && (NROW(x)!=length(weight)) && (length(weight)!=1), 
           "'weight' should have the same number of rows as 'x' or length 1.")
    stopif(NCOL(predictor)>1, "predictor should have only one column.")
    if (is.matrix(x)) {
        vallab0 = val_lab(x)
        varlab0 = var_lab(x)
        x = as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
        val_lab(x) = vallab0
        var_lab(x) = varlab0
    }
    if (is.null(weight)) {
        weight = rep(1, NROW(x))
    } else {
        if(length(weight)==1) weight = rep(weight, NROW(x))
        # change negative and NA weights to 0 
        if_val(weight) = list(lo %thru% 0 ~ 0, NA ~ 0)
    }    
    valid = valid(x) # are there not-NA in row?
    valid = valid & (weight>0)
    not_nas = sum(weight*(valid), na.rm = TRUE) # for fre
    nas = sum(weight*(!valid), na.rm = TRUE) # for fre
    if (!is.null(predictor)){
        stopif(NROW(x)!=length(predictor), "predictor should have the same number of rows as x.")
        valid = valid & valid(predictor)
        predictor = predictor[valid]
    }
    if(is.data.frame(x)){
        for(each in seq_along(x)){
            if(is.factor(x[[each]])) x[[each]] = as.labelled(x[[each]])
        }
        x = x[valid,]
    } else {
        if(is.factor(x)) x = as.labelled(x)
        x = x[valid]
    }
    weight = weight[valid]
    if(!is.null(predictor)){
        if(is.factor(predictor)) predictor = as.labelled(predictor)
        predictor = unvr(predictor)
        total = tapply(weight, list(f(predictor)), FUN = sum, na.rm = TRUE)
        predictor = rep(predictor, NCOL(x))
    } else {
        total = NULL
    }
    vallab = val_lab(x)
    x = unlab(x)
    weight = rep(weight, NCOL(x))
    x = c(x, recursive = TRUE)
    val_lab(x) = vallab
    
    if (is.null(predictor)){
        res = tapply(weight, list(f(x)), FUN = sum, na.rm = TRUE)
    } else {
        res = tapply(weight, list(f(x), f(predictor)), FUN = sum, na.rm = TRUE)
    }
    labels = rownames(res)
    if(is.null(labels)) labels = character(0)
    res = data.frame(labels = labels, res, stringsAsFactors = FALSE, check.names = FALSE)
    rownames(res) = NULL
    list(freq = res, not_nas = not_nas, nas = nas, total = total) 
    
}



#' @export
#' @rdname fre
cro = function(x, predictor, weight = NULL){
    str_x = deparse(substitute(x))
    str_predictor = deparse(substitute(predictor))
    check_cro_arguments(x = x, 
                        str_x = str_x, 
                        predictor = predictor, 
                        str_predictor = str_predictor, 
                        weight = weight, 
                        fun = NULL)
    predictor = prepare_predictor(predictor, NROW(x))
    raw = elementary_freq(x = x, predictor = predictor, weight = weight)
    res = raw$freq
    
    
    column_total = if_na(raw$total, 0) 
    res = res[, c(TRUE, column_total>0), drop = FALSE]
    column_total = column_total[column_total>0]

    column_total = data.frame(labels = "#Total", 
                              t(column_total), 
                              stringsAsFactors = FALSE,
                              check.names = FALSE
    )
    res = rbind(res, column_total)
    row_total = sum_row(res[,-1])
    res[,"#row_total"] = row_total
    res = res[(row_total>0) | (seq_len(NROW(res)) == NROW(res)),, drop = FALSE]
    
    varlab = var_lab(x)
    if (is.null(varlab)){
        varlab = deparse(substitute(x))
    }
    
    colnames(res)[1] = varlab
    colnames(res)[NCOL(res)] = "#Total"
    class(res) = union("simple_table", class(res))
    rownames(res) = NULL
    res
    
}

#' @export
#' @rdname fre
cro_cpct = function(x, predictor, weight = NULL){
    str_x = deparse(substitute(x))
    str_predictor = deparse(substitute(predictor))
    check_cro_arguments(x = x, 
                        str_x = str_x, 
                        predictor = predictor, 
                        str_predictor = str_predictor, 
                        weight = weight, 
                        fun = NULL)
    res = cro(x = x, predictor = predictor, weight = weight)
    last_row = NROW(res)
    if(NCOL(res)>2 & last_row>1){
        total_row = res[last_row, ]
        for (i in seq_along(res)[-1]){
            res[[i]][-last_row] = res[[i]][-last_row]/total_row[[i]]*100 
        }    
        
    }
    varlab = var_lab(x)
    if (is.null(varlab)){
        varlab = deparse(substitute(x))
    }
    colnames(res)[1] = varlab
    res
    
    
}

#' @export
#' @rdname fre
cro_rpct = function(x, predictor, weight = NULL){
    str_x = deparse(substitute(x))
    str_predictor = deparse(substitute(predictor))
    check_cro_arguments(x = x, 
                        str_x = str_x, 
                        predictor = predictor, 
                        str_predictor = str_predictor, 
                        weight = weight, 
                        fun = NULL)
    res = cro(x = x, predictor = predictor, weight = weight)
    last_col = NCOL(res)
    if(NROW(res)>1 & last_col>2){
        total_col = res[[last_col]]
        for (i in seq_len(last_col)[c(-1,-last_col)]){
            res[[i]] = res[[i]]/total_col*100 
        }    
        
    }
    varlab = var_lab(x)
    if (is.null(varlab)){
        varlab = deparse(substitute(x))
    }
    colnames(res)[1] = varlab
    res
    
    
}

#' @export
#' @rdname fre
cro_tpct = function(x, predictor, weight = NULL){
    str_x = deparse(substitute(x))
    str_predictor = deparse(substitute(predictor))
    check_cro_arguments(x = x, 
                        str_x = str_x, 
                        predictor = predictor, 
                        str_predictor = str_predictor, 
                        weight = weight, 
                        fun = NULL)
    res = cro(x = x, predictor = predictor, weight = weight)
    last_row = NROW(res)
    last_col = NCOL(res)
    if(last_col>2 & last_row>1){
        total = res[[last_col]][last_row]
        res[,-1] = res[,-1]/total*100 
        res[last_row, last_col] = total
    }
    varlab = var_lab(x)
    if (is.null(varlab)){
        varlab = deparse(substitute(x))
    }
    colnames(res)[1] = varlab
    res
    
    
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
    
    predictor = f(unvr(predictor))
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
    class(res) = union("summary_table", class(res))
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
    predictor = f(unvr(predictor))
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
    class(res) = union("summary_table", class(res))
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
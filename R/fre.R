#' Simple frequencies and crosstabs with support of labels, weights and multiple response variables.
#' 
#' \code{fre} returns data.frame with six columns: labels or values, counts, 
#' valid percent (excluding NA), percent (with NA), percent of responses(for 
#' single-column \code{x} it equals to valid percent) and cumulative percent of 
#' responses.
#' \code{cro} returns data.frame with counts (possibly weighted) with column and
#' row totals.
#' \code{cro_pct}, \code{cro_cpct}, \code{cro_rpct} return data.frame with 
#' table/column/row percent with column and row totals. There are always 
#' weighted counts instead of margin with 100\%. Empty labels/factor levels are 
#' removed from results of these functions. Base for multiple response (x is 
#' data.frame) percent is number of valid cases (not sum of responses) so sum of
#' percent may be greater than 100. Case is considered as valid if it has at
#' least one non-NA value.
#'
#' @param x vector/data.frame. data.frames are considered as multiple response
#'   variables.
#' @param predictor vector. By now multiple-response predictor is not supported.
#' @param weight numeric vector. Optional case weights. NA's and negative weights
#'   treated as zero weights.
#' @param fun custom summary function. It should return scalar.
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
#' @export
fre = function(x, weight = NULL){
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
    stopif(!is.null(weight) && (NROW(x)!=length(weight)), "weight should have the same number of rows as x.")
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
        # change negative and NA weights to 0 
        if_val(weight) = list(lo %thru% 0 ~ 0, NA ~ 0)
    }    
    valid = (NULL %in_row% x) # are there not-NA in row?
    valid = valid & (weight>0)
    not_nas = sum(weight*(valid), na.rm = TRUE) # for fre
    nas = sum(weight*(!valid), na.rm = TRUE) # for fre
    if (!is.null(predictor)){
        stopif(NROW(x)!=length(predictor), "predictor should have the same number of rows as x.")
        valid = valid & (NULL %in_row% predictor)
        predictor = predictor[valid]
    }
    if(is.data.frame(x)){
        for(each in seq_along(x)){
            if(is.factor(x[[each]])) x[[each]] = as.character(x[[each]])
        }
        x = x[valid,]
    } else {
        if(is.factor(x)) x = as.character(x)
        x = x[valid]
    }
    weight = weight[valid]
    if(!is.null(predictor)){
        if(is.factor(predictor)) predictor = as.character(predictor)
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
    row.names(res) = NULL
    list(freq = res, not_nas = not_nas, nas = nas, total = total) 
    
}



#' @export
#' @rdname fre
cro = function(x, predictor, weight = NULL){
    stopif(NROW(x)!=length(predictor), "predictor should have the same number of rows as x.")
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
    res
    
}

#' @export
#' @rdname fre
cro_cpct = function(x, predictor, weight = NULL){
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
print.simple_table = function(x, round_digits = 2, ...,  row.names = FALSE){
    class(x) = class(x) %d% "simple_table"
    if(!is.null(round_digits)){
        for (each in seq_along(x)){
            if(is.numeric(x[[each]])) x[[each]] = round(x[[each]], round_digits)
        }
    }
    print(x, ..., row.names = row.names)
}

#' @export
print.summary_table = function(x, ...,  row.names = FALSE){
    class(x) = class(x) %d% "summary_table"
    print(x, ..., row.names = row.names)
}

#' @export
#' @rdname fre
cro_fun = function(x, predictor, fun, ..., weight = NULL){
    stopif(NROW(x)!=length(predictor), "predictor should have the same number of rows as x.")
    stopif(NCOL(predictor)>1, "predictor should have only one column.")
    stopif(!is.null(weight) && (NROW(x)!=length(weight)), "weight should have the same number of rows as x.")
    fun = match.fun(fun)
    if(!is.data.frame(x)){
        if (is.matrix(x)) {
            varlab0 = var_lab(x)
            x = as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
            if(!is.null(varlab0)) colnames(x) = paste(varlab0, LABELS_SEP,colnames(x))
        } else {
            x = as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE) 
        }
    }
    for(each in seq_along(x)){
        if(is.factor(x[[each]])) x[[each]] = as.character(x[[each]])
    }
    
    test_result = function(each){
        stopif(length(each)>1, "Length of result of 'fun' should be equals to 1")
        if (length(each)==0){
            NA
        } else {
            each
        }
    }
    
    if (!is.null(weight)) {
        # change negative and NA weights to 0 
        if_val(weight) = list(lo %thru% 0 ~ 0, NA ~ 0)
        splitted_weight = split(weight, predictor, drop = TRUE)
        column_total = lapply(x, FUN = function(each) fun(each, weight = weight, ...))
        
    } else {
        column_total = lapply(x, FUN = fun, ...)

    }
    column_total = unlist(lapply(column_total, test_result))

    labels = vapply(colnames(x), function(each) {
            varlab = var_lab(x[[each]])
            if(is.null(varlab)) varlab = each
            varlab
        }, 
        FUN.VALUE = character(1)
    )

    predictor = f(unvr(predictor))
    if(is.null(weight)){
        result = lapply(x, function(col){
            res = lapply(split(col, predictor, drop = TRUE), FUN = fun, ...)
            unlist(lapply(res, test_result))
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
            unlist(lapply(res, test_result))
        }) 
    }
    res = do.call(rbind, result)
    res = data.frame(" " = labels, res, "#Total" = column_total, stringsAsFactors = FALSE, check.names = FALSE)
    
    class(res) = union("summary_table", class(res))
    res
    
}

#' @export
#' @rdname fre
cro_mean = function(x, predictor, weight = NULL){
    stopif(NROW(x)!=length(predictor), "predictor should have the same number of rows as x.")
    stopif(NCOL(predictor)>1, "predictor should have only one column.")
    stopif(!is.null(weight) && (NROW(x)!=length(weight)), "weight should have the same number of rows as x.")
    set_na(x) = is.na(predictor)
    if (is.null(weight)){
        cro_fun(x = x, predictor = predictor, fun = mean, na.rm = TRUE)
    } else {
        cro_fun(x = x, predictor = predictor, weight = weight, fun = function(x, weight, na.rm){
            weighted.mean(x = x, w = weight, na.rm = TRUE)
        })
    }
}

#' @export
#' @rdname fre
cro_sum = function(x, predictor, weight = NULL){
    stopif(NROW(x)!=length(predictor), "predictor should have the same number of rows as x.")
    stopif(NCOL(predictor)>1, "predictor should have only one column.")
    stopif(!is.null(weight) && (NROW(x)!=length(weight)), "weight should have the same number of rows as x.")
    set_na(x) = is.na(predictor)
    if (is.null(weight)){
        cro_fun(x = x, predictor = predictor, fun = sum, na.rm = TRUE)
    } else {
        cro_fun(x = x, predictor = predictor, weight = weight, fun = function(x, weight, na.rm){
            sum(x*weight, na.rm = TRUE)
        })
    }
}

#' @export
#' @rdname fre
cro_median = function(x, predictor){
    stopif(NROW(x)!=length(predictor), "predictor should have the same number of rows as x.")
    stopif(NCOL(predictor)>1, "predictor should have only one column.")
    set_na(x) = is.na(predictor)
    cro_fun(x = x, predictor = predictor, fun = median, na.rm = TRUE)
}

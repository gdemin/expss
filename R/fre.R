#' Title
#'
#' @param x 
#' @param predictor 
#' @param weight 
#'
#' @return data.frame
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
#' with(mtcars, cro_pct(am, vs))
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
#' cro_pct(brands, score)
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
        cum = NA
    )
    res$cum = cumsum(res$rpercent)
    dfs_na = data.frame(labels = "<NA>", 
                        res = nas, 
                        valid_percent = NA, 
                        percent = ifelse(base>0, nas/base*100, 0),
                        rpercent = NA,
                        cum = NA
                        )
    res = rbind(res, dfs_total, dfs_na)
    rownames(res) = NULL
    
    varlab = var_lab(x)
    if (is.null(varlab)){
        varlab = deparse(substitute(x))
    }

    colnames(res) = c(varlab, "Count", "Valid percent", "Percent", "Responses, %", "Cumulative responses, %")
    

    res
    
}



elementary_freq = function(x, predictor = NULL, weight = NULL){
    stopif(!is.null(weight) && (NROW(x)!=length(weight)), "weight should have the same number of rows as x.")
    stopif(NCOL(predictor)>1, "predictor should have only one column.")
    if (is.matrix(x)) {
        x = as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
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

    column_total = data.frame(labels = "#Total(counts)", 
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
    res
    
}

#' @export
#' @rdname fre
cro_pct = function(x, predictor, weight = NULL){
    res = cro(x = x, predictor = predictor, weight = weight)
    last_row = NROW(res)
    if(NCOL(res)>1 & last_row>1){
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



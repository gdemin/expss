#' Title
#'
#' @param x 
#' @param predictor 
#' @param weight 
#'
#' @return data.frame
#' @export
#'
#' @examples
#' a = a + 1
fre = function(x, weight = NULL){
    raw = elementary_freq(x = x, weight = weight)
    not_nas = raw$not_nas
    nas = raw$nas
    res = raw$freq
    res = res[!is.na(res$res), ]

    if(not_nas>0) {
        res$valid_percent =  res$res/not_nas*100    
    } else {
        res$valid_percent = 0
    }    
    base = not_nas + nas
    if(base>0) {
        res$percent =  res[[2]]/base*100    
    } else {
        res$percent = 0
    }   
    total = sum_col(res[,-1])
    dfs_total = data.frame(
        labels = "#Total",
        res = total[1],
        valid_percent = total[2], 
        percent = total[3],
        cum = NA
    )
    res$cum = cumsum(res$valid_percent)
    dfs_na = data.frame(labels = "<NA>", 
                        res = nas, 
                        valid_percent = NA, 
                        percent = ifelse(base>0, nas/base*100, 0),
                        cum = NA
                        )
    res = rbind(res, dfs_total, dfs_na)
    rownames(res) = NULL
    
    varlab = var_lab(x)
    if (is.null(varlab)){
        varlab = deparse(substitute(x))
    }
    colnames(res) = c(varlab, "Count", "Valid percent", "Percent", "Cumulative percent")
    res
    
}



elementary_freq = function(x, predictor = NULL, weight = NULL){
    stopif(!is.null(weight) && (NROW(x)!=length(weight)), "weight should have the same number of rows as x.")
    stopif(NCOL(predictor)>1, "predictor should have only one column.")
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
    res = data.frame(labels = labels, res, stringsAsFactors = FALSE, check.names = FALSE)
    row.names(res) = NULL
    list(freq = res, not_nas = not_nas, nas = nas, total = total) 
    
}



#' @export
#' @rdname fre
cro = function(x, predictor, weight = NULL){
    raw = elementary_freq(x = x, predictor = predictor, weight = weight)
    column_total = raw
    not_nas = raw$not_nas
    nas = raw$nas
    res = raw$freq
    res = res[!is.na(res$res), ]
    
    if(not_nas>0) {
        res$valid_percent =  res$res/not_nas*100    
    } else {
        res$valid_percent = 0
    }    
    base = not_nas + nas
    if(base>0) {
        res$percent =  res[[2]]/base*100    
    } else {
        res$percent = 0
    }   
    total = sum_col(res[,-1])
    dfs_total = data.frame(
        labels = "#Total",
        res = total[1],
        valid_percent = total[2], 
        percent = total[3],
        cum = NA
    )
    res$cum = cumsum(res$valid_percent)
    dfs_na = data.frame(labels = "<NA>", 
                        res = nas, 
                        valid_percent = NA, 
                        percent = ifelse(base>0, nas/base*100, 0),
                        cum = NA
    )
    res = rbind(res, dfs_total, dfs_na)
    rownames(res) = NULL
    
    varlab = var_lab(x)
    if (is.null(varlab)){
        varlab = deparse(substitute(x))
    }
    colnames(res) = c(varlab, "Count", "Valid percent", "Percent", "Cumulative percent")
    res
    
}





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
    stopif(!is.null(weight) && (NROW(x)!=length(weight)), "weight should have the same number of rows as x.")
    if (is.null(weight)) {
        weight = rep(1, NROW(x))
    } else {
        # change negative and NA weights to 0 
        if_val(weight) = list(lo %thru% 0 ~ 0, NA ~ 0)
    }    
    not_all_na = (NULL %in_row% x) # count not-NA
    not_nas = sum(weight*(not_all_na), na.rm = TRUE)
    nas = sum(weight*(!not_all_na), na.rm = TRUE)
    varlab = var_lab(x)
    if (is.null(varlab)){
        varlab = deparse(substitute(x))
    }
    vallab = val_lab(x)
    x = unlab(x)
    weight = rep(weight, NCOL(x))
    if(is.data.frame(x)){
        for(each in seq_along(x)){
            if(is.factor(x[[each]])) x[[each]] = as.character(x[[each]])
        }
    } else {
        if(is.factor(x)) x = as.character(x)
    }
    x = c(x, recursive = TRUE)
    val_lab(x) = vallab
    res = tapply(weight, list(f(x)), FUN = sum, na.rm = TRUE)
    res = res[!is.na(res)]
    # if_na(res) = 0
    labels = rownames(res)
    res = data.frame(labels = labels, res = res, stringsAsFactors = FALSE, check.names = FALSE)
    if(not_nas>0) {
        res$valid_percent =  res$res/not_nas*100    
    } else {
        res$valid_percent = 0
    }    
    base = not_nas + nas
    if(base>0) {
        res$percent =  res$res/base*100    
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
    colnames(res) = c(varlab, "Count", "Valid percent", "Percent", "Cumulative percent")
    res
    
}

#' @export
#' @rdname fre
cro = function(x, predictor, weight = NULL){
    stopif(!is.null(weight) && (NROW(x)!=length(weight)), "weight should have the same number of rows as x.")
    stopif(NROW(x)!=length(predictor), "predictor should have the same number of rows as x.")
    stopif(NCOL(predictor)>1, "predictor should have only one column.")
    if (is.null(weight)) {
        weight = rep(1, NROW(x))
    } else {
        # change negative and NA weights to 0 
        if_val(weight) = list(lo %thru% 0 ~ 0, NA ~ 0)
    }    
    not_all_na = (NULL %in_row% x) # count not-NA
    not_nas = sum(weight*(not_all_na), na.rm = TRUE)
    nas = sum(weight*(!not_all_na), na.rm = TRUE)
    varlab = var_lab(x)
    if (is.null(varlab)){
        varlab = deparse(substitute(x))
    }
    vallab = val_lab(x)
    x = unlab(x)
    weight = rep(weight, NCOL(x))
    x = c(x, recursive = TRUE)
    val_lab(x) = vallab
    res = tapply(weight, list(f(x)), FUN = sum, na.rm = TRUE)
    labels = rownames(res)
    res = data.frame(labels = labels, res = res, stringsAsFactors = FALSE, check.names = FALSE)
    if(not_nas>0) {
        res$valid_percent =  res$res/not_nas*100    
    } else {
        res$valid_percent = 0
    }    
    base = not_nas + nas
    if(base>0) {
        res$percent =  res$res/base*100    
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
    colnames(res) = c(varlab, "Count", "Valid percent", "Percent", "Cumulative percent")
    res
    
}
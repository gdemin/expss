#' Simple frequencies with support of labels, weights and multiple response variables.
#' 
#' \code{fre} returns data.frame with six columns: labels or values, counts, 
#' valid percent (excluding NA), percent (with NA), percent of responses(for 
#' single-column \code{x} it equals to valid percent) and cumulative percent of 
#' responses.
#' 
#' @param x vector/data.frame/list. data.frames are considered as multiple
#'   response variables. If \code{x} is list then vertically stacked frequencies
#'   for each element of list will be generated,
#' @param weight numeric vector. Optional case weights. NA's and negative weights
#'   treated as zero weights.
#' @param drop_unused_labels logical. Should we drop unused value labels?
#'   Default is TRUE.
#' @param prepend_var_lab logical. Should we prepend variable label before value
#'   labels? By default we will add variable labels to value labels only if
#'   \code{x} or predictor is list (several variables).
#' @param stat_lab character. Labels for the frequency columns. 
#'
#' @return object of class 'etable'. Basically it's a data.frame but class
#'   is needed for custom methods.
#'
#' @examples
#' data(mtcars)
#' mtcars = mtcars %>% 
#'     apply_labels(
#'         mpg = "Miles/(US) gallon",
#'         cyl = "Number of cylinders",
#'         disp = "Displacement (cu.in.)",
#'         hp = "Gross horsepower",
#'         drat = "Rear axle ratio",
#'         wt = "Weight (lb/1000)",
#'         qsec = "1/4 mile time",
#'         vs = "Engine",
#'         vs = c("V-engine" = 0, 
#'                 "Straight engine" = 1),
#'         am = "Transmission",
#'         am = c(automatic = 0, 
#'                 manual=1),
#'         gear = "Number of forward gears",
#'         carb = "Number of carburetors"
#'     )
#' 
#' fre(mtcars$vs)
#' 
#' # stacked frequencies
#' fre(list(mtcars$vs, mtcars$am))
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
#' fre(brands)
#' 
#' # stacked frequencies
#' fre(list(score, brands))
#' 
#' @export
fre = function(x, 
               weight = NULL, 
               drop_unused_labels = TRUE, 
               prepend_var_lab = FALSE, 
               stat_lab = getOption("expss.fre_stat_lab", c("Count", "Valid percent", "Percent", "Responses, %", "Cumulative responses, %"))){
    UseMethod("fre")
}

#' @export
fre.list = function(x, 
                    weight = NULL, 
                    drop_unused_labels = TRUE, 
                    prepend_var_lab = TRUE, 
                    stat_lab = getOption("expss.fre_stat_lab", c("Count", "Valid percent", "Percent", "Responses, %", "Cumulative responses, %"))){
    x = flat_list(x, flat_df = FALSE)
    res = lapply(x, fre, 
                 weight = weight, 
                 drop_unused_labels = drop_unused_labels, 
                 prepend_var_lab = prepend_var_lab,
                 stat_lab = stat_lab
                 )
    do.call(add_rows, res)
}

#' @export
fre.default = function(x, weight = NULL, drop_unused_labels = TRUE, prepend_var_lab = FALSE, stat_lab = getOption("expss.fre_stat_lab", c("Count", "Valid percent", "Percent", "Responses, %", "Cumulative responses, %"))){
    str_x = expr_to_character(substitute(x))
    if(is.null(x)){
        stop(paste0(str_x," is NULL. Perhaps a variable does not exist."))
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
    res = long_datatable_to_table(dtable, rows = "x", columns = "count", values = "weight")
    res = as.sheet(res)
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
    dfs_total = sheet(
        labels = "#Total",
        res = not_nas,
        valid_percent = ifelse(total>0, 100,0), 
        percent = ifelse(base>0, not_nas/base*100, 0),
        rpercent = ifelse(total>0, 100, 0),
        cum = NA
    )
    res$cum = cumsum(if_na(res$rpercent, 0))
    dfs_na = sheet(labels = "<NA>", 
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
    colnames(res) = c(first_column_label, stat_lab)
    
    class(res) = union("etable", class(res))
    res
    
}


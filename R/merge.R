
#' Merge two tables/data.frames
#'
#' \code{\%merge\%} is infix shortcut for base \link[base]{merge} with 
#' \code{all.x = TRUE} and  \code{all.y = FALSE} (left join). There is also 
#' special method for combining results of \code{cro_*} and \code{fre}. For them
#' \code{all = TRUE} (full join). It allows make complex tables from simple
#' ones. See examples. Strange result is possible if one or two arguments have
#' duplicates in first column (column with labels).
#'
#' @seealso \link{fre}, \link{cro}, \link{cro}, \link{cro_fun}, \link[base]{merge}
#'
#' @param x data.frame or results of \code{fre}/\code{cro_*}/\code{table_*}
#' @param y data.frame or results of \code{fre}/\code{cro_*}/\code{table_*}
#'
#' @return data.frame
#' @name merge.etable
#' @export
#'
#' @examples
#' data(mtcars)
#' # apply labels
#' mtcars = apply_labels(mtcars,
#'                 mpg = "Miles/(US) gallon",
#'                 cyl = "Number of cylinders",
#'                 disp = "Displacement (cu.in.)",
#'                 hp = "Gross horsepower",
#'                 drat = "Rear axle ratio",
#'                 wt = "Weight (lb/1000)",
#'                 qsec = "1/4 mile time",
#'                 vs = "V/S",
#'                 vs = c("V-engine" = 0, "Straight engine" = 1),
#'                 am = "Transmission (0 = automatic, 1 = manual)",
#'                 am = c(automatic = 0, manual = 1),
#'                 gear = "Number of forward gears",
#'                 carb = "Number of carburetors"
#' )
#'
#' # table by 'am'
#' tab1 = calculate(mtcars, cro_cpct(gear, am))
#' # table with percents
#' tab2 = calculate(mtcars, cro_cpct(gear, vs))
#'
#' # combine tables
#' tab1 %merge% tab2
#'
#' # complex tables
#' # table with counts
#' counts = calculate(mtcars, cro(list(vs, am, gear, carb), list("Count")))
#' # table with percents
#' percents = calculate(mtcars, cro_cpct(list(vs, am, gear, carb), list("Column, %")))
#'
#' # combine tables
#' counts %merge% percents
'%merge%' = function(x, y) UseMethod('%merge%')

#' @export
'%merge%.default' = function(x, y) {
    common = intersect(colnames(x), colnames(y))
    stopif(!length(common), "`%merge%` - there are no common column names between `x` and `y`.")
    merge(x, y, all.x = TRUE, all.y = FALSE)
}


#' @export
merge.etable = function(x, y,
                        by = 1,
                        by.x = by,
                        by.y = by,
                        all = TRUE,
                        all.x = all,
                        all.y = all,
                        sort = FALSE,
                        suffixes = c("",""),
                        incomparables = NULL, ...){

    res = merge_table(x = x, 
                      y = y,
                      by.x = by.x,
                      by.y = by.y,
                      all.x = all.x,
                      all.y = all.y,
                      sort = sort,
                      suffixes = suffixes,
                      incomparables = incomparables,
                      ...)

    if(!("etable" %in% class(res))) class(res) = c("etable", class(res))
    res

}




#' @export
'%merge%.etable' = function(x, y) merge.etable(x, y)

merge_table = function(x, y,
                       by.x,
                       by.y,
                       all.x,
                       all.y,
                       sort,
                       suffixes,
                       incomparables, ...){
    stopif(length(by.x)>1 || length(by.y)>1, "'etable' can be merged only by single column.")
    class_x = class(x)
    class_y = class(y)
    # below we try to preserve order in rows in y for rows which doesn't exists in x
    order.x = seq_len(nrow(x))
    x[['..order..x']] = order.x
    
    #### duplicated column names are possible so we use integer position if by.x or by.y is character
    if(is.numeric(by.x)){
        pos1 = by.x
    } else {
        pos1 = match(by.x, colnames(x))[[1]]
    }
    if(is.numeric(by.y)){
        pos2 = by.y
    } else {
        pos2 = match(by.y, colnames(y))[[1]]
    }
    order.y = order.x[match(y[[pos2]], x[[pos1]])]
    # fill NA.
    need_sort = anyNA(order.y) & !all(is.na(order.y))
    if(need_sort){
        delta = 1/length(order.y)/10
        if(is.na(order.y[1])) order.y[1] = 0
        for (i in seq_along(order.y)[-1]){
            if(is.na(order.y[i])){
                order.y[i] = order.y[i-1] + delta
            }
        }
        y[['..order..y']] = order.y
    }
    
    ###### actions for avoiding duplication of rows if we have duplicated keys
    x_match_col = x[[pos1]]
    y_match_col = y[[pos2]]
    # if(anyDuplicated(x_match_col) || anyDuplicated(y_match_col)){
    x[[pos1]] = make_items_unique(x[[pos1]])       
    y[[pos2]] = make_items_unique(y[[pos2]])
    uniqs = c(x[[pos1]], y[[pos2]])
    old = c(x_match_col, y_match_col)
    # } else {
    # old = NULL
    # }

    ##########################################
    if(length(suffixes)>0){
        suffix.x = suffixes[1]
    } else {
        suffix.x = ""
    } 
    if(length(suffixes)>1){
        suffix.y = suffixes[2]
    } else {
        suffix.y = ""
    } 
    preserve_colnames = c(colnames(x)[pos1], 
                          paste0(colnames(x)[-pos1] %d% '..order..x', suffix.x), 
                          paste0(colnames(y)[-pos2] %d% '..order..y', suffix.y)
    )
    res = suppressWarnings(merge.data.frame(x, y, by.x = by.x, by.y = by.y,
                                            all.x = all.x, all.y = all.y,
                                            sort = sort, suffixes = suffixes,
                                            incomparables = incomparables,
                                            ... ))
    if(need_sort){
        order.y = res[['..order..y']]
        if_na(order.y) = res[['..order..x']]
        res = res[order(order.y), , drop = FALSE]
    } else {
        res = res[order(res[['..order..x']]), , drop = FALSE]
        
    }
    res = res %n_d% c('..order..y','..order..x')
    res[[1]] = old[match(res[[1]], uniqs)]
    colnames(res) = preserve_colnames
    class(res) = intersect(class_x, class_y)
    rownames(res) = NULL
    res

}
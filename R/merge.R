#' Merge two tables/data.frames
#'
#' \code{\%merge\%} is infix shortcut for base \link[base]{merge} with 
#' \code{all.x = TRUE} and  \code{all.y = FALSE} (left join). There is also 
#' special method for combining results of \code{cro_*} and \code{fre}. For them
#' \code{all = TRUE} (full join). It allows make complex tables from simple
#' ones. See examples. Strange result is possible if one or two arguments have
#' duplicates in first column (column with labels).
#'
#' @seealso \link{fre}, \link{cro}, \link[base]{merge}
#'
#' @param x data.frame or results of \code{fre}/\code{cro_*}
#' @param y data.frame or results of \code{fre}/\code{cro_*}
#'
#' @return data.frame
#' @name merge.simple_table
#' @export
#'
#' @examples
#' data(mtcars)
#' # apply labels
#' mtcars = modify(mtcars,{
#'                 var_lab(mpg) = "Miles/(US) gallon"
#'                 var_lab(cyl) = "Number of cylinders"
#'                 var_lab(disp) = "Displacement (cu.in.)"
#'                 var_lab(hp) = "Gross horsepower"
#'                 var_lab(drat) = "Rear axle ratio"
#'                 var_lab(wt) = "Weight (lb/1000)"
#'                 var_lab(qsec) = "1/4 mile time"
#'                 var_lab(vs) = "V/S"
#'                 val_lab(vs) = c("V-engine" = 0, "Straight engine" = 1)
#'                 var_lab(am) = "Transmission (0 = automatic, 1 = manual)"
#'                 val_lab(am) = c(automatic = 0, manual = 1)
#'                 var_lab(gear) = "Number of forward gears"
#'                 var_lab(carb) = "Number of carburetors"
#' })
#'
#' # table by 'am'
#' tab1 = with(mtcars, cro_cpct(gear, am))
#' # table with percents
#' tab2 = with(mtcars, cro_cpct(gear, vs))
#'
#' # combine tables
#' # %n_d% remove first total
#' tab1 %n_d% "#Total" %merge% tab2
'%merge%' = function(x, y) UseMethod('%merge%')

#' @export
'%merge%.default' = function(x, y) {
    common = intersect(colnames(x), colnames(y))
    stopif(!length(common), "`%merge%` - there are no common column names between `x` and `y`.")
    merge(x, y, all.x = TRUE, all.y = FALSE)
}

#' @export
'%merge%.simple_table' = function(x, y) merge.simple_table(x, y)




#' @export
merge.simple_table = function(x, y,
                        by.x = colnames(x)[1],
                        by.y = colnames(y)[1],
                        all = TRUE,
                        all.x = all,
                        all.y = all,
                        sort = FALSE,
                        suffixes = c("",""),
                        incomparables = NULL, ...){

    class_x = class(x)
    class_y = class(y)
    # below we try to preserve order in rows in y for rows which doesn't exists in x
    order.x = seq_len(nrow(x))
    x[['..order..x']] = order.x
    order.y = order.x[match(y[[1]], x[[1]])]
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
    
    res = suppressWarnings(merge.data.frame(x, y, by.x = by.x, by.y = by.y,
                                            all.x = all.x, all.y = all.y,
                                            sort = sort, suffixes = suffixes,
                                            incomparables = incomparables,
                                            ... ))
    preserve_colnames = colnames(res) %d% c('..order..y','..order..x')
    if(need_sort){
        order.y = res[['..order..y']]
        if_na(order.y) = res[['..order..x']]
        res = res[order(order.y), , drop = FALSE]
    } else {
        res = res[order(res[['..order..x']]), , drop = FALSE]
        
    }
    res = res %n_d% c('..order..y','..order..x')
    colnames(res) = preserve_colnames
    class(res) = intersect(class_x, class_y)
    if(!("simple_table" %in% class(res))) class(res) = c("simple_table", class(res))
    res

}


#' @export
merge.summary_table = merge.simple_table

#' @export
'%merge%.summary_table' = function(x, y) merge.summary_table(x, y)
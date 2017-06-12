#' Aggregate dataset by grouping variable(s).
#'
#' Splits the data by groups, computes summary statistics for each, and returns
#' \code{data.frame}/\code{data.table}. 
#'
#' @param data data for aggregation
#' @param ... aggregation parameters. It should be names of variables in quotes 
#'   (characters, e. g. 'Species') and formulas with aggregation expressions, 
#'   such as \code{mean_x ~ mean(x)}. Instead of the formulas it can be single 
#'   function as last argument - it will be applied to all non-grouping columns.
#'   Note that there is no non-standard evaluation by design so use quotes for
#'   names of your variables or use \link{qc}.
#' @return aggregated data.frame/data.table
#' @export
#'
#' @examples
#' # compute mean of the every column for every value of the Species
#' data(iris)
#' by_groups(iris, "Species", mean)
#'
#' # compute mean of the every numeric column
#' by_groups(iris %except% "Species", mean)
#'
#' # compute different functions for different columns
#' # automatic naming
#' data(mtcars)
#' by_groups(mtcars, "cyl", "am", ~ mean(hp), ~ median(mpg))
#'
#' # with custom names
#' by_groups(mtcars, "cyl", "am", mean_hp ~ mean(hp), median_mpg ~ median(mpg))
#'
#' # 'qc' usage to avoide quotes
#' by_groups(mtcars, qc(cyl, am), ~ mean(hp), ~ median(mpg))
#'
#' # variable substitution
#' group1 = "cyl"
#' statistic1 = as.formula("~ mean(hp)")
#' by_groups(mtcars, group1, statistic1)
#'
#' group2 = "am"
#' statistic2 = as.formula("~ median(mpg)")
#' by_groups(mtcars, group2, statistic2)
#'
#' by_groups(mtcars, group1, group2, statistic1, statistic2)
#'
#'
by_groups = function(data, ...){
    UseMethod("by_groups")
}

#' @export
by_groups.data.table = function(data, ...){
    args = unlist(list(...))
    stopif(length(args)==0, "'by groups' - insufficient number of arguments.")
    is_formula = vapply(args, function(each) "formula" %in% class(each), FUN.VALUE = logical(1))
    formulas = args[is_formula]
    if(length(formulas)==0){
        fun = match.fun(args[[length(args)]])
        grouping_variables = args[-length(args)]  
    } else {
        grouping_variables = args[!is_formula]
    }
    grouping_variables = unique(as.character(unlist(grouping_variables)))
    non_grouping = setdiff(colnames(data), grouping_variables)
    unknowns = grouping_variables %d% colnames(data)
    stopif(length(unknowns), "some variables doesn't exist in 'data': ", paste(unknowns, collapse = ","))

    if(length(grouping_variables)){
        grouping_variables = paste(grouping_variables, collapse = ",")
    }

    if (length(formulas)==0){
        if (length(grouping_variables)){
            res = data[, lapply(.SD, fun), by = grouping_variables]
        } else {
            res = data[, lapply(.SD, fun)]
        }
    } else {
        processed_formulas = lapply(formulas, function(each_formula){
            res = as.character(each_formula)[-1]
            if(length(res)==1) res = c(res, res) # new_name=expression
            res
        })
        expressions = vapply(processed_formulas, function(item){
            paste0('"', item[1], '" = ', item[2])
        }, FUN.VALUE = character(1))
        all_expressions = parse(text = paste0("list(", paste(expressions, collapse = ","), ")"))
        if (length(grouping_variables)){
            res = data[, eval(all_expressions), by = grouping_variables]
        } else {
            res = data[, eval(all_expressions)]
        }
    }
    for(each in intersect(non_grouping, colnames(res))){
        var_lab(res[[each]]) = var_lab(data[[each]])
    }
    res
}

#' @export
by_groups.data.frame = function(data, ...){
    res = by_groups.data.table(data.table(data), ...)
    as.data.frame(res, stringsAsFactors = FALSE, check.names = FALSE)
}

#' @export
by_groups.default = function(data, ...){
    by_groups(as.data.frame(data), ...)

}



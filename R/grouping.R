#' Aggregate dataset by grouping variable(s).
#'
#' Splits the data by groups, computes summary statistics for each, and returns
#' \code{data.frame}/\code{data.table}.
#'
#' @param data data for aggregation
#' @param ... aggregation parameters. Character/numeric or criteria/logical
#'   functions (see \link{criteria}) for grouping variables. Names of variables
#'   at the top-level can be unquoted (non-standard evaluation). For standard
#'   evaluation of parameters, you can surround them by round brackets. You need
#'   additionally specify formulas with aggregation expressions, such as
#'   \code{mean_x ~ mean(x)}. Instead of the formulas it can be single function
#'   as last argument - it will be applied to all non-grouping columns. See
#'   examples.
#' @return aggregated data.frame/data.table
#' @export
#'
#' @examples
#' # compute mean of the every column for every value of the Species
#' data(iris)
#' by_groups(iris, Species, mean)
#'
#' # compute mean of the every numeric column
#' iris %>% except(Species) %>% by_groups(mean)
#'
#' # compute different functions for different columns
#' # automatic naming
#' data(mtcars)
#' by_groups(mtcars, cyl, am, ~ mean(hp), ~ median(mpg))
#'
#' # with custom names
#' by_groups(mtcars, cyl, am, mean_hp ~ mean(hp), median_mpg ~ median(mpg))
#'
#' # variable substitution
#' group1 = "cyl"
#' statistic1 = ~ mean(hp)
#' by_groups(mtcars, (group1), (statistic1))
#'
#' group2 = "am"
#' # formulas can be easily constructed from text strings
#' statistic2 = as.formula("~ median(mpg)") 
#' by_groups(mtcars, (group2), (statistic2))
#'
#' by_groups(mtcars, (group1), (group2), (statistic1), (statistic2))
#' 
by_groups = function(data, ...){
    .Deprecated("take", "maditr", "'by_groups' is deprecated and will be removed in the future version.\nUse 'take' from 'maditr' package instead. See '?maditr::take'.")
    args = substitute(list(...))
    by_groups_internal(data, args, envir = parent.frame())
}


by_groups_internal = function(data, args, envir){
   UseMethod("by_groups_internal") 
}

#' @export
by_groups_internal.data.frame = function(data, args, envir){
    res = by_groups_internal.data.table(data.table(data), args, envir = envir)
    as.sheet(res)
}

#' @export
by_groups_internal.default = function(data, args, envir){
    res = by_groups_internal.data.table(as.data.table(data), args, envir = envir)
    as.sheet(res)
}

# data - data.table
#' @export
by_groups_internal.data.table = function(data, args, envir){
    
    evaluated_args = evaluate_variable_names(args, 
                                             envir = envir, 
                                             symbols_to_characters = TRUE)
    stopif(length(evaluated_args)==0, "'by groups' - insufficient number of arguments.")
    is_formula = vapply(evaluated_args, function(each) "formula" %in% class(each), FUN.VALUE = logical(1))
    formulas = evaluated_args[is_formula]
    if(length(formulas)==0){
        expr = args[[length(args)]]
        fun = eval(expr, envir = envir, enclos = baseenv())
        fun = match.fun(fun)
        evaluated_args = evaluate_variable_names(args[-length(args)], 
                                                 envir = envir, 
                                                 symbols_to_characters = TRUE)  
    } else {
        evaluated_args = evaluated_args[!is_formula]
    }
    cl_names = colnames(data)
    var_indexes = create_indexes_from_evaluated_names(cl_names, evaluated_args)
    grouping_variables = cl_names[var_indexes]
    non_grouping = setdiff(cl_names, grouping_variables)
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


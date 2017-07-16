#' Modify data.frame/modify subset of the data.frame
#' 
#' \itemize{
#' \item{{\code{compute}}{ evaluates expression \code{expr} in the context of data.frame 
#' \code{data} and return original data possibly modified. It works similar to
#' \code{\link[base]{within}} in base R but try to return new variables in order
#' of their occurrence in the expression and make available
#' full-featured \code{\%to\%} and \code{.N} in the expressions. See \link{vars}.}}
#' \item{{\code{calculate}}{ evaluates expression \code{expr} in the context of 
#' data.frame \code{data} and return value of the evaluated expression. It works
#' similar to \code{\link[base]{with}} in base R but make available 
#' full-featured \code{\%to\%} and \code{.N} in the expressions. See 
#' \link{vars}. Function \code{use_labels} is shortcut for \code{calculate} with
#' argument \code{use_labels} set to \code{TRUE}.}}
#' \item{{\code{do_if}}{ modifies only rows for which \code{cond} equals to
#' TRUE. Other rows remain unchanged. Newly created variables also will have
#' values only in rows for which \code{cond} have TRUE. There will be NA's in
#' other rows. This function tries to mimic SPSS "DO IF(). ... END IF."
#' statement.}}
#' }
#' There is a special constant \code{.N} which equals to number of cases in 
#' \code{data} for usage in expression inside \code{compute}/\code{calculate}. 
#' Inside \code{do_if} \code{.N} gives number of rows which will be affected by 
#' expressions. For parametrization (variable substitution) see \link{..} or 
#' examples. Sometimes it is useful to create new empty variable inside compute.
#' You can use \code{.new_var} function for this task. This function creates
#' variable of length \code{.N} filled with NA. See examples.
#' \code{modify} is an alias for \code{compute}, \code{modify_if} is
#' an alias for \code{do_if} and \code{calc} is an alias for \code{calculate}.
#' 
#' @param data data.frame/list of data.frames. If \code{data} is list of
#'   data.frames then expression \code{expr} will be evaluated inside each
#'   data.frame separately.
#' @param expr expression that should be evaluated in the context of data.frame \code{data}
#' @param cond logical vector or expression. Expression will be evaluated in the context of the data.  
#' @param use_labels logical. Experimental feature. If it equals to \code{TRUE} 
#'   then we will try to replace variable names with labels. So many base R
#'   functions which show variable names will show labels.
#'
#' @return \code{compute} and \code{do_if} functions return modified 
#'   data.frame/list of modified data.frames, \code{calculate} returns value of
#'   the evaluated expression/list of values.
#' @examples
#' dfs = data.frame(
#'     test = 1:5,
#'     a = rep(10, 5),
#'     b_1 = rep(11, 5),
#'     b_2 = rep(12, 5),
#'     b_3 = rep(13, 5),
#'     b_4 = rep(14, 5),
#'     b_5 = rep(15, 5) 
#' )
#' 
#' 
#' # compute sum of b* variables and attach it to 'dfs'
#' compute(dfs, {
#'     b_total = sum_row(b_1 %to% b_5)
#'     var_lab(b_total) = "Sum of b"
#'     random_numbers = runif(.N) # .N usage
#' })
#' 
#' # calculate sum of b* variables and return it
#' calculate(dfs, sum_row(b_1 %to% b_5))
#' 
#' 
#' # set values to existing/new variables
#' compute(dfs, {
#'     (b_1 %to% b_5) %into% subst('new_b`1:5`')
#' })
#' 
#' # .new_var usage
#' compute(dfs, {
#'     new_var = .new_var()
#'     new_var[1] = 1 # this is not possible without preliminary variable creation
#' })
#' 
#' # conditional modification
#' do_if(dfs, test %in% 2:4, {
#'     a = a + 1    
#'     b_total = sum_row(b_1 %to% b_5)
#'     random_numbers = runif(.N) # .N usage
#' })
#' 
#' 
#' # variable substitution
#' name1 = "a"
#' name2 = "new_var"
#' 
#' # example with short notation but it can be applied only for simple cases - 
#' # when 'name' is vector of length 1
#' compute(dfs, {
#'      ..$name2 = ..$name1*2    
#' })
#' 
#' compute(dfs, {
#'      for(name1 in paste0("b_", 1:5)){
#'          name2 = paste0("new_", name1) 
#'          ..$name2 = ..$name1*2 
#'      }
#'      rm(name1, name2) # we don't need this variables as columns in 'dfs'
#' })
#' 
#' # square brackets notation
#' compute(dfs, {
#'      ..[name2] = ..[name1]*2  
#' })
#' 
#' compute(dfs, {
#'      for(name1 in paste0("b_", 1:5)){
#'          ..[paste0("new_", name1)] = ..$name1*2 
#'      }
#'      rm(name1) # we don't need this variable as column in 'dfs'
#' })
#' 
#' # '..$' doesn't work for case below so we need to use square brackets form
#' name1 = paste0("b_", 1:5)
#' name2 = paste0("new_", name1)
#' compute(dfs, {
#'      for(i in 1:5){
#'          ..[name2[i]] = ..[name1[i]]*3
#'      }
#'      rm(i) # we don't need this variable as column in 'dfs'
#' })
#' 
#' # 'use_labels' examples. Utilization of labels in base R.
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       mpg = "Miles/(US) gallon",
#'                       cyl = "Number of cylinders",
#'                       disp = "Displacement (cu.in.)",
#'                       hp = "Gross horsepower",
#'                       drat = "Rear axle ratio",
#'                       wt = "Weight (lb/1000)",
#'                       qsec = "1/4 mile time",
#'                       vs = "Engine",
#'                       vs = c("V-engine" = 0,
#'                              "Straight engine" = 1),
#'                       am = "Transmission",
#'                       am = c("Automatic" = 0,
#'                              "Manual"=1),
#'                       gear = "Number of forward gears",
#'                       carb = "Number of carburetors"
#' )
#' 
#' use_labels(mtcars, table(am, vs))
#' 
#' \dontrun{
#' use_labels(mtcars, plot(mpg, hp))
#' }
#' 
#' mtcars %>% 
#'        use_labels(lm(mpg ~ disp + hp + wt)) %>% 
#'        summary()
#' 
#' @export
modify =  function (data, expr) {
    parent = parent.frame()
    expr = substitute(expr)
    modify_internal(data, expr, parent = parent)
}


modify_internal =  function (data, expr, parent) {
    UseMethod("modify_internal")
}

#' @export
modify_internal.data.frame = function (data, expr, parent) {
    # based on 'within' from base R by R Core team
    e = evalq(environment(), data, parent)
    prepare_env(e, n = nrow(data), column_names = colnames(data))
    eval(expr, envir = e, enclos = baseenv())
    clear_env(e)
    l = as.list(e, all.names = TRUE)
    l = l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    del = setdiff(names(data), names(l))
    if(length(del)){
        data[, del] = NULL
    }
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    wrong_rows = nrows!=1L & nrows!=nrow(data)
    if(any(wrong_rows)){
        er_message = utils::head(paste0("'", names(l)[wrong_rows], "' has ", nrows[wrong_rows], " rows"), 5)
        er_message = paste(er_message, collapse = ", ")
        stop(paste0("Bad number of rows: ", er_message, " instead of ", nrow(data), " rows."))
    }
    
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    nl = c(names(data), new_vars)
    data[nl] = l[nl]
    data
}

#' @export
modify_internal.list = function (data, expr, parent) {
    for(each in seq_along(data)){
        data[[each]] = modify_internal(data[[each]], expr, parent = parent)
    }
    data
}



#' @export
#' @rdname modify
compute = modify


#' @export
#' @rdname modify
modify_if = function (data, cond, expr){
    cond = substitute(cond)
    expr = substitute(expr)
    parent = parent.frame()
    modify_if_internal(data, cond, expr, parent = parent)
}


#' @export
#' @rdname modify
do_if = modify_if

modify_if_internal = function (data, cond, expr, parent){
    UseMethod("modify_if_internal")
} 

#' @export
modify_if_internal.data.frame = function (data, cond, expr, parent) {
    # based on 'within' from base R by R Core team
    e = evalq(environment(), data, parent)
    prepare_env(e, n = NROW(data), column_names = colnames(data))
    cond = calc_cond(cond, envir = e)
    
    new_data = modify_internal(data[cond,, drop = FALSE], expr, parent)
    del = setdiff(names(data), names(new_data))
    if(length(del)){
        data[, del] = NULL
    }
    new_vars = names(new_data)[!(names(new_data) %in% names(data))]
    data[cond, names(data)] = new_data[names(data)]
    data[, new_vars] = NA
    data[cond, new_vars] = new_data[new_vars]
    data
}

#' @export
modify_if_internal.list = function (data, cond, expr, parent) {
    for(each in seq_along(data)){
        data[[each]] = modify_if_internal(data[[each]], cond, expr, parent = parent)
    }
    data
}

########

#' @export
#' @rdname modify
calculate =  function (data, expr, use_labels = FALSE) {
    expr = substitute(expr)
    parent = parent.frame()
    calculate_internal(data, expr, parent, use_labels = use_labels)
}

#' @export
#' @rdname modify
use_labels =  function (data, expr) {
    expr = substitute(expr)
    parent = parent.frame()
    calculate_internal(data, expr, parent, use_labels = TRUE)
}


calculate_internal =  function (data, expr, parent, use_labels = FALSE) {
    UseMethod("calculate_internal")
}

#' @export
calculate_internal.data.frame = function (data, expr, parent, use_labels = FALSE) {
    # based on 'within' from base R by R Core team
    if(use_labels){
        substitution_list = extract_var_labs_as_list_with_symbols(data)
        data = names2labels(data) 
        if(length(substitution_list)>0){
            expr = substitute_symbols(expr, substitution_list)
        }
    }
    e = evalq(environment(), data, parent)
    prepare_env(e, n = nrow(data), column_names = colnames(data))
    eval(expr, envir = e, enclos = baseenv())
}

#' @export
calculate_internal.list = function (data, expr, parent, use_labels = FALSE) {
    for(each in seq_along(data)){
        data[[each]] = calculate_internal(data[[each]], expr, parent, use_labels)
    }
    data
}

extract_var_labs_as_list_with_symbols = function(data){
    res = lapply(data, function(x) {
        new_name = var_lab(x)
        if(!is.null(new_name) && !is.na(new_name) && new_name!=""){
            as.symbol(new_name)
        } else {
            NULL
        }
        })
    names(res) = colnames(data)
    res[lengths(res)>0]
}

#' @export
#' @rdname modify
calc = calculate

#' @export
#' @rdname modify
'%calc%' = calculate

#' @export
#' @rdname modify
'%use_labels%' = use_labels

#' @export
#' @rdname modify
'%calculate%' = calculate





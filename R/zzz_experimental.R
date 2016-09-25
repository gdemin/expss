#' Experimental functions for operations with default dataset
#'
#' Workflow for these functions is rather simple. You should set up default 
#' data.frame with \link{default_dataset} and then operate with it without any 
#' reference to your data.frame. There are two kinds of operations. The first kind
#' modify default dataset, the second kind will be evaluated in the context of
#' the default dataset but doesn't modify it. It is not recommended to use one
#' of these functions in the scope of another of these functions. By now their
#' performance is not so high, especially \code{.do_if}/\code{.modify_if} can be
#' very slow.
#' 
#' @details 
#' Functions which modify default dataset:
#' \itemize{
#' \item{\code{.modify}}{ Add and modify variables inside default data.frame. See
#' \link{modify}.}
#' \item{\code{.compute}}{ Shortcut for \code{.modify}. Name is inspired by
#' SPSS COMPUTE operator. See \link{modify}.}
#' \item{\code{.modify_if}}{ Add and modify variables inside subset of default
#' data.frame. See \link{modify_if}.}
#' \item{\code{.do_if}}{ Shortcut for \code{.modify_if}. Name is inspired by
#' SPSS DO IF operator. See \link{modify_if}.}
#' \item{\code{.filter}}{ Leave subset of default data.frame which meet
#' condition. See \link[base]{subset}.}
#' \item{\code{.set_var_lab}}{ Set variable label in the default data.frame. See
#' \link{set_var_lab}.}
#' \item{\code{.set_val_lab}}{ Set value labels for variable in the default
#' data.frame. See \link{set_val_lab}.}
#' \item{\code{.add_val_lab}}{ Add value labels for variable in the default
#' data.frame. See \link{add_val_lab}.}
#' \item{\code{.if_val}}{ Change, rearrange or consolidate the values of an existing
#' variable inside default data.frame. See \link{if_val}.}
#' \item{\code{.recode}}{ Shortcut for \code{.if_val}. Name is inspired by
#' SPSS RECODE. See \link{if_val}.}
#' \item{\code{.set}}{ Set variables values in the default dataset with given 
#' names filled with \code{value}. It is possible to set multiple variables at 
#' once. Expressions inside backticks in \code{varnames} will be expanded as
#' with \link{subst}. \code{set} (without dot) is also available inside
#' \code{.compute}, \code{.modify}, \code{.modify_if}, \code{.do_if},
#' \link{modify}, \link{modify_if}.} }
#' Other functions:
#' \itemize{
#' \item{\code{.var_lab}}{ Return variable label from default dataset. See
#' \link{var_lab}.}
#' \item{\code{.val_lab}}{ Return value labels from default dataset. See
#' \link{val_lab}.}
#' \item{\code{.fre }}{ Simple frequencies of variable in the default
#' data.frame.  See \link{fre}.}
#' \item{\code{.cro}/\code{.cro_cpct}/\code{.cro_rpct}/\code{.cro_tpct}}{ Simple
#' crosstabulations of variable in the default data.frame.  See \link{cro}.}
#' \item{\code{.cro_mean}/\code{.cro_sum}/\code{.cro_median}/\code{.cro_fun}/\code{.cro_fun_df}}{
#' Simple crosstabulations of variable in the default data.frame.  See 
#' \link{cro_fun}.}
#' \item{\code{.with}}{ Evaluate arbitrary expression in the context of
#' data.frame.  See \link[base]{with}.}
#' }
#' @param x vector/data.frame - variable names in the scope of default dataset
#' @param expr set of expressions  in curly brackets which will be evaluated in
#'   the context of default dataset
#' @param cond logical vector/expression
#' @param varnames character vector. Names of variables which should be created
#'   in the default dataset. Expressions inside backticks in \code{varnames}
#'   will be expanded as with \link{subst}.
#' @param value value/vector/matrix/data.frame. Value for newly created/existing
#'   variables.
#' @param ... further arguments 
#'
#' @examples 
#' data(mtcars)
#' 
#' default_dataset(mtcars) # set mtcars as default dataset
#' 
#' # calculate new variables
#' .compute({
#'     mpg_by_am = ave(mpg, am, FUN = mean)
#'     hi_low_mpg = ifs(mpg<mean(mpg) ~ 0, default = 1)    
#' })
#' 
#' # set labels
#' .set_var_lab(mpg, "Miles/(US) gallon")
#' .set_var_lab(cyl, "Number of cylinders")
#' .set_var_lab(disp, "Displacement (cu.in.)")
#' .set_var_lab(hp, "Gross horsepower")
#' .set_var_lab(mpg_by_am, "Average mpg for transimission type")
#' .set_var_lab(hi_low_mpg, "Miles per gallon")
#' .set_val_lab(hi_low_mpg, ml_left("
#'                                   0 Low
#'                                   1 High
#'                                   "))
#' 
#' .set_var_lab(vs, "Engine")
#' .set_val_lab(vs, ml_left(" 
#'                           0 V-engine
#'                           1 Straight engine
#'                           "))
#' 
#' .set_var_lab(am, "Transmission")
#' .set_val_lab(am, ml_left(" 
#'                           0 automatic
#'                           1 manual
#'                           "))
#' 
#' # calculate frequencies
#' .fre(hi_low_mpg)
#' .cro(cyl, hi_low_mpg)
#' .cro_mean(mpg, am)
#' .cro_mean(data.frame(mpg, disp, hp), vs)
#' 
#' # disable default dataset
#' default_dataset(NULL)
#' 
#' # Example of .recode
#' 
#' data(iris)
#' 
#' default_dataset(iris) # set iris as default dataset
#' 
#' .recode(Sepal.Length, lo %thru% median(Sepal.Length) ~ "small", . ~ "large")
#' 
#' .fre(Sepal.Length)
#' 
#' # example of .do_if
#'  
#' .do_if(Species == "setosa",{
#'      Petal.Length = NA
#'      Petal.Width = NA
#' })
#' 
#' .cro_mean(data.frame(Petal.Length, Petal.Width), Species)
#' 
#' # disable default dataset
#' default_dataset(NULL)
#' @export
#' @name compute
.modify = function (expr) {
    # based on 'within' from base R by R Core team
    reference = suppressMessages(default_dataset())
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    e$.n = nrow(data)
    e$set = set_generator(e$.n)
    eval(substitute(expr), e)
    rm(".n", "set", envir = e)
    l = as.list(e, all.names = TRUE)
    
    l = l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    del = setdiff(names(data), names(l))
    if(length(del)){
        data[, del] = NULL
    }
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    stopif(any(nrows!=1L & nrows!=nrow(data)),"Bad number of rows")
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    nl = c(names(data), new_vars)
    data[nl] = l[nl]
    ref(reference) = data
    invisible(NULL)
}



#' @export
#' @rdname compute
.modify_if = function (cond, expr) {
    # based on 'within' from base R by R Core team
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    cond = substitute(cond)
    cond = eval(cond, data, parent.frame())
    if (!is.logical(cond)) 
        stop("'cond' must be logical")
    cond = cond & !is.na(cond)
    new_data = data[cond,, drop = FALSE]
    e = evalq(environment(), new_data, parent)
    e$.n = nrow(new_data)
    e$set = set_generator(e$.n)
    eval(substitute(expr), e)
    rm(".n", "set", envir = e)
    l = as.list(e, all.names = TRUE)
    l = l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    del = setdiff(names(data), names(l))
    if(length(del)){
        data[, del] = NULL
    }
    
    nrows = vapply(l, NROW, 1, USE.NAMES = FALSE)
    stopif(any(nrows!=1L & nrows!=nrow(new_data)),"Bad number of rows")
    new_vars = rev(names(l)[!(names(l) %in% names(data))])
    data[cond, names(data)] = l[names(data)]
    data[, new_vars] = NA
    data[cond, new_vars] = l[new_vars]
    ref(reference) = data
    invisible(NULL)
}

# doesn't create new variables
modify_default_dataset_light = function(x, ...){
    expr = as.character(as.expression(sys.call()))
    expr = parse(text = gsub("^\\.","", expr, perl = TRUE))
    for_names = as.expression(substitute(x))
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    e$.n = nrow(data)
    if (length(all.vars(for_names, functions = FALSE))==1 & length(all.vars(for_names, functions = TRUE))==1){
        for_names = as.character(for_names) 
    } else {
        for_names = names(eval(for_names, e))
    }
    stopif(length(for_names)==0, "Something is going wrong. Variables not found: ", deparse((substitute(x))))
    res = eval(expr, e)
    data[, for_names] = res
    ref(reference) = data
    invisible(NULL)
}




#' @export
#' @rdname compute
.do_if = .modify_if


#' @export
#' @rdname compute
.compute = .modify

#' @export
#' @rdname compute
.filter = function (cond) {
    # based on 'within' from base R by R Core team
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    cond = substitute(cond)
    cond = eval(cond, data, parent.frame())
    if (!is.logical(cond)) 
        stop("'cond' must be logical")
    cond = cond & !is.na(cond)
    new_data = data[cond,, drop = FALSE]
    ref(reference) = new_data
    invisible(NULL)
}

# #' @export
# #' @rdname compute
# .select = function (...) {
#     # based on 'within' from base R by R Core team
#     reference = suppressMessages(default_dataset())
#     data = ref(reference)
#     parent = parent.frame()
#     e = evalq(environment(), data, parent)
#     res = eval(substitute(list(...)), e)
# 
#     ref(reference) = as.data.frame(res, stringsAsFactors = FALSE, check.names = FALSE)
#     invisible(NULL)
# }



#' @export
#' @rdname compute
.with = function (expr, ...) {
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    eval(substitute(expr), data, enclos = parent.frame())
}    

#' @export
#' @rdname compute
.val_lab = eval_in_default_dataset

#' @export
#' @rdname compute
.var_lab = eval_in_default_dataset

#' @export
#' @rdname compute
.set_var_lab = modify_default_dataset_light


#' @export
#' @rdname compute
.set_val_lab = modify_default_dataset_light


#' @export
#' @rdname compute
.add_val_lab = modify_default_dataset_light

#' @export
#' @rdname compute
.if_val = modify_default_dataset_light

#' @export
#' @rdname compute
.recode = function(x, ...){
    expr = as.character(as.expression(sys.call()))
    expr = parse(text = gsub("^\\.recode","if_val", expr, perl = TRUE))
    for_names = as.expression(substitute(x))
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    e$.n = nrow(data)
    if (length(all.vars(for_names, functions = FALSE))==1 & length(all.vars(for_names, functions = TRUE))==1){
        for_names = as.character(for_names) 
    } else {
        for_names = names(eval(for_names, e))
    }
    stopif(length(for_names)==0, "Something is going wrong. Variables not found: ", deparse((substitute(x))))
    res = eval(expr, e)
    data[, for_names] = res
    ref(reference) = data
    invisible(NULL)
}

#' @export
#' @rdname compute
.fre = eval_in_default_dataset

#' @export
#' @rdname compute
.cro = eval_in_default_dataset

#' @export
#' @rdname compute
.cro_cpct = eval_in_default_dataset

#' @export
#' @rdname compute
.cro_rpct = eval_in_default_dataset

#' @export
#' @rdname compute
.cro_tpct = eval_in_default_dataset

#' @export
#' @rdname compute
.cro_mean = eval_in_default_dataset

#' @export
#' @rdname compute
.cro_sum = eval_in_default_dataset

#' @export
#' @rdname compute
.cro_median = eval_in_default_dataset

#' @export
#' @rdname compute
.cro_fun = eval_in_default_dataset

#' @export
#' @rdname compute
.cro_fun_df = eval_in_default_dataset



#' @export
#' @rdname compute
.set = function(varnames, value = NA){
    reference = suppressMessages(default_dataset() )
    dd_name = all.vars(reference)
    stopif(length(dd_name)!=1,"Reference should have only one variable name, e. g. ref_var = ~a")
    envir = environment(reference)
    value = eval(substitute(value), envir[[dd_name]], enclos = parent.frame())
    varnames = subst(varnames)
    num_of_vars = length(varnames)
    d_nrows = NROW(envir[[dd_name]])
    value_nrows = NROW(value)
    value_ncols = NCOL(value)
    stopif(value_nrows!=1 & value_nrows!= d_nrows, "Incorrect number of rows in 'value': ", value_nrows, 
           " There are ", d_nrows, " rows in default dataset.")
    stopif(value_ncols!=1 & value_ncols!= num_of_vars, "Incorrect number of columns in 'value': ", value_ncols, 
           " There are ", num_of_vars, " names in 'varnames'.")
    for (each in seq_along(varnames)){
        envir[[dd_name]][, varnames[[each]]] = column(value, each)
    }
    invisible(NULL)
}

set_generator = function(number_of_rows){
    force(number_of_rows)
    function(varnames, value = NA){
        value = eval(substitute(value), envir = parent.frame(), enclos = baseenv())
        varnames = subst(varnames)
        num_of_vars = length(varnames)
        value_nrows = NROW(value)
        value_ncols = NCOL(value)
        stopif(value_nrows!=1 & value_nrows!= number_of_rows, "Incorrect number of rows in 'value': ", value_nrows, 
               " There are ", number_of_rows, " rows in dataset.")
        stopif(value_ncols!=1 & value_ncols!= num_of_vars, "Incorrect number of columns in 'value': ", value_ncols, 
               " There are ", num_of_vars, " names in 'varnames'.")
        if(value_nrows==1){
        for (each in seq_along(varnames)){
            for (each in seq_along(varnames)){
                assign(varnames[[each]], rep(column(value, each), number_of_rows) , pos = parent.frame())
            } 
        }
        } else {
            for (each in seq_along(varnames)){
                assign(varnames[[each]], column(value, each), pos = parent.frame())
            }   
        }
        invisible(NULL)
    }
}

 
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
#' \item{\code{.where}}{ Leave subset of default data.frame which meet
#' condition. See \link{where}, \link[base]{subset}.}
#' \item{\code{.set_var_lab}}{ Deprecated. Use \link{.apply_labels}.}
#' \item{\code{.set_val_lab}}{ Deprecated. Use \link{.apply_labels}.}
#' \item{\code{.add_val_lab}}{ Deprecated. Use \link{.apply_labels}.}
#' \item{\code{.recode}}{ Change, rearrange or consolidate the values of an existing
#' variable inside default data.frame. See \link{recode}.}
#' \item{\code{.if_val}}{ Shortcut for \code{.recode}. See \link{recode}.}
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
#'     hi_low_mpg = ifs(mpg<mean(mpg) ~ 0, TRUE ~ 1)    
#' })
#' 
#' # set labels
#' .apply_labels(
#'     mpg = "Miles/(US) gallon",
#'     cyl = "Number of cylinders",
#'     disp = "Displacement (cu.in.)",
#'     hp = "Gross horsepower",
#'     mpg_by_am = "Average mpg for transimission type",
#'     hi_low_mpg = "Miles per gallon",
#'     hi_low_mpg = num_lab("
#'                      0 Low
#'                      1 High
#'                      "),
#' 
#'     vs = "Engine",
#'     vs = num_lab(" 
#'                      0 V-engine
#'                      1 Straight engine
#'                  "),
#' 
#'     am = "Transmission",
#'     am = num_lab(" 
#'                      0 Automatic
#'                      1 Manual
#'                           ")
#' )
#' # calculate frequencies
#' .fre(hi_low_mpg)
#' .cro(cyl, hi_low_mpg)
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
#' .recode(Sepal.Length, lo %thru% median(Sepal.Length) ~ "small", other ~ "large")
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
#' @name experimental
.modify = function (expr) {
    # based on 'within' from base R by R Core team
    reference = suppressMessages(default_dataset())
    data = ref(reference)
    # expr = substitute(expr)
    data = eval(substitute(modify(data, expr)),
                envir = parent.frame(),
                enclos = baseenv()
                )
    ref(reference) = data
    invisible(data)
}



#' @export
#' @rdname experimental
.modify_if = function (cond, expr) {
    # based on 'within' from base R by R Core team
    reference = suppressMessages(default_dataset())
    data = ref(reference)
    # cond = substitute(cond)
    # expr = substitute(expr)
    data = eval(substitute(modify_if(data, cond, expr)),
                envir = parent.frame(),
                enclos = baseenv()               
               )
    ref(reference) = data
    invisible(data)
}

in_place_if_val = function(x, ..., from = NULL, to = NULL){
    if(is.null(from)){
        if_val(x) = list(...)
    } else {
        if_val(x, from = from) = to
    }
    x
}



# doesn't create new variables
modify_default_dataset_light = function(x, ...){
    expr = as.character(as.expression(sys.call()))
    expr = parse(text = gsub("^\\.","", expr, perl = TRUE))
    if(as.character(expr[[1]][[1]]) %in% c("set_var_lab", "set_val_lab", "add_val_lab")){
        .Deprecated("apply_labels")
    }
    for_names = as.expression(substitute(x))
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    prepare_env(e, n = nrow(data), column_names = colnames(data))
    if (length(all.vars(for_names, functions = FALSE))==1 & length(all.vars(for_names, functions = TRUE))==1){
        for_names = as.character(for_names) 
    } else {
        for_names = names(eval(for_names, e))
    }
    stopif(length(for_names)==0, "Something is going wrong. Variables not found: ", deparse((substitute(x))))
    res = eval(expr, e)
    data[, for_names] = res
    ref(reference) = data
    invisible(data)
}




#' @export
#' @rdname experimental
.do_if = .modify_if


#' @export
#' @rdname experimental
.compute = .modify



#' @export
#' @rdname experimental
.with = function (expr, ...) {
    .Deprecated(".calculate")
    reference = suppressMessages(default_dataset() )
    # expr = substitute(expr)
    data = ref(reference)
    eval(substitute(with(data, expr)), envir = parent.frame(), enclos = baseenv())
}  

#' @export
#' @rdname experimental
.calculate = function (expr, ...) {
    reference = suppressMessages(default_dataset() )
    # expr = substitute(expr)
    data = ref(reference)
    eval(substitute(calculate(data, expr, ...)), envir = parent.frame(), enclos = baseenv())
} 

#' @export
#' @rdname experimental
.calc = .calculate

#' @export
#' @rdname experimental
.val_lab = eval_in_default_dataset

#' @export
#' @rdname experimental
.var_lab = eval_in_default_dataset

#' @export
#' @rdname experimental
.set_var_lab = modify_default_dataset_light


#' @export
#' @rdname experimental
.set_val_lab = modify_default_dataset_light


#' @export
#' @rdname experimental
.add_val_lab = modify_default_dataset_light

#' @export
#' @rdname experimental
.if_val =  function(x, ...){
    expr = as.character(as.expression(sys.call()))
    expr = parse(text = gsub("^\\.(if_val|recode)","expss:::in_place_if_val", expr, perl = TRUE))
    for_names = as.expression(substitute(x))
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    prepare_env(e, n = nrow(data), column_names = colnames(data))
    if (length(all.vars(for_names, functions = FALSE))==1 & length(all.vars(for_names, functions = TRUE))==1){
        for_names = as.character(for_names) 
    } else {
        for_names = names(eval(for_names, e))
    }
    stopif(length(for_names)==0, "Something is going wrong. Variables not found: ", deparse((substitute(x))))
    res = eval(expr, e)
    data[, for_names] = res
    ref(reference) = data
    invisible(data)
}



#' @export
#' @rdname experimental
.recode = .if_val 
    

#' @export
#' @rdname experimental
.fre = eval_in_default_dataset

#' @export
#' @rdname experimental
.cro = eval_in_default_dataset

#' @export
#' @rdname experimental
.cro_cpct = eval_in_default_dataset

#' @export
#' @rdname experimental
.cro_rpct = eval_in_default_dataset

#' @export
#' @rdname experimental
.cro_tpct = eval_in_default_dataset

#' @export
#' @rdname experimental
.cro_mean = eval_in_default_dataset

#' @export
#' @rdname experimental
.cro_sum = eval_in_default_dataset

#' @export
#' @rdname experimental
.cro_median = eval_in_default_dataset

#' @export
#' @rdname experimental
.cro_fun = eval_in_default_dataset

#' @export
#' @rdname experimental
.cro_fun_df = eval_in_default_dataset



#' @export
#' @rdname experimental
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
    stopif(value_nrows!=1 & value_nrows!= d_nrows, paste(varnames, collapse = ","), 
           ": incorrect number of rows in 'value': ", value_nrows, 
           " There are ", d_nrows, " rows in default dataset.")
    stopif(value_ncols!=1 & value_ncols!= num_of_vars, paste(varnames, collapse = ","),
           ": incorrect number of columns in 'value': ", value_ncols, 
           " There are ", num_of_vars, " names in 'varnames'.")
    for (each in seq_along(varnames)){
        envir[[dd_name]][, varnames[[each]]] = column(value, each)
    }
    invisible(ref(reference))
}

set_generator = function(number_of_rows){
    force(number_of_rows)
    function(varnames, value = NA){
        value = eval(substitute(value), envir = parent.frame(), enclos = baseenv())
        varnames = subst(varnames)
        num_of_vars = length(varnames)
        value_nrows = NROW(value)
        value_ncols = NCOL(value)
        stopif(value_nrows!=1 & value_nrows!= number_of_rows, paste(varnames, collapse = ","), 
               ": incorrect number of rows in 'value': ", value_nrows, 
               " There are ", number_of_rows, " rows in dataset.")
        stopif(value_ncols!=1 & value_ncols!= num_of_vars, paste(varnames, collapse = ","), 
               ": incorrect number of columns in 'value': ", value_ncols, 
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

 
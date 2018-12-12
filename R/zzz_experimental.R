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
#' \item{\code{.recode}}{ Change, rearrange or consolidate the values of an existing
#' variable inside default data.frame. See \link{recode}.}
#' \item{\code{.if_val}}{ Shortcut for \code{.recode}. See \link{recode}.} }
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
#' \item{\code{.calculate}}{ Evaluate arbitrary expression in the context of
#' data.frame.  See \link{calculate}.}
#' }
#' @param x vector/data.frame - variable names in the scope of default dataset
#' @param expr set of expressions  in curly brackets which will be evaluated in
#'   the context of default dataset
#' @param cond logical vector/expression
#' @param use_labels logical. Experimental feature. If it equals to \code{TRUE} 
#'   then we will try to replace variable names with labels. Many base R
#'   functions which show variable names will show labels.
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
.compute = function (expr) {
    reference = suppressMessages(default_dataset())
    data = eval.parent(substitute(compute(ref(reference), expr)))
    ref(reference) = data
    invisible(data)
}



#' @export
#' @rdname experimental
.do_if = function (cond, expr) {
    # based on 'within' from base R by R Core team
    reference = suppressMessages(default_dataset())
    data = eval.parent(substitute(do_if(ref(reference), cond, expr)))
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



#' @export
#' @rdname experimental
.modify_if = .do_if 


#' @export
#' @rdname experimental
.modify = .compute 


#' @export
#' @rdname experimental
.calculate = function (expr, use_labels = FALSE) {
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    eval.parent(substitute(calculate(ref(reference), expr, use_labels = use_labels)))
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
    stopif(length(for_names)==0, "Something is going wrong. Variables not found: ", expr_to_character((substitute(x))))
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
.cro_cases = eval_in_default_dataset

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
.cro_mean_sd_n = eval_in_default_dataset

#' @export
#' @rdname experimental
.cro_fun = eval_in_default_dataset

#' @export
#' @rdname experimental
.cro_fun_df = eval_in_default_dataset




#' Subset (filter) data.frames/matrices/vectors/lists
#'
#' For the data frame \code{cond} will be evaluated in the data.frame's context.
#' So columns can be referred as variables in the expression (see the examples).
#' If \code{data} is list then \code{where} will be applied to each element of
#' the list. For other types there is no non-standard evaluation. There is a
#' special constant \code{.N} which equals to number of rows in \code{data} for
#' usage in \code{cond} expression. \code{\%where\%} is infix function with the
#' same functional. See examples. \code{.where} is version for working with
#' default dataset. See \link{default_dataset}.
#'  
#' @param data data.frame/matrix/vector/list to be subsetted
#' @param cond logical or numeric expression indicating elements or rows to 
#'   keep: missing values (NA) are taken as false. If \code{data} is data.frame
#'   then \code{cond} will be evaluated in the scope of the \code{data}.
#'
#' @return data.frame/matrix/vector/list which contains just selected rows.
#' @export
#'
#' @examples
#' # leave only 'setosa'
#' where(iris, Species == "setosa")
#' # leave only first five rows
#' where(iris, 1:5)
#' 
#' # infix version
#' # note that '%where%' have higher precendence than '=='
#' # so we need to put condition inside brackets
#' iris %where% (Species == "setosa")
#' 
#' iris %where% 1:5
#' 
#' # example of .N usage. 
#' set.seed(42)
#' train = iris %where% sample(.N, 100)
#' str(train)
#' 
#' set.seed(42)
#' test = iris %where% -sample(.N, 100)
#' str(test)
#' 
#' # list example
#' set.seed(123)
#' rand_matr = matrix(sample(10, 60, replace = TRUE), ncol = 3)
#' rand_vec = sample(10, 20, replace = TRUE)
#' my_list = list(iris, rand_matr, rand_vec)
#' # two random elements from the each list item
#' where(my_list, sample(.N, 2))
where = function (data, cond) {
    UseMethod("where")
}

#' @export
where.data.frame = function (data, cond) {
    where_data_frame = 1
    cond = substitute(cond)
    eval(bquote(where_helper(data, .(cond))))
}

#' @export
where.default = function (data, cond) {
    where_default = 1
    cond = substitute(cond)
    eval(bquote(where_helper(data, .(cond))))
}


#' @export
where.list = function (data, cond) {
    where_list = 1
    cond = substitute(cond)
    eval(bquote(lapply(data, where, .(cond))))
}


#' @rdname where
#' @export
'%where%' = function(data, cond){
    where_infix = 1
    cond = substitute(cond)
    eval(bquote(where(data, .(cond))))
}


#' @rdname where
#' @export
.where = function (cond) {
    where_dd = 1
    cond = substitute(cond)
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    data = eval(bquote(where(data, .(cond))))
    ref(reference) = data 
    invisible(data)
}

where_helper = function(data, cond){
    where_helper = 1
    cond = substitute(cond)
    if(is.data.frame(data)){
        e = evalq(environment(), data, parent.frame())
    } else {
        e = evalq(environment(), new.env(), parent.frame())
    }
    e$.n = NROW(data)
    e$.N = NROW(data)
    lockBinding(".n", e)
    lockBinding(".N", e)
    cond = eval_dynamic_scoping(cond, e, skip = 3)
    if (!is.logical(cond) && !is.numeric(cond)){ 
        stop("'cond' must be logical or numeric.")
    }    
    if(is.logical(cond)) cond = cond & !is.na(cond)
    if(is.matrix(data) || is.data.frame(data)){
        data[cond,, drop = FALSE]    
    }  else {
        data[cond]
    }  
}
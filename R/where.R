#' Subset (filter) data.frames/matrices/vectors/lists
#'
#' For the data frame \code{cond} will be evaluated in the data.frame's context.
#' So columns can be referred as variables in the expression (see the examples).
#' If \code{data} is list then \code{where} will be applied to each element of 
#' the list. For other types (vector/matrix) there is no non-standard
#' evaluation. There is a special constant \code{.N} which equals to number of
#' rows in \code{data} for usage in \code{cond} expression. \code{.where} is
#' version for working with default dataset. See \link{default_dataset}.
#'  
#' @param data data.frame/matrix/vector/list to be subsetted
#' @param cond logical or numeric expression indicating elements or rows to 
#'   keep: missing values (NA) are taken as FALSE. If \code{data} is data.frame
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
#' 
#' # example of .N usage. 
#' set.seed(42)
#' train = where(iris, sample(.N, 100))
#' str(train)
#' 
#' set.seed(42)
#' test = where(iris, -sample(.N, 100))
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


#' @rdname where
#' @export
.where = function (cond) {
    reference = suppressMessages(default_dataset())
    ref(reference) = eval.parent(substitute(where(ref(reference), cond)))
    invisible(data)
}



#' @export
where.data.frame = function (data, cond) {
    cond = substitute(cond)
    e = evalq(environment(), data, parent.frame())
    prepare_env(e, n = NROW(data), column_names = colnames(data))
    cond = calc_cond(cond, envir = e)
    data[cond,, drop = FALSE] 
}

#' @export
where.default = function (data, cond) {
    cond = substitute(cond)
    e = evalq(environment(), list(), parent.frame())
    prepare_env(e, n = NROW(data), NULL)
    cond = calc_cond(cond, envir = e)
    
    if(is.matrix(data)){
        data[cond,, drop = FALSE]    
    }  else {
        data[cond]
    }  
}

#' @export
where.list = function (data, cond) {
    for(each in seq_along(data)){
        data[[each]] = eval.parent(substitute(where(data[[each]], cond)))
    }
    data
}


# 'cond' is expression - result of 'substitute'
calc_cond = function(cond, envir){
    cond = eval(cond, envir = envir, enclos = baseenv())
    if (!is.logical(cond) && !is.numeric(cond)){ 
        stop("'cond' must be logical or numeric.")
    }    
    if(is.logical(cond)) cond = cond & !is.na(cond)
    cond
}
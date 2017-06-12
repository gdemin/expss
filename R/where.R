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
    cond = substitute(cond)
    parent = parent.frame()
    where_internal(data, cond, parent)
}


#' @rdname where
#' @export
.where = function (cond) {
    cond = substitute(cond)
    parent = parent.frame()
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    ref(reference) = where_internal(data, cond, parent)
    invisible(data)
}

# 'cond' is expression - result of 'substitute'
where_internal = function(data, cond, parent){
    UseMethod("where_internal")    
}

#' @export
where_internal.data.frame = function (data, cond, parent) {
    e = evalq(environment(), data, parent)
    prepare_env(e, n = NROW(data), column_names = colnames(data))
    cond = calc_cond(cond, envir = e)
    data[cond,, drop = FALSE] 
}

#' @export
where_internal.default = function (data, cond, parent) {
    e = evalq(environment(), list(), parent)
    prepare_env(e, n = NROW(data), NULL)
    cond = calc_cond(cond, envir = e)
    
    if(is.matrix(data)){
        data[cond,, drop = FALSE]    
    }  else {
        data[cond]
    }  
}

#' @export
where_internal.list = function (data, cond, parent) {
    for(each in seq_along(data)){
        data[[each]] = where_internal(data[[each]], cond, parent)
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
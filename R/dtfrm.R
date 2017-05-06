#' Make data.frame without conversion to factors and without fixing names
#' 
#' \code{dtfrm} and \code{as.dtfrm} are shortcuts to \code{data.frame} and 
#' \code{as.data.frame} with stringsAsFactors = FALSE, check.names = FALSE.
#' \code{lst} creates list with names.
#' \code{.dtfrm}, \code{.lst} the same as above but work in the scope of default
#' dataset.
#'
#' @param ... objects, possibly named
#' @param x object to be coerced 
#'
#' @return data.frame/list
#' @export
#' @seealso \link{default_dataset}, \link[base]{data.frame}, \link[base]{as.data.frame}, 
#'  \link[base]{list}
#' @examples
#' 
#' # see the difference
#' df1 = data.frame(a = letters[1:3], "This is my long name" = 1:3)
#' df2 = dtfrm(a = letters[1:3], "This is my long name" = 1:3)
#' 
#' str(df1)
#' str(df2)
#' 
#' # lst
#' a = 1:3
#' b = 3:1
#' 
#' list1 = list(a, b)
#' list2 = lst(a, b)
#' 
#' str(list1)
#' str(list2)
#' 
#' data(iris)
#' default_dataset(iris)
#' 
#' .dtfrm(Sepal.Width,  Sepal.Length)
#' .lst(Sepal.Width,  Sepal.Length)
#' 
dtfrm = function(...){
    data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)
}

#' @export
#' @rdname dtfrm
as.dtfrm = function(x, ...) {
    as.data.frame(x, optional = FALSE, check.names = FALSE, ...,
                  cut.names = FALSE, col.names = names(x),
                  stringsAsFactors = FALSE)
    
}

#' @export
#' @rdname dtfrm
lst = function(...){
    res = list(...)
    current_names = names(res)
    possible_names = vapply(as.list(substitute(list(...)))[-1], 
                        FUN = expr_to_character, 
                        FUN.VALUE = character(1),
                        USE.NAMES = FALSE)
    if(!is.null(current_names)){
        names(res)[current_names==""] = possible_names[current_names==""]
    } else {
        names(res) = possible_names
    }
    
    res
 
}

# doesn't modify dataset, just evaluate expression
eval_in_default_dataset = function(...){
    expr = as.character(as.expression(sys.call()))
    expr = parse(text = gsub("^\\.","", expr, perl = TRUE))
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    parent = parent.frame()
    e = evalq(environment(), data, parent)
    prepare_env(e, n = nrow(data), column_names = colnames(data))
    eval(expr, e, enclos = baseenv())
}

#' @export
#' @rdname dtfrm
.dtfrm = eval_in_default_dataset

#' @export
#' @rdname dtfrm
.lst =  eval_in_default_dataset
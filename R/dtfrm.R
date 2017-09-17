#' Make data.frame without conversion to factors and without fixing names
#' 
#' \code{sheet} and \code{as.sheet} are shortcuts to \code{data.frame} and 
#' \code{as.data.frame} with stringsAsFactors = FALSE, check.names = FALSE.
#' \code{lst} creates list with names.
#' \code{.sheet}, \code{.lst} the same as above but work in the scope of default
#' dataset.
#'
#' @param ... objects, possibly named
#' @param x object to be coerced to data.frame
#'
#' @return data.frame/list
#' @export
#' @seealso \link{default_dataset}, \link[base]{data.frame}, \link[base]{as.data.frame}, 
#'  \link[base]{list}
#' @examples
#' 
#' # see the difference
#' df1 = data.frame(a = letters[1:3], "This is my long name" = 1:3)
#' df2 = sheet(a = letters[1:3], "This is my long name" = 1:3)
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
#' .sheet(Sepal.Width,  Sepal.Length)
#' .lst(Sepal.Width,  Sepal.Length)
#' 
sheet = function(...){
    data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)
}

#' @export
#' @rdname sheet
as.sheet = function(x, ...) {
    as.data.frame(x, optional = FALSE, check.names = FALSE, ...,
                  cut.names = FALSE, col.names = names(x),
                  stringsAsFactors = FALSE)
    
}

#' @export
#' @rdname sheet
dtfrm = sheet

#' @export
#' @rdname sheet
as.dtfrm = as.sheet 

#' @export
#' @rdname sheet
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
#' @rdname sheet
.dtfrm = eval_in_default_dataset

#' @export
#' @rdname sheet
.sheet = eval_in_default_dataset

#' @export
#' @rdname sheet
.lst =  eval_in_default_dataset
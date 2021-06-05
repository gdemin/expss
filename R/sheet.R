#' Make data.frame without conversion to factors and without fixing names
#' 
#' \code{sheet} and \code{as.sheet} are shortcuts to \code{data.frame} and 
#' \code{as.data.frame} with stringsAsFactors = FALSE, check.names = FALSE.
#'
#' @param ... objects, possibly named
#' @param x object to be coerced to data.frame
#'
#' @return data.frame/list
#' @export
#' @seealso \link[base]{data.frame}, \link[base]{as.data.frame}
#' @examples
#' 
#' # see the difference
#' df1 = data.frame(a = letters[1:3], "This is my long name" = 1:3)
#' df2 = sheet(a = letters[1:3], "This is my long name" = 1:3)
#' 
#' str(df1)
#' str(df2)
#' 
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




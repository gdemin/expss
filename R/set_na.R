
#' Title
#'
#' @param x 
#' @param value 
#'
#' @return
#'
#' @examples
#' list()
#' @export
set_na = function(x, value){
    if_val(x, from=list(value), to = list(NA))
}

#' @rdname set_na
#' @export
'set_na<-' = function(x, value){
    set_na(x, value)
}

#' Title
#'
#' @param ... vectors/data.frames/lists. Variables which will be used as banner
#'   of the table.
#' @param weight optional numeric vector - weight for tables
#' @param subset optional logical vector specifying a subset of observations to
#'   be used for the table.
#'
#' @return list of variables
#' @export
#'
#' @examples
#' 1 == 1
banner = function(..., weight = NULL, subset = NULL){
    res = lst(...)
    if(!is.null(weight)){
        res$weight = weight
    }
    if(!is.null(subset)){
        res$subset = subset
    }
    res
}



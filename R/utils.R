## similar to stopifnot but with message

stopifnot_message = function(cond,...){
    if (!isTRUE(cond)) {
        # to know name of the parent function
        location = deparse(sys.call(2))
        stop(do.call(paste0,c(list(...),": ",location)),call. = FALSE)
    }
    invisible()
}


#' @import dplyr
NULL
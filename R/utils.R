## stop if condition with message

# TODO Удалить
#' @export
stopif = function(cond,...){
    if (isTRUE(cond)) {
        # to know name of the parent function
        location = deparse(sys.call(2))
#         stop(do.call(paste0,c(list(...),": ",location)),call. = FALSE)
        stop(do.call(paste0,c(list(...))),call. = FALSE)
    }
    invisible()
}


#' @import dplyr
NULL
## similar to stopifnot but with message

stopifnot_message = function(cond,...){
    if (!isTRUE(cond)) {
        # to know name of the parent function
        location = deparse(sys.call(2))
        stop(do.call(paste0,c(list(...),": ",location)),call. = FALSE)
    }
    invisible()
}

function (...) 
{

    mc <- match.call()
    for (i in 1L:n) if (!(is.logical(r <- ll[[i]]) && !anyNA(r) && 
                              all(r))) {
        ch <- deparse(mc[[i + 1]], width.cutoff = 60L)
        if (length(ch) > 1L) 
            ch <- paste(ch[1L], "....")
        stop(sprintf(ngettext(length(r), "%s is not TRUE", "%s are not all TRUE"), 
                     ch), call. = FALSE, domain = NA)
    }
    invisible()
}

#' @import dplyr
NULL
#' Partially (inside blocks) sort tables/data.frames
#' 
#' \code{tab_sort_asc}/\code{tab_sort_desc} sort tables (usually result of 
#' \link{cro}/\link{tables}) in ascending/descending order between specified
#' rows (by default, it is rows which contain '#' in the first column).
#' @param x data.frame 
#' @param ... character/numeric or criteria/logical functions (see 
#'   \link{criteria}). Column names/numbers for data.frame/matrix by which 
#'   object will be sorted. Names at the top-level can be unquoted (non-standard
#'   evaluation). For standard evaluation of parameters you can surround them by
#'   round brackets. See examples. If this argument is missing then table will 
#'   be sorted by second column. Usually second column is the first column with
#'   numbers in the table (there are row labels in the first column).
#' @param excluded_rows character/logical/numeric rows which won't be sorted. 
#'   Rows of the table will be sorted between excluded rows. If it is characters
#'   then they will be considered as pattern/vector of patterns. Patterns will
#'   be matched with Perl-style regular expression with values in the first
#'   column of \code{x} (see \link[base]{grep}, \code{perl = TRUE} argument).
#'   Rows which have such patterns will be excluded. By default, pattern is "#"
#'   because "#" marks totals in the result of \link{cro}.
#' @param na.last for controlling the treatment of NAs. If TRUE, missing values
#'   in the data are put last; if FALSE, they are put first; if NA, they are
#'   removed.
#'
#' @return sorted table('etable')/data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#'
#' # apply labels
#' mtcars = apply_labels(mtcars,
#'     cyl = "Number of cylinders",
#'     vs = "Engine",
#'     vs = c("V-engine" = 0,
#'                     "Straight engine" = 1),
#'     am = "Transmission",
#'     am = c(automatic = 0,
#'                     manual=1),
#'     gear = "Number of forward gears",
#'     carb = "Number of carburetors"
#' )
#' 
#' # without sorting
#' mtcars %>% cross_cpct(list(cyl, gear, carb), list("#total", vs, am))
#' 
#' # with sorting
#' mtcars %>% 
#'     cross_cpct(list(cyl, gear, carb), list("#total", vs, am)) %>% 
#'     tab_sort_desc
#'     
#' # sort by parameter
#' sorting_column = "Engine|V-engine"
#' 
#' mtcars %>% 
#'     cross_cpct(list(cyl, gear, carb), list("#total", vs, am)) %>% 
#'     tab_sort_desc((sorting_column))
#' }    
tab_sort_asc = function(x, ..., excluded_rows = "#", na.last = FALSE){
    UseMethod("tab_sort_asc")
}

#' @export
#' @rdname tab_sort_asc
tab_sort_desc = function(x, ..., excluded_rows = "#", na.last = TRUE){
    UseMethod("tab_sort_desc")
}


#' @export
tab_sort_asc.data.frame = function(x, ..., excluded_rows = "#", na.last = FALSE){
    tab_sort_internal(x = x, ..., excluded_rows = excluded_rows, 
                      na.last = na.last, decreasing = FALSE, envir = parent.frame())
}

#' @export
tab_sort_desc.data.frame = function(x, ..., excluded_rows = "#", na.last = TRUE){
    tab_sort_internal(x = x, ..., excluded_rows = excluded_rows, 
                      na.last = na.last, decreasing = TRUE, envir = parent.frame())
}

tab_sort_internal = function(x, ..., excluded_rows, na.last, decreasing, envir){
    stopif(!is.null(excluded_rows) && !is.numeric(excluded_rows) && !is.logical(excluded_rows) &&
               !is.character(excluded_rows),
           "`excluded_rows` should be character/numeric or logical.")
    if(is.null(excluded_rows)){
        excluded_rows = rep(FALSE, nrow(x))
    } else {
        if(is.numeric(excluded_rows)) {
            stopif(!all(excluded_rows %in% seq_len(nrow(x))) , "some of the 'excluded_rows' not found: ",
                   paste(excluded_rows %d% seq_len(nrow(x)), collapse = ", ")
            )
            
            excluded_rows = seq_len(nrow(x)) %in% excluded_rows
        }
        if(is.character(excluded_rows)){
            excluded_rows = lapply(excluded_rows, grepl, x[[1]], perl = TRUE)
            excluded_rows = Reduce("|", excluded_rows)
        }
        stopif(is.logical(excluded_rows) && (length(excluded_rows) != nrow(x)) ,
               "length of the 'excluded_rows' not equals to number of rows in 'x'"
        )
        stopif(is.logical(excluded_rows) && anyNA(excluded_rows),
               "'excluded_rows' contains NA"
        )
    }
    groups = split(seq_len(nrow(x)), cumsum(excluded_rows))
    for(each in (names(groups) %d% '0')){
        groups[[each]] = groups[[each]][-1]  # remove excluded row in each group
    }
    if(missing(...)){
        for(each_group in groups){
            if(length(each_group)){
                x[each_group, ] = sort_internal(x[each_group, ], 
                                                2,
                                                decreasing = decreasing, 
                                                na.last = na.last, 
                                                envir = envir)    
            }
        }        
        
    } else {
        for(each_group in groups){
            if(length(each_group)){
                x[each_group, ] = sort_internal(x[each_group, ], 
                                                ...,
                                                decreasing = decreasing, 
                                                na.last = na.last, 
                                                envir = envir)    
            }
        }
    }
    x    
}

#' @export
tab_sort_asc.intermediate_table = function(x, ..., excluded_rows = "#", na.last = FALSE){
    result_num = length(x[[RESULT]])
    stopif(result_num==0, 
           "No results for sorting. Use 'tab_sort_*' after 'tab_stat_*' or after 'tab_pivot'.")
    x[[RESULT]][[result_num]] = tab_sort_internal(x[[RESULT]][[result_num]], 
                                                  ..., 
                                                  excluded_rows = excluded_rows, 
                                                  na.last = na.last, 
                                                  decreasing = FALSE, 
                                                  envir = parent.frame()
    )
    x
}

#' @export
tab_sort_desc.intermediate_table = function(x, ..., excluded_rows = "#", na.last = TRUE){
    result_num = length(x[[RESULT]])
    stopif(result_num==0,
           "No results for sorting. Use 'tab_sort_*' after 'tab_stat_*' or after 'tab_pivot'.")
    x[[RESULT]][[result_num]] = tab_sort_internal(x[[RESULT]][[result_num]], 
                                                 ..., 
                                                 excluded_rows = excluded_rows, 
                                                 na.last = na.last, 
                                                 decreasing = TRUE, 
                                                 envir = parent.frame()
    )
    x
}


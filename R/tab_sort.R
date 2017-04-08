#' Partially (inside blocks) sort tables/data.frames
#' 
#' \code{tab_sort_asc}/\code{tab_sort_desc} sort tables (usually result of 
#' \link{cro}/\link{tables}) in ascending/descending order between specified
#' rows (by default, it is row which contain '#' in the first column). There is
#' no non-standard evaluation in these functions by design so use quotes for
#' names of your columns or use \link{qc}.
#' @param x data.frame 
#' @param columns character/numeric. Column names/numbers for data.frame/table
#'   by which object will be sorted. By default it is 2 - the first column with 
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
#' mtcars %>% calculate(cro_cpct(list(cyl, gear, carb), list("#total", vs, am)))
#' 
#' # with sorting
#' mtcars %>% 
#'     calculate(cro_cpct(list(cyl, gear, carb), list("#total", vs, am))) %>% 
#'     tab_sort_desc
tab_sort_asc = function(x, columns = 2, excluded_rows = "#", na.last = FALSE){
    UseMethod("tab_sort_asc")
}

#' @export
#' @rdname tab_sort_asc
tab_sort_desc = function(x, columns = 2, excluded_rows = "#", na.last = TRUE){
    UseMethod("tab_sort_desc")
}


#' @export
tab_sort_asc.data.frame = function(x, columns = 2, excluded_rows = "#", na.last = FALSE){
    tab_sort_internal(x = x, columns = columns, excluded_rows = excluded_rows, na.last = na.last, decreasing = FALSE)
}

#' @export
tab_sort_desc.data.frame = function(x, columns = 2, excluded_rows = "#", na.last = TRUE){
    tab_sort_internal(x = x, columns = columns, excluded_rows = excluded_rows, na.last = na.last, decreasing = TRUE)
}

tab_sort_internal = function(x, columns, excluded_rows, na.last, decreasing){
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
    for(each_group in groups){
        if(length(each_group)){
            x[each_group, ] = sort_internal(x[each_group, ], columns, decreasing = decreasing, na.last = na.last)    
        }
    }
    x    
}

#' @export
tab_sort_asc.intermediate_table = function(x, columns = 2, excluded_rows = "#", na.last = FALSE){
    result_num = length(x[[RESULT]])
    stopif(result_num==0, 
           "No results for sorting. Use 'tab_format_sort_*' after 'tab_stat_*' or after 'tab_pivot'.")
    x[[RESULT]][[result_num]] = tab_sort_asc(x[[RESULT]][[result_num]], 
                                             columns = columns, 
                                             excluded_rows = excluded_rows,
                                             na.last = na.last
    )
    x
}

#' @export
tab_sort_desc.intermediate_table = function(x, columns = 2, excluded_rows = "#", na.last = TRUE){
    result_num = length(x[[RESULT]])
    stopif(result_num==0,
           "No results for sorting. Use 'tab_sort_*' after 'tab_stat_*' or after 'tab_pivot'.")
    x[[RESULT]][[result_num]] = tab_sort_desc(x[[RESULT]][[result_num]], 
                                              columns = columns, 
                                              excluded_rows = excluded_rows,
                                              na.last = na.last
    )
    x
}


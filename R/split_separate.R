#' Splits data.frame into list of data.frames that can be analyzed separately
#' 
#' Splits data.frame into list of data.frames that can be analyzed separately. 
#' These data.frames are sets of cases that have the same values for the 
#' specified split variables. Any missing values in split variables are dropped 
#' together with the corresponding values of \code{data}. \code{split_off} works
#' only with lists of data frames (assumed to have compatible structure).
#' Resulting rows will be sorted in order of the split variables.
#' 
#' @param data data.frame for \code{split_separate}/list for \code{split_off}
#' @param ... unquoted variables names (see \link{keep}) by which \code{data}
#'   will be split into list.
#' @param drop should we drop combination of levels with zero observation? TRUE
#'   by default.
#' @param add_group Should we add group values as \code{.group} variable to
#'   result of \code{split_off}? FALSE by default.
#' @param add_rownames Should we add rownames as \code{.rownames} variable to
#'   result of \code{split_off}? FALSE by default.
#' @return \code{split_separate} returns list of data.frames/\code{split_off}
#'   returns data.frame
#' @export
#' @seealso \link[base]{split}, \link{compute}, \link{calculate},
#'   \link{do_repeat}, \link{where}
#'
#' @examples
#' 
#' # example from base R 'split'
#' data(airquality)
#' airquality2 = airquality %>% 
#'     split_separate(Month) %>% 
#'     compute({
#'         Ozone_zscore = scale(Ozone)
#'     }) %>% 
#'     split_off() 
#' head(airquality2)
#'         
split_separate = function(data, ..., drop = TRUE){
    UseMethod("split_separate")    
}

#' @export
split_separate.data.frame = function(data, ..., drop = TRUE){
    variables_names = substitute(list(...))
    stopif(length(variables_names)==0, "'split_separate' - no grouping variables.")
    splitter = keep_internal(data, variables_names, envir = parent.frame())
    splitter = do.call(nest, splitter)
    if(drop){
        splitter = drop_unused_labels(splitter)
    }
    splitter = fctr(splitter, drop_unused_labels = FALSE, prepend_var_lab = TRUE)
    index = split(seq_len(NROW(data)), splitter)
    lapply(index, function(ind) universal_subset(data, ind, drop = FALSE))
}

#' @export
split_separate.matrix = split_separate.data.frame

#' @export
#' @rdname split_separate
split_off = function(data, add_group = FALSE, add_rownames = FALSE){
    UseMethod("split_off")
}


#' @export
split_off.list = function(data, add_group = FALSE, add_rownames = FALSE){
    groups = names(data)
    if(add_group && !is.null(groups)){
        for(each in seq_along(data)){
            if(NROW(data[[each]])>0) {
                data[[each]][[".group"]] = groups[each]
            }
        }
    }
    if(add_rownames){
        for(each in seq_along(data)){
            if(NROW(data[[each]])>0) {
                data[[each]][[".rownames"]] = rownames(data[[each]])
            }
        }
    }
    res = data.table::rbindlist(data, use.names = TRUE, fill = TRUE)
    if((!is.null(groups) && add_group) || add_rownames){
        columns = NULL
        if(add_group) columns = c(columns, ".group")
        if(add_rownames) columns = c(columns, ".rownames")
        data.table::setcolorder(res, c(columns, setdiff(names(res), columns)))
    }
    data.table::setDF(res)
    res
}



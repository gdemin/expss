#' Splits data.frame into list of data.frames that can be analyzed separately
#' 
#' Splits data.frame into list of data.frames that can be analyzed separately.
#' These data.frames are sets of cases that have the same values for the
#' specified split variables. Any missing values in split variables are dropped
#' together with the corresponding values of \code{data}. \code{split_off} works
#' with lists of data.frames or objects that can be coerced to data.frame and
#' assumed to have compatible structure. Resulting rows will be sorted in order
#' of the split variables. 
#' 
#' @param data data.frame for \code{split_by}/list for \code{split_off}
#' @param ... unquoted variables names (see \link{keep}) by which \code{data}
#'   will be split into list.
#' @param drop should we drop combination of levels with zero observation? TRUE
#'   by default.
#' @param groups character If it is not \code{NULL} then we add list
#'   names as variable to result of \code{split_off} with the name specified by
#'   \code{groups}. If it is \code{TRUE} then name will be \code{.groups}.
#' @param rownames character If it is not \code{NULL} then we add data.frames
#'   rownames as variable to result of \code{split_off} with the name specified
#'   by \code{rownames}. If it is \code{TRUE} then name will be 
#'   \code{.rownames}.
#' @return \code{split_by} returns list of data.frames/\code{split_off}
#'   returns data.frame
#' @export
#' @seealso \link[base]{split}, \link{compute}, \link{calculate},
#'   \link{do_repeat}, \link{where}
#'
#' @examples
#' # example from base R 'split'
#' data(airquality)
#' airquality2 = airquality %>% 
#'     split_by(Month) %>% 
#'     compute({
#'         Ozone_zscore = scale(Ozone)
#'     }) %>% 
#'     split_off() 
#'     
#' head(airquality2)
#' 
#' # usage of 'groups', 'rownames'
#' data(mtcars)
#' # add labels to dataset
#' mtcars %>% 
#'     apply_labels(mpg = "Miles/(US) gallon",
#'                  disp = "Displacement (cu.in.)",
#'                  wt = "Weight",
#'                  hp = "Gross horsepower",
#'                  vs = "Engine",
#'                  vs = num_lab(" 
#'                                    0 V-engine
#'                                    1 Straight engine
#'                                    "),
#'                  
#'                  am = "Transmission",
#'                  am = num_lab(" 
#'                                    0 Automatic
#'                                    1 Manual
#'                                    ")
#'     ) %>% 
#'     split_by(am, vs) %>% 
#'     use_labels({
#'         res = lm(mpg ~ hp + disp + wt)
#'         cbind(Coef. = coef(res), confint(res))
#'     }) %>% 
#'     split_off(groups = TRUE, rownames = "variable")
split_by = function(data, ..., drop = TRUE){
    UseMethod("split_by")    
}

#' @export
#' @rdname split_by
split_separate = function(data, ..., drop = TRUE){
    .Deprecated("split_by")
    eval.parent(substitute(split_by(data, ..., drop = drop)))
}

#' @export
split_by.data.frame = function(data, ..., drop = TRUE){
    variables_names = substitute(list(...))
    stopif(length(variables_names)==0, "'split_by' - no grouping variables.")
    splitter = eval.parent(substitute(keep(data, ...)))
    splitter = do.call(nest, splitter)
    if(drop && is.labelled(splitter)){
        splitter = drop_unused_labels(splitter)
    }
    splitter = fctr(splitter, drop_unused_labels = drop, prepend_var_lab = TRUE)
    index = split(seq_len(NROW(data)), splitter)
    lapply(index, function(ind) universal_subset(data, ind, drop = FALSE))
}

#' @export
split_by.matrix = split_by.data.frame

#' @export
#' @rdname split_by
split_off = function(data, groups = NULL, rownames = NULL){
    UseMethod("split_off")
}


#' @export
split_off.list = function(data, groups = NULL, rownames = NULL){
    first_elem_class = class(data[[1]])
    for(each in seq_along(data)){
        if(!is.data.frame(data[[each]])){
            if(is.vector(data[[each]]) && !is.list(data[[each]])){
                data[[each]] = sheet(value = data[[each]])    
            } else {
                data[[each]] = as.sheet(data[[each]])
            }
        }
        if(is.with_caption(data[[each]])){
            data[[each]] = move_caption_to_first_row(data[[each]])
        }
    }
    if(!is.null(groups)){
        group_names = if_null(names(data), NA)
        stopif(length(groups)!=1, "'split_off' - 'groups' should be vector of length 1.")
        if("etable" %in% first_elem_class){
            if(isTRUE(groups)){
                groups = NULL
            } else {
                
                groups = paste0(as.character(groups), "|")
                
            }
            for(each in seq_along(data)){
                if(NROW(data[[each]])>0) {
                    data[[each]][[1]] = paste0(groups, group_names[each], "|", data[[each]][[1]])
                }
            }
        } else {
            if(isTRUE(groups)){
                groups = ".groups"
            } else {
                groups = as.character(groups)
            }
            for(each in seq_along(data)){
                if(NROW(data[[each]])>0) {
                    data[[each]][[groups]] = group_names[each]
                }
            }
        }
    }
    if(!(is.null(rownames) || ("etable" %in% first_elem_class))){
        stopif(length(rownames)!=1, "'split_off' - 'rownames' should be vector of length 1.")
        if(isTRUE(rownames)) {
            rownames = ".rownames"
        } else {
            rownames = as.character(rownames)
        }
        for(each in seq_along(data)){
            if(NROW(data[[each]])>0) {
                data[[each]][[rownames]] = row.names(data[[each]])
            }
        }
        
    }
    res = data.table::rbindlist(data, use.names = TRUE, fill = TRUE)
    if(!("etable" %in% first_elem_class) & (!is.null(groups) || !is.null(rownames))){
        columns = c(groups, rownames)
        data.table::setcolorder(res, c(columns, setdiff(names(res), columns)))
    }
    if(!data.table::is.data.table(data[[1]])){
        data.table::setDF(res)
        if("data.frame" %in% first_elem_class){
            class(res) = first_elem_class
        }
    }
    res
}



move_caption_to_first_row = function(etab){
    caption = get_caption(etab)
    etab = set_caption(etab, NULL)
    caption = setNames(sheet(caption), colnames(etab)[1])
    add_rows(caption, etab)
    
}
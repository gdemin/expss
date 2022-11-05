#' Prepend values/variable names to value/variable labels
#' 
#' These functions add values/variable names as prefixes to value/variable 
#' labels. Functions which start with \code{tab_} intended for usage inside 
#' table creation sequences. See examples and \link{tables}. It is recommended
#' to use \code{tab_prepend_*} at the start of sequence of tables creation. If
#' you use it in the middle of the sequence then previous statements will not be
#' affected.
#' 
#' @param x vector/data.frame. \code{prepend_names} can be applied only to data.frames.
#' @param data data.frame/intermediate result of tables construction. See \link{tables}.
#'
#' @return original object with prepended names/values to labels
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       mpg = "Miles/(US) gallon",
#'                       cyl = "Number of cylinders",
#'                       disp = "Displacement (cu.in.)",
#'                       hp = "Gross horsepower",
#'                       drat = "Rear axle ratio",
#'                       wt = "Weight (lb/1000)",
#'                       qsec = "1/4 mile time",
#'                       vs = "Engine",
#'                       vs = c("V-engine" = 0,
#'                              "Straight engine" = 1),
#'                       am = "Transmission",
#'                       am = c("Automatic" = 0,
#'                              "Manual"=1),
#'                       gear = "Number of forward gears",
#'                       carb = "Number of carburetors"
#' )
#' 
#' # prepend names and 'cross_cpct'
#' mtcars %>% 
#'        prepend_names %>% 
#'        cross_cpct(list(cyl, gear), list(total(), vs, am))
#'      
#' # prepend values to value labels                 
#' mtcars %>% 
#'    tab_prepend_values %>% 
#'    tab_cols(total(), vs, am) %>% 
#'    tab_cells(cyl, gear) %>% 
#'    tab_stat_cpct() %>% 
#'    tab_pivot()
#'
#' # prepend names and labels
#' mtcars %>% 
#'    tab_prepend_all %>% 
#'    tab_cols(total(), vs, am) %>% 
#'    tab_cells(cyl, gear) %>% 
#'    tab_stat_cpct() %>% 
#'    tab_pivot() 
#'    
#' # variable in rows without prefixes
#' mtcars %>% 
#'    tab_cells(cyl, gear) %>% 
#'    tab_prepend_all %>% 
#'    tab_cols(total(), vs, am) %>% 
#'    tab_stat_cpct() %>% 
#'    tab_pivot()  
#'    } 
prepend_values = function(x){
    UseMethod("prepend_values")
}

#' @export
prepend_values.default = function(x){
    vallab = val_lab(x)
    if(!is.null(vallab)){
        names(vallab) = paste(vallab, names(vallab), sep = " ")
        set_val_lab(x, vallab)
    } else {
        x
    }
}

#' @export
prepend_values.list = function(x){
    for(each in seq_along(x)){
        x[[each]] = prepend_values(x[[each]])
    }
    x
}


#' @export
prepend_values.data.frame = prepend_values.list

#############################################

#' @export
#' @rdname prepend_values
prepend_names = function(x){
    UseMethod("prepend_names")
}

#' @export
prepend_names.default = function(x){
    stop("'prepend_names' implemented only for data.frames")
}

#' @export
prepend_names.data.frame = function(x){
    all_names = colnames(x)
    for(each in seq_along(x)){
        varlab = var_lab(x[[each]])
        currname = all_names[[each]]
        if(is.null(varlab)){
            var_lab(x[[each]]) = currname
        } else {
            var_lab(x[[each]]) = paste(currname, varlab, sep = " ")
        }
    }
    x
}

##################################################

#' @export
#' @rdname prepend_values
prepend_all = function(x){
    UseMethod("prepend_all")
}

#' @export
prepend_all.default = function(x){
    prepend_values(x)
}

#' @export
prepend_all.list = function(x){
    x = prepend_values(x)
    x = prepend_names(x)
    x
}

#' @export
prepend_all.data.frame = prepend_all.list

########################################

#' @export
#' @rdname prepend_values
tab_prepend_values = function(data){
    UseMethod("tab_prepend_values")
}


#' @export
tab_prepend_values.data.frame = function(data){
    res = make_empty_intermediate_table(data)
    tab_prepend_values(res)
}

#' @export
tab_prepend_values.intermediate_table = function(data){
    data[["data"]] = prepend_values(data[["data"]])
    data
}

##########################################

#' @export
#' @rdname prepend_values
tab_prepend_names = function(data){
    UseMethod("tab_prepend_names")
}


#' @export
tab_prepend_names.data.frame = function(data){
    res = make_empty_intermediate_table(data)
    tab_prepend_names(res)
}

#' @export
tab_prepend_names.intermediate_table = function(data){
    data[["data"]] = prepend_names(data[["data"]])
    data
}

##########################################

#' @export
#' @rdname prepend_values
tab_prepend_all = function(data){
    UseMethod("tab_prepend_all")
}


#' @export
tab_prepend_all.data.frame = function(data){
    res = make_empty_intermediate_table(data)
    tab_prepend_all(res)
}

#' @export
tab_prepend_all.intermediate_table = function(data){
    data[["data"]] = prepend_all(data[["data"]])
    data
}

##########################################
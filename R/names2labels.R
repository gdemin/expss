#' Replace data.frame/list names with corresponding variables labels.
#' 
#' \code{names2labels} replaces data.frame/list names with corresponding 
#' variables labels. If there are no labels for some variables their names
#' remain unchanged. \code{n2l} is just shortcut for \code{names2labels}.
#' @param x data.frame/list.
#' @param exclude logical/integer/character columns which names should be left
#'   unchanged. Only applicable to list/data.frame.
#' @param keep_names logical. If TRUE original column names will be kept with
#'   labels. Only applicable to list/data.frame.
#' @return Object of the same type as x but with variable labels instead of
#'   names. 
#' @seealso \link{values2labels}, \link{val_lab},  \link{var_lab}
#' @examples
#' data(mtcars)
#' mtcars = modify(mtcars,{
#'                 var_lab(mpg) = "Miles/(US) gallon"
#'                 var_lab(cyl) = "Number of cylinders"
#'                 var_lab(disp) = "Displacement (cu.in.)"
#'                 var_lab(hp) = "Gross horsepower"
#'                 var_lab(drat) = "Rear axle ratio"
#'                 var_lab(wt) = "Weight (lb/1000)"
#'                 var_lab(qsec) = "1/4 mile time"
#'                 var_lab(vs) = "V/S"
#'                 var_lab(am) = "Transmission (0 = automatic, 1 = manual)"
#'                 var_lab(gear) = "Number of forward gears"
#'                 var_lab(carb) = "Number of carburetors"
#' })
#' 
#' # without original names
#' # note: we exclude dependent variable 'mpg' from conversion to use its short name in formula
#' summary(lm(mpg ~ ., data = names2labels(mtcars, exclude = "mpg")))
#' # with names
#' summary(lm(mpg ~ ., data = names2labels(mtcars, exclude = "mpg", keep_names = TRUE)))
#' @export
names2labels = function(x, exclude = NULL, keep_names = FALSE){
    UseMethod("names2labels")
    
}

#' @export
names2labels.default = function(x, exclude = NULL, keep_names = FALSE){
    lab = var_lab(x)
    x = as.matrix(x)
    if (is.null(lab) || (lab=="")) return(x)
    colnames(x) = lab
    x
}

#' @export
names2labels.list = function(x, exclude = NULL, keep_names = FALSE){
    labs = lapply(x, var_lab)
    no_labs = sapply(labs,function(each) is.null(each) || (each==""))
    if (length(exclude)>0) {
        if(is.logical(exclude)){
            stopif(length(exclude)!=length(x), 
                   "Length of logical exclude should be equal length of x but length(exclude) = " ,length(exclude), " and ncol(x)=", length(x))
            include = !exclude
        
            } else {
            if (is.character(exclude)){
                stopif(!all(exclude %in% names(x)), 
                       "Some names in 'exclude' argument doesn't found in 'x': " , setdiff(exclude, names(x)))
                include = !(names(x) %in% exclude)
            } else {
                stopif(!is.numeric(exclude), "'exclude' argument should be logical, numeric or character but: ", exclude)
                stopif(max(exclude, na.rm = TRUE)>length(x), 
                       "max(exclude) greater than number of elements in 'x': ", max(max(exclude, na.rm = TRUE))," vs ", length(x))
                include = !(seq_along(x) %in% exclude)
                
            }
        }
    } else {
        include = TRUE
    }
    include = !no_labs & include
    labs = unlist(labs[include])
    if (keep_names) labs = paste0(names(x)[include], " ", labs)
    names(x)[include] = labs
    x
}

#' @export
names2labels.data.frame = function(x, exclude = NULL, keep_names = FALSE){
    names2labels.list(x = x, exclude = exclude, keep_names = keep_names)    
    
}




#' @export
#' @rdname names2labels
n2l = names2labels

### set variable label to variable name if label is absent
make_labels_from_names = function(x){
    UseMethod("make_labels_from_names")
}

#' @export
make_labels_from_names.default = function(x){
   x
}

#' @export
make_labels_from_names.data.frame = function(x){
    for(each in seq_along(x)){
        if(is.null(var_lab(x[[each]]))){
            var_lab(x[[each]]) = names(x)[each]
        }
    }
    x
}

#' @export
make_labels_from_names.list = function(x){
    list_names = names(x)
    for(each in seq_along(x)){
        if(!is.null(list_names) && !is.matrix(x) && !is.data.frame(x) && is.null(var_lab(x[[each]]))){
            var_lab(x[[each]]) = list_names[each]
        } else {
            if(is.data.frame(x) || is.list(x)){
                x[[each]] = make_labels_from_names(x[[each]])
            }            
        }
    }
    x
}

make_value_labels_from_names = function(x){
    for(each in seq_along(x)){
        curr_lab = if_null(var_lab(x[[each]]), names(x)[each])
        x[[each]] = set_val_lab(x[[each]], setNames(1, curr_lab))
        x[[each]] = unvr(x[[each]])
    }
    x
}
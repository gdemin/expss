#' Replace data.frame/list names with corresponding variables labels.
#' 
#' \code{names2labels} replaces data.frame/list names with corresponding 
#' variables labels. If there are no labels for some variables their names
#' remain unchanged. \code{n2l} is just shortcut for \code{names2labels}.
#' @param x data.frame/list.
#' @param exclude logical/integer/character columns which names should be left
#'   unchanged. Only applicable to list/data.frame.
#' @param keep_names logical. If TRUE original column names will be appended to
#'   labels in round brackets. Only applicable to list/data.frame.
#' @return Object of the same type as x but with variable labels instead of
#'   names. 
#' @seealso \link{values2labels}, \link{f}, \link{val_lab},  \link{var_lab}
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
    if (keep_names) labs = paste0(labs," (",names(x)[include],")")
    names(x)[include] = labs
    x
}

#' @export
names2labels.data.frame = function(x, exclude = NULL, keep_names = FALSE){
    names2labels.list(x = x, exclude = exclude, keep_names = keep_names)    
    
}

#' @export
names2labels.matrix = function(x, exclude = NULL, keep_names = FALSE){
    lab = var_lab(x)
    if (is.null(lab) || (lab=="")) return(x)
    clm = colnames(x)
    if (is.null(clm)) clm = seq_len(ncol(x))
    colnames(x) = paste0(lab,LABELS_SEP,clm)
    x

}


#' @export
#' @rdname names2labels
n2l = names2labels
#' Replace data.frame/list names with corresponding variables labels.
#' 
#' @param x data.frame/list.
#' @param keep_names logical. If TRUE original names will be appended to labels in round brackets.
#' @return Object of the same type as x but with variable labels instead of names. If there are no labels for
#'  some variables their names remain unchanged. 
#' @seealso \code{\link{values2labels}}
#' @examples
#' data(mtcars)
#' mtcars = within(mtcars,{
#'                 var_lab(mpg) = NULL
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
#' summary(lm(mpg ~ ., data = names2labels(mtcars)))
#' # with names
#' summary(lm(mpg ~ ., data = names2labels(mtcars, keep_names = TRUE)))
#' @export
names2labels = function(x, keep_names = FALSE){
    UseMethod("names2labels")
    
}

#' @export
names2labels.default = function(x, keep_names = FALSE){
    x
}

#' @export
names2labels.list = function(x, keep_names = FALSE){
    labs = lapply(x, var_lab)
    no_labs = sapply(labs,function(each) is.null(each) || (each==""))
    labs = unlist(labs[!no_labs])
    if (keep_names) labs = paste0(labs," (",names(x)[!no_labs],")")
    names(x)[!no_labs] = labs
    x
}

#' @export
names2labels.data.frame = function(x, keep_names = FALSE){
    names2labels.list(x,keep_names)    
    
}

#' @export
names2labels.matrix = function(x, keep_names = FALSE){
    lab = var_lab(x)
    if (is.null(lab) || (lab=="")) return(x)
    clm = colnames(x)
    if (is.null(clm)) clm = seq_len(ncol(x))
    colnames(x) = paste0(lab,labels_sep,clm)
    x

}
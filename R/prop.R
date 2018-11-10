#' Compute proportions from numeric vector/matrix/data.frame
#' 
#' \code{prop} returns proportion to sum of entire \code{x}. \code{prop_col} 
#' returns proportion to sum of each column of \code{x}. \code{prop_row} returns
#' proportion to sum of each row of \code{x}. Non-numeric columns in the
#' data.frame are ignored. NA's are also ignored.
#'
#' @param x numeric vector/matrix/data.frame
#'
#' @return the same structure as \code{x} but with proportions of original
#'   values from sum of original values.
#' @export
#' @examples
#' a = c(25, 25, NA)
#' prop(a)
#' 
#' # data.frame with non-numeric columns
#' fac = factor(c("a", "b", "c"))
#' char = c("a", "b", "c")
#' dat = as.POSIXct("2016-09-27") 
#' a = sheet(fac, a = c(25, 25, NA), b = c(100, NA, 50), char, dat)
#' 
#' prop(a)
#' prop_row(a)
#' prop_col(a)
#' 
#' # the same as result as with 'prop.table'
#' tbl = table(state.division, state.region)
#' 
#' prop(tbl)
#' prop_row(tbl)
#' prop_col(tbl)
prop = function(x){
    UseMethod("prop")
}

#' @export
prop.default = function(x){
   x/sum(x, na.rm = TRUE) 
}

#' @export
prop.data.frame = function(x){
    numerics = vapply(x, 
                      correct_numeric, 
                      FUN.VALUE = NA, 
                      USE.NAMES = FALSE
    )
    total = sum(x[, numerics], na.rm = TRUE)
    x[, numerics] = x[, numerics]/total 
    x
}

#' @export
prop.list = function(x){
    for(each in seq_along(x)){
        x[[each]] = prop(x[[each]])
    }
    x
}


#' @export
#' @rdname prop
prop_col = function(x){
    UseMethod("prop_col")
}

#' @export
prop_col.default = function(x){
    x/sum(x, na.rm = TRUE) 
}

#' @export
prop_col.data.frame = function(x){
    numerics = vapply(x, 
                      correct_numeric, 
                      FUN.VALUE = NA, 
                      USE.NAMES = FALSE
    )
    
    x[, numerics] = lapply(x[, numerics, drop = FALSE], prop_col) 
    x
}

#' @export
prop_col.matrix = function(x){
    total = colSums(x, na.rm = TRUE)
    for(each in seq_len(NCOL(x))){
        x[, each] = x[, each]/total[each]     
    }
    x
}

#' @export
prop_col.table = function(x){
    total = colSums(x, na.rm = TRUE)
    for(each in seq_len(NCOL(x))){
        x[, each] = x[, each]/total[each]     
    }
    x
}

#' @export
prop_col.list = function(x){
    for(each in seq_along(x)){
        x[[each]] = prop_col(x[[each]])
    }
    x
}

#' @export
#' @rdname prop
prop_row = function(x){
    UseMethod("prop_row")
}

#' @export
prop_row.default = function(x){
    # yes, it is. each row in vector has only one element so sum of row elements is equal to this single element
    x/x 
}


#' @export
prop_row.data.frame = function(x){
    numerics = vapply(x, 
                      correct_numeric, 
                      FUN.VALUE = NA, 
                      USE.NAMES = FALSE
    )
    
    x[, numerics] = x[, numerics, drop = FALSE]/rowSums(x[, numerics, drop = FALSE], na.rm = TRUE)
    x
}

#' @export
prop_row.matrix = function(x){
    total = rowSums(x, na.rm = TRUE)
    x/total
}

#' @export
prop_row.table = function(x){
    total = rowSums(x, na.rm = TRUE)
    x/total
}

#' @export
prop_row.list = function(x){
    for(each in seq_along(x)){
        x[[each]] = prop_row(x[[each]])
    }
    x
}


correct_numeric = function(x) {
    !is.factor(x) && 
        !("POSIXct" %in% class(x)) && 
        (mode(x) %in% c("numeric","logical"))
}

#' Replace NA values in vector/data.frame/matrix/list with supplied value
#'
#' @param x 
#' @param value 
#'
#' @return
#' @export
#'
#' @examples
if_na = function(x, value){
    UseMethod("if_na")
}

#' @export
'if_na<-' = function(x, value){
    if_na(x, value)
}

#' @export
if_na.default = function(x, value){
    check_conformance(x, value)
    if (anyNA(x)){
        nas = is.na(x)
        if(NROW(value)>1){
            x[nas] = value[nas]
        } else {
            x[nas] = value
        }
    }
    x
}

#' @export
if_na.data.frame = function(x, value){
    check_conformance(x, value)
    if(NCOL(value)>1) {
        if(is.matrix(value)){
            for(each in seq_along(x)){
                if_na(x[[each]]) = value[,each]
            }
        } else {
            for(each in seq_along(x)){
                if_na(x[[each]]) = value[[each]]
            }    
        }
    } else {
        for(each in seq_along(x)){
            if_na(x[[each]]) = value
        } 
    }
    x
}

#' @export
if_na.matrix = function(x, value){
    check_conformance(x, value)
    if(anyNA(x)){
        nas = is.na(x)
        if(NCOL(value)>1 && NROW(value)>1) {
            value = as.matrix(value)
            x[nas] = value[nas]
        }
        if(NCOL(value)<2 && NROW(value)<2) {
            x[nas] = value
        }
        if(NCOL(value)<2 && NROW(value)>1) {
            for(each in seq_len(ncol(x))){
                x[nas[,each],each] = value[nas[,each]]
            }
        }  
        if(NCOL(value)>1 && NROW(value)<2) {
            for(each in seq_len(ncol(x))){
                x[nas[,each],each] = value[,each]
            }
        } 
    }
    x
}

#' @export
if_na.list = function(x, value){
    for(each in seq_along(x)){
        if_na(x[[each]]) = value
    }
    x
}

# value should be ncol(value)==1 or ncol(value) = ncol(x) 
# value should be nrow(value)==1 or nrow(value) = nrow(x) 
check_conformance = function(x,value){
    UseMethod("check_conformance")
}

check_conformance.default = function(x,value){
    stopif(length(value)==0, "'value' has zero length.")
    stopif(NCOL(value)>1 && NCOL(x)!=NCOL(value), "Number of columns in 'value' should be 1 or equal 'x' number of columns.
           Now NCOL(value)=",NCOL(value)," NCOL(x)=", NCOL(x))
    stopif(NROW(value)>1 && NROW(x)!=NROW(value), "Number of rows in 'value' should be 1 or equal 'x' number of rows.
           Now NROW(value)=",NROW(value)," NROW(x)=", NROW(x))
    invisible(TRUE)
}
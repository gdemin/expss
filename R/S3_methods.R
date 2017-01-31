
# this entire method for compatibility with other packages where 
# "labelled' is single class rather than c("labelled", "numeric") etc.
#' @export
as.data.frame.labelled = function(x_flat, ..., nm = paste(deparse(substitute(x_flat), width.cutoff = 500L)) ){
    if(length(class(x_flat))>1){
        # because we can have labelled matrices or factors with variable label
        NextMethod("as.data.frame", ..., nm = nm, stringsAsFactors = FALSE)
        
    } else {
        # this branch for other packages where "labelled' is single class rather than c("labelled", "numeric") etc.
        
        as.data.frame.vector(x_flat, ..., nm = nm, stringsAsFactors = FALSE)
    }
}

#' @export
c.labelled = function(..., recursive = FALSE)
    ### concatenate vectors of class 'labelled' and preserve labels
{
    y = NextMethod("c")
    vectors=list(...)
    dummy= lapply(vectors,var_lab)
    dummy=dummy[lengths(dummy)>0]
    if (length(dummy)>0) var_lab(y)=dummy[[1]]
    
    dummy= lapply(vectors,val_lab)
    val_lab(y)=do.call(combine_labels,dummy)
    class(y) = union("labelled",class(y))
    y
}



#' @export
rep.labelled = function (x_flat, ...){
    y=NextMethod("rep")
    var_attr(y)=var_attr(x_flat)
    class(y) = class(x_flat)
    y	
}

#' @export
'[.labelled' = function (x_flat, ...){
    y = NextMethod("[")
    # browser()
    # class(x_flat) = setdiff(class(x_flat), "labelled")
    # y = x_flat[...]
    var_attr(y)=var_attr(x_flat)
    # class(y) = union("labelled", class(x_flat))
    class(y) = class(x_flat)
    y
}

#' @export
'[[.labelled' = function (x_flat, ...){
    y = NextMethod("[[")
    # browser()
    # class(x_flat) = setdiff(class(x_flat), "labelled")
    # y = x_flat[...]
    var_attr(y)=var_attr(x_flat)
    # class(y) = union("labelled", class(x_flat))
    class(y) = class(x_flat)
    y
}

# it is needed to prevent state with inconsistent class and mode
# (such as 'numeric' in class but mode is character)
#' @export
'[<-.labelled' = function (x_flat, ..., value){
    class(x_flat) = setdiff(class(x_flat), "labelled")
    y = NextMethod("[<-")
    class(y) = c("labelled", class(y))
    y
}

#' @export
'[[<-.labelled' = function (x_flat, ..., value){
    class(x_flat) = setdiff(class(x_flat), "labelled")
    y = NextMethod("[[<-")
    class(y) = c("labelled", class(y))
    y
}

var_attr = function(x_flat){
    list(label=var_lab(x_flat),labels=val_lab(x_flat))
}

'var_attr<-' = function(x_flat,value){
    var_lab(x_flat)=value[["label"]]
    val_lab(x_flat)=value[["labels"]]
    x_flat
}

### All subsetting methods are so strange because
### NextMethod doesn't work and I don't know why 
#' @export
"[.simple_table" = function(x_flat, i, j, drop = FALSE){
    subset_helper(x_flat, i, j, drop, class_name = "simple_table")
}


#' @export
"[.summary_table" = function(x_flat, i, j, drop = FALSE){
    subset_helper(x_flat, i, j, drop, class_name = "summary_table")
}



#' @export
"[.etable" = function(x_flat, i, j, drop = FALSE){
    subset_helper(x_flat, i, j, drop, class_name = "etable")
}

subset_helper = function(x_flat, i, j, drop, class_name){
    class(x_flat) = setdiff(class(x_flat), class_name)
    res = x_flat[i, j,  drop = drop]     
    if(!drop) class(res) = union(class_name, class(res))
    res    
}


# it's strange but I cannot make to work "NextMethod"
# #' @export
# "[.category" = function(x_flat, i, j, drop = FALSE){
#     subset_helper(x_flat, i, j, drop, class_name = "category")
# }
# 
# #' @export
# "[.dichotomy" = function(x_flat, i, j, drop = FALSE){
#     subset_helper(x_flat, i, j, drop, class_name = "dichotomy")
# }



#' @export
as.double.labelled = function (x_flat, ...){
    y = NextMethod("as.double")
    var_attr(y)=var_attr(x_flat)
    class(y) = union("labelled", class(y))
    y	
}

#' @export
as.integer.labelled = function (x_flat, ...){
    y = NextMethod("as.integer")
    var_attr(y)=var_attr(x_flat)
    class(y) = union("labelled", class(y))
    y	
}

#' @export
as.character.labelled = function (x_flat, ...){
    y = NextMethod("as.character")
    var_attr(y)=var_attr(x_flat)
    class(y) = union("labelled", class(y))
    y	
}

#' @export
as.logical.labelled = function (x_flat, ...){
    y = NextMethod("as.logical")
    var_attr(y)=var_attr(x_flat)
    class(y) = union("labelled", class(y))
    y	
}

#' @export
print.labelled = function(x, max = 50, max_labels = 20, ...){
    varlab = var_lab(x)
    vallab = val_lab(x)
    if(is.list(x)){
        x_flat = unname(c(x, recursive = TRUE))
    }

    if(!is.null(varlab)){
        cat('LABEL:', varlab, "\n")

    }
    cat("VALUES:\n")
    cat(unlab(head(x_flat, max)))
    if(max < NROW(x_flat)) {
        cat("... only", max, "printed from", NROW(x_flat),"items.\n")
    } else {
        cat("\n")
    }
    if(!is.null(vallab)){
        vallab = sort_asc(dtfrm(Value = vallab, Label = names(vallab)),"Value") 
        vallab = setNames(vallab, NULL)
        # colnames(vallab) = gsub(".", " ", colnames(vallab), perl = TRUE)
        cat("VALUE LABELS:")
        if(max_labels < nrow(vallab)){
            max_labels  = floor(max_labels/2)
            print(head(vallab, max_labels), row.names = FALSE, right = FALSE)
            cat("\n  ...\n")
            tail_vallab = tail(vallab, max_labels)
            
            print(tail_vallab, row.names = FALSE, right = FALSE)
            
        } else {
            print(vallab, row.names = FALSE, right = FALSE)   
        }
        
        
    }
    
    invisible(x)
}




# this entire method for compatibility with other packages where 
# "labelled' is single class rather than c("labelled", "numeric") etc.
#' @export
as.data.frame.labelled = function(x, ..., nm = paste(deparse(substitute(x), width.cutoff = 500L)) ){
    if(length(class(x))>1){
        # because we can have labelled matrices or factors with variable label
        NextMethod("as.data.frame", ..., nm = nm, stringsAsFactors = FALSE)
        
    } else {
        # this branch for other packages where "labelled' is single class rather than c("labelled", "numeric") etc.
        
        as.data.frame.vector(x, ..., nm = nm, stringsAsFactors = FALSE)
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
rep.labelled = function (x, ...){
    y= NextMethod("rep")
    var_attr(y)=var_attr(x)
    class(y) = class(x)
    y	
}

#' @export
'[.labelled' = function (x, ...){
    y = NextMethod("[", ...)
    # browser()
    # class(x) = setdiff(class(x), "labelled")
    # y = `[`(x, ...)
    var_attr(y)=var_attr(x)
    class(y) = union("labelled", class(x))
    # class(y) = class(x)
    y
}

#' @export
'[[.labelled' = function (x, ...){
    y = NextMethod("[[")
    # browser()
    # class(x) = setdiff(class(x), "labelled")
    # y = x[...]
    var_attr(y)=var_attr(x)
    class(y) = union("labelled", class(x))
    # class(y) = class(x)
    y
}

# it is needed to prevent state with inconsistent class and mode
# (such as 'numeric' in class but mode is character)
#' @export
'[<-.labelled' = function (x, ..., value){
    class(x) = setdiff(class(x), "labelled")
    y = NextMethod("[<-")
    class(y) = c("labelled", class(y))
    y
}

#' @export
'[[<-.labelled' = function (x, ..., value){
    class(x) = setdiff(class(x), "labelled")
    y = NextMethod("[[<-")
    class(y) = c("labelled", class(y))
    y
}

var_attr = function(x){
    list(label=var_lab(x),labels=val_lab(x))
}

'var_attr<-' = function(x,value){
    var_lab(x)=value[["label"]]
    val_lab(x)=value[["labels"]]
    x
}

### All subsetting methods are so strange because
### NextMethod doesn't work and I don't know why 


#' @export
"[.etable" = function(x, i, j, drop = FALSE){
    subset_helper(x, i, j, drop, class_name = "etable")
}

subset_helper = function(x, i, j, drop, class_name){
    class(x) = setdiff(class(x), class_name)

    
    res = x[i, j,  drop = drop]
    # to preserve column names
    if(is.data.frame(res)){
        old_names = colnames(x)
        names(old_names) = old_names # when j is character
        old_names = old_names[j]
        colnames(res) = old_names
    }
    if(!drop) class(res) = union(class_name, class(res))
    res    
}


# it's strange but I cannot make to work "NextMethod"
# #' @export
# "[.category" = function(x, i, j, drop = FALSE){
#     subset_helper(x, i, j, drop, class_name = "category")
# }
# 
# #' @export
# "[.dichotomy" = function(x, i, j, drop = FALSE){
#     subset_helper(x, i, j, drop, class_name = "dichotomy")
# }



#' @export
as.double.labelled = function (x, ...){
    y = NextMethod("as.double")
    var_attr(y)=var_attr(x)
    class(y) = union("labelled", class(y))
    y	
}

#' @export
as.integer.labelled = function (x, ...){
    y = NextMethod("as.integer")
    var_attr(y)=var_attr(x)
    class(y) = union("labelled", class(y))
    y	
}

#' @export
as.numeric.labelled = as.double.labelled

#' @export
as.character.labelled = function (x, ...){
    if(!identical(getOption("expss.enable_value_labels_support"), 0)){
        prepend_varlab = isTRUE(getOption("expss.prepend_var_lab"))
        labelled_to_character_internal(x, prepend_varlab = prepend_varlab)  
    } else {
        y = NextMethod("as.character")
        var_lab(y) = var_lab(x)
        y
  
    } 
}

labelled_to_character_internal = function(x, prepend_varlab, ...) {
    vallab=val_lab(x)
    varlab =  var_lab(x)
    x = unlab(x)
    if(anyDuplicated(vallab)){
        warning("duplicated values in labels: ",paste(vallab[duplicated(vallab)],collapse=" "))
    }
    names_vallab = names(vallab)
    if (anyDuplicated(names_vallab)){
        duplicates = duplicated(names_vallab)
        warning(paste0("duplicated labels: ", paste(names_vallab[duplicates], collapse = ", ")))
        names(vallab)[duplicates] = paste0(names_vallab[duplicates], seq_len(sum(duplicates)))
    }
    
    uniqs = unique(x)
    vallab = labelled_and_unlabelled(uniqs,vallab) 
    if(prepend_varlab){
        if (!is.null(varlab) && (varlab!="")) names(vallab) = paste(varlab,names(vallab),sep = LABELS_SEP)
    }
    names(vallab)[match(x, vallab,incomparables = NA)]
}
    

#' @export
unique.labelled = function(x, ...){
    y = NextMethod("unique")
    if(!identical(getOption("expss.enable_value_labels_support"), 0)){
        var_lab(y) = var_lab(x)
        val_lab(y) = val_lab(x)
    }
    y
}




#' @export
as.logical.labelled = function (x, ...){
    y = NextMethod("as.logical")
    var_lab(y)=var_lab(x)
    class(y) = union("labelled", class(y))
    y	
}

#' @export
print.labelled = function(x, max = 50, max_labels = 20, ...){
    varlab = var_lab(x)
    vallab = val_lab(x)
    x_flat = x

    if(!is.null(varlab)){
        cat('LABEL:', varlab, "\n")
        
    }
    cat("VALUES:\n")
    cat(unlab(head(x_flat, max)))
    if(max < NROW(x_flat)) {
        cat("...", max, "items printed out of", NROW(x_flat), "\n")
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


#' @export
print.etable = function(x, digits = getOption("expss.digits"), ...,  right = TRUE){
    x = round_dataframe(x, digits = digits)
    print.data.frame(x, ...,  right = right, row.names = FALSE)
}


#' @export
str.labelled = function(object, ...){
    cat("Class 'labelled'")
    str(unlab(object), ...)
    max_labels = 20
    if(!is.null(var_lab(object))) cat("   .. .. LABEL:",var_lab(object), "\n")
    vallab = val_lab(object)
    if(!is.null(vallab)){
        vallab = paste0(vallab, "=", names(vallab))
        n_labs = length(vallab)
        
        if(n_labs>max_labels) {
            max_labels  = floor(max_labels/2)
            if(max_labels<1) max_labels = 1
            head_vallab = paste(head(vallab, max_labels), collapse = ", ")
            tail_vallab = paste(tail(vallab, max_labels), collapse = ", ")
            vallab = paste0(head_vallab," ... ", tail_vallab)
        }  else {
            vallab = paste(vallab, collapse = ", ")
            
        } 
        cat("   .. .. VALUE LABELS",paste0("[1:",n_labs,"]:"),vallab, "\n")
    }
    invisible(NULL)
}


# #' @export
# cbind.etable = function(..., deparse.level = 1){
#     args = list(...)
#     classes = lapply(args, class)
#     new_class = Reduce('%i%', classes)
#     if (!("data.frame" %in% new_class)) new_class = union("data.frame", new_class)
#     if (!("etable" %in% new_class)) new_class = union("etable", new_class)
#     
#     res = dtfrm(...)
#     class(res) = new_class
#     res    
#     
# }
# 
# 
# 
# 
# #' @export
# rbind.etable = function(..., deparse.level = 1){
#     args = list(...)
#     classes = lapply(args, class)
#     new_class = Reduce('%i%', classes)
#     if (!("data.frame" %in% new_class)) new_class = union("data.frame", new_class)
#     if (!("etable" %in% new_class)) new_class = union("etable", new_class)
#     
#     res = rbind.data.frame(..., stringsAsFactors = FALSE)
#     class(res) = new_class
#     res    
#     
#     
# }
# 
# 
# 


# #' @export
# as.etable = function(x, ...){
#     UseMethod("as.etable")
# }
# 
# #' @export
# as.etable.default = function(x, ...){
#     class(x) = union("etable", x)
#     x
# }
# 




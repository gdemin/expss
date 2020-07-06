
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
    y = NextMethod()
    vectors=list(...)
    dummy= lapply(vectors,var_lab)
    dummy=dummy[lengths(dummy)>0]
    if (length(dummy)>0) y = set_var_lab(y, dummy[[1]])
    
    dummy= lapply(vectors,val_lab)
    y = set_val_lab(y, do.call(combine_labels,dummy))
    y
}



#' @export
rep.labelled = function (x, ...){
    y= NextMethod()
    y = restore_attributes(y, x)
    y	
}

#' @export
'[.labelled' = function (x, ...){
    y = NextMethod()
    y = restore_attributes(y, x)
    y
}

#' @export
'[[.labelled' = function (x, ...){
    y = NextMethod()
    y = restore_attributes(y, x)
    y
}

#' @export
'[.labelled_spss' = `[.labelled`
#' @export
'[[.labelled_spss' = `[[.labelled`

# two assignment methods are needed to prevent state with inconsistent class and mode
# (such as 'numeric' in class but mode is character)
#' @export
'[<-.labelled' = function (x, ..., value){
    class(x) = setdiff(class(x), c("labelled", "labelled_spss"))
    y = NextMethod()
    class(y) = c("labelled", class(y))
    y
}

#' @export
'[[<-.labelled' = function (x, ..., value){
    class(x) = setdiff(class(x), c("labelled", "labelled_spss"))
    y = NextMethod()
    class(y) = c("labelled", class(y))
    y
}

# # @export
# levels.labelled = function(x){
#   if(is.factor(x)){
#     return(levels(unlab(x)))
#   }
#   names(val_lab(x))
# }
# 
# # @export
# relevel.labelled = function(x, ref, ...){
#   if(is.factor(x)){
#     res = relevel(unlab(x), ref, ...)
#   } else {
#     # nmax = 1 for future usage, see unique.labelled
#     res = relevel(factor(x, nmax = 1), ref, ...)
#   }
#   var_lab(res) = var_lab(x)
#   res
# }
# 
# # @export
# `levels<-.labelled` = function(x, value){
#   if(is.factor(x)){
#     res = unlab(x)
#     levels(res) = value
#     var_lab(res) = var_lab(x)
#     return(res)
#   }
#   names(val_lab(x)) = value
#   x
# }



restore_attributes = function(new_var, old_var){
    # "measurement", "spss_measure", "spss.measure", "measure",
    preserved_attributes = c("label",  "format.spss", "measure",  
                             "display_width", "labels", "na_values", "na_range")
    # we bypass interfaces set_val_lab, set_var_lab to 
    # skip perfomance unfriendly sorting of labels
    for(each_attr in preserved_attributes){
        attr_value = attr(old_var, each_attr, exact = TRUE)
        if(!is.null(attr_value)){
            attr(new_var, each_attr) = attr_value
        }
    }
    # we use new_var class for such functions as `as.integer.labelled`
    class(new_var) = unique(c("labelled", class(new_var), use.names = FALSE))
    new_var
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
    y = NextMethod()
    y = restore_attributes(y, x)
    y	
}

#' @export
as.integer.labelled = function (x, ...){
    y = NextMethod()
    y = restore_attributes(y, x)
    y	
}

#' @export
as.numeric.labelled = as.double.labelled

#' @export
as.character.labelled = function (x, prepend_varlab = FALSE, ...){
    if(!identical(getOption("expss.enable_value_labels_support"), 0)){
        labelled_to_character_internal(x, prepend_varlab = prepend_varlab)  
    } else {
        y = NextMethod()
        if(!prepend_varlab) {
            y = set_var_lab(y, var_lab(x))
        }
        y
  
    } 
}

labelled_to_character_internal = function(x, prepend_varlab, ...) {
    vallab = val_lab(x)
    varlab = var_lab(x)
    x = unlab(x)
    uniqs = unique(x)
    vallab = labelled_and_unlabelled(uniqs,vallab) 
    if(prepend_varlab){
        if (!is.null(varlab) && (varlab!="")) names(vallab) = paste(varlab,names(vallab),sep = LABELS_SEP)
    }
    names(vallab)[match(x, vallab,incomparables = NA)]
}
    

#' @export
unique.labelled = function(x, incomparables = FALSE, fromLast = FALSE,
                           nmax = NA, ...){
  # TODO additional arguments with sensible names about labels support
  y = NextMethod()
  if(identical(nmax, 1)) {
    labels_support = 2
  } else {
    if(is.null(nmax)){
      labels_support = 0  
    } else {
      labels_support = getOption("expss.enable_value_labels_support", 1) 
    }
  }
  if(!identical(labels_support, 0)){
    if(identical(labels_support, 2)){
      y = unique(c(y, val_lab(x), use.names = FALSE), 
                 incomparables = incomparables, 
                 fromLast = fromLast, 
                 nmax = nmax, 
                 ...)
    }
    y = restore_attributes(y, x)
  }
  y
}



#' @export
as.logical.labelled = function (x, ...){
    y = NextMethod()
    set_var_lab(y, var_lab(x))
	
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
    cat(unlab(as.character(unlab(head(x_flat, max)))), sep = ", ")
    if(max < NROW(x_flat)) {
        cat("...", max, "items printed out of", NROW(x_flat), "\n")
    } else {
        cat("\n")
    }
    if(!is.null(vallab)){
        vallab = sort_asc(sheet(Value = vallab, Label = names(vallab)),"Value") 
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
print.etable = function(x, digits = get_expss_digits(), remove_repeated = TRUE, ...,  right = TRUE){
    curr_output = getOption("expss.output")
    if(!is.null(curr_output)){
        if("rnotebook" %in% curr_output){
            res = htmlTable(x, digits = digits)

            res = fix_cyrillic_for_rstudio(res)

            print(res)
            return(invisible(NULL))
        }
        if("viewer" %in% curr_output){
            res = htmlTable(x, digits = digits)
            res = fix_cyrillic_for_rstudio(res)
            attr(res, "html") = NULL
            class(res) = class(res) %d% "html"
            print(res)
            return(invisible(NULL))
        }
        if("huxtable" %in% curr_output){
          res = huxtable::as_huxtable(x)
          print(res, colnames = FALSE)
          return(invisible(NULL))
        }

    }
    if(!("raw" %in% curr_output)){
        x = split_all_in_etable_for_print(x,
                                          digits = digits, 
                                          remove_repeated = remove_repeated)
    }
    if("commented" %in% curr_output){
        if(NROW(x)>0 && NCOL(x)>0){
            x = cbind("#" = "#", x)
            colnames(x) = rep("", length(x))
        } 
        
    }
    print.data.frame(x, ...,  right = right, row.names = FALSE)
}

fix_cyrillic_for_rstudio = function(x){
    need_fix = isTRUE(getOption("expss.fix_encoding"))
    if(need_fix){
        curr_enc = Encoding(x)
        if(toupper(curr_enc) %in% c("UTF-8", "UTF8")){
            x = iconv(x, from = "UTF8", to = "cp65001")
        } else {
            x = iconv(enc2utf8(x), from = "UTF8", to = "cp65001")
        }
    }
    x
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



#' @export
t.etable = function(x){
    row_labels = x[[1]]
    col_names = colnames(x)[-1]
    data = x[,-1]
    class(data) = class(data) %d% "etable"
    res = sheet(col_names, t(data))
    res =setNames(res, c("row_labels", row_labels))
    class(res) = union("etable", class(res))
    res
}

#' @export
print.with_caption = function(x, digits = get_expss_digits(), remove_repeated = TRUE, ...,  right = TRUE){
  curr_output = getOption("expss.output")
  if(!is.null(curr_output)){
    if("rnotebook" %in% curr_output){
      res = htmlTable(x, digits = digits)
      res = fix_cyrillic_for_rstudio(res)
      print(res)
      return(invisible(NULL))
    }
    if("viewer" %in% curr_output){
      res = htmlTable(x, digits = digits)
      res = fix_cyrillic_for_rstudio(res)
      attr(res, "html") = NULL
      class(res) = class(res) %d% "html"
      print(res)
      return(invisible(NULL))
    }
    if("huxtable" %in% curr_output){
      res = huxtable::as_huxtable(x)
      print(res)
      return(invisible(NULL))
    }
    
  }
  caption = get_caption(x)
  if("commented" %in% curr_output){
    cat(" # ", caption,  sep = "")  
  } else if("raw" %in% curr_output){
    cat(caption,  "\n", sep = "") 
  } else {
    cat(" ", caption,  sep = "")  
  }
  print(set_caption(x, NULL), digits = digits, remove_repeated = remove_repeated, ...,  right = right)
  invisible(x)
}

# #' @export
# cbind.etable = function(..., deparse.level = 1){
#     args = list(...)
#     classes = lapply(args, class)
#     new_class = Reduce('%i%', classes)
#     if (!("data.frame" %in% new_class)) new_class = union("data.frame", new_class)
#     if (!("etable" %in% new_class)) new_class = union("etable", new_class)
#     
#     res = sheet(...)
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





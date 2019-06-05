#' @export
net = function(x, ..., position = c("below", "above", "top", "bottom"), prefix = "TOTAL ", new_label = c("all", "range", "first", "last")){
    UseMethod("net")
}

#' @export
net.default = function(x, ..., position = c("below", "above", "top", "bottom"), prefix = "TOTAL ", new_label = c("all", "range", "first", "last")){
     position = match.arg(position)  
     new_label = match.arg(new_label)
     possible_values = unique(x, nmax = 1)
     args = list(...)
     arg_names = names(args)
     if(is.null(arg_names)) arg_names = rep("", length(args))
     first_col = x
     other_cols = vector(mode = "list", length = length(args))
     for(i in seq_along(args)){
         possible_values = possible_values[!is.na(possible_values)]
         curr_net = args[[i]]
         curr_names = arg_names[i]
         if(!inherits(curr_net, "formula")){
             if(!inherits(curr_net, "criterion")) curr_net = as.criterion(curr_net)
             source_codes = x %i% curr_net
             target = find_code(source_codes, possible_values, position = position)
             frm_net = curr_net ~ target
         } else {
             frm_net = curr_net
         }
         recode_args = list(x, frm_net, new_label = new_label, with_labels = TRUE)
         names(recode_args)[[2]] = curr_names 
         # browser()
         res = do.call(recode, recode_args)
         possible_values = union(possible_values, unique(res)) 
         if(!is.null(val_lab(res))) names(val_lab(res)) = paste0(prefix, names(val_lab(res)))
         other_cols[[i]] = res
         frm_net[[3]] = NA
         recode(first_col, with_labels = TRUE) = frm_net
     }
     res = c(list(first_col), other_cols)
     # if(names(res)
     do.call(mrset, res)
}

#' @export
net.category = function(x, ..., position = c("below", "above", "top", "bottom"), prefix = "TOTAL ", new_label = c("all", "range", "first", "last")){
    
}

#' @export
net.dichotomy = function(x, ..., position = c("below", "above", "top", "bottom"), prefix = "TOTAL ", new_label = c("all", "range", "first", "last")){
    
}

#' @export
net.list = function(x, ..., position = c("below", "above", "top", "bottom"), prefix = "TOTAL ", new_label = c("all", "range", "first", "last")){
    
}

#' @export
net.data.frame = function(x, ..., position = c("below", "above", "top", "bottom"), prefix = "TOTAL ", new_label = c("all", "range", "first", "last")){
    
}


construct_new_label = function(values, val_labs, prefix, new_label){
            
}


find_code = function(codes, possible_values, position = c("below", "above", "bottom", "top")){
    position = match.arg(position)
    switch(position, 
           above = {
               # We need number x: x < min_code and x > next_code.
               # If some of the boundaries is NA we ignore them.
               # All codes are always sorted in ascending order. So if 
               # we want position above we need new code which is less than existing codes.
               min_code = suppressWarnings(min(codes, na.rm = TRUE))
               next_code = suppressWarnings(max(possible_values[possible_values < min_code], na.rm = TRUE))
               res = c((min_code + next_code)/2, # no NAs 
                       min_code - 1,             # next_code is NA
                       suppressWarnings(min(possible_values, na.rm = TRUE)) - 1, # min_code is NA
                       0)     # all is NA
               res[is.finite(res)][1]
               
           },
           below = {
               # we need number x: x > max_code and x < next_code
               # if some of the boundaries is NA we ignore them
               max_code = suppressWarnings(max(codes, na.rm = TRUE))
               next_code = suppressWarnings(min(possible_values[possible_values > max_code], na.rm = TRUE))
               res = c((max_code + next_code)/2, # no NAs 
                       max_code + 1,             # next_code is NA
                       suppressWarnings(max(possible_values, na.rm = TRUE)) + 1, # min_code is NA
                       0)     # all is NA
               res[is.finite(res)][1]
               
           },
           top = {
               min_code = suppressWarnings(min(c(codes, possible_values), na.rm = TRUE))
               if(is.finite(min_code)){
                   min_code - 1
               } else {
                   0
               }
           },
           bottom = {
               max_code = suppressWarnings(max(c(codes, possible_values), na.rm = TRUE))
               if(is.finite(max_code)){
                   max_code + 1
               } else {
                   0
               }
           }
           
    )
    
}
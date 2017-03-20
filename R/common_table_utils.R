##########################################

pack_data.table = function(..., subset = NULL){
    args = list(...)
    res = as.data.table(do.call(c, args))
    names_res = names(res)
    bad_names = is.na(names_res) | (names_res=="") | duplicated(names_res)
    if(sum(bad_names)>0){
        names(res)[bad_names] = paste0("...bbbaaaddd__", 1:sum(bad_names)) 
    }
    if(!is.null(subset)){
        res[subset & !is.na(subset),]
    } else {
        res    
    }
    
}

###########################
universal_subset = function(data, index, drop = TRUE){
    if(is.matrix(data) || is.data.frame(data)){
        data =  data[index, , drop = drop]
    } else {
        data =  data[index]
    }
    data
}

recycle_if_single_row = function(data, nrows){
    if(NCOL(data) == 0) {
        data = set_val_lab(set_var_lab(NA, var_lab(data)), val_lab(data))
    }    
    if(NROW(data)==1){
        if(is.matrix(data) || is.data.frame(data)){
            data =  data[rep(1, nrows), ]
        } else {
            data =  rep(data, nrows)
        }
    }
    data
}

#############################################################
### check that all arguments ... have equal length or length 1 (NULLs are also allowed)
check_sizes = function(caller_name, ...){

    sizes = unlist(rapply(list(...),  
                   NROW, classes = "ANY", how = "replace"))
    stopif(!all(sizes %in% c(0, 1, max(sizes))),
           caller_name, ": all variables should be of the same length or length 1.")
    invisible(TRUE)
}



######
# remove duplicated or trailing splitters ('|') from labels
remove_unnecessary_splitters = function(labels){
    if(length(labels)>0) {
        labels = gsub("\\.\\.\\.bbbaaaddd__\\d+", "", labels, perl = TRUE)
        labels = gsub("\\|+", "|", labels, perl = TRUE)
        labels = gsub("(^\\|)|(\\|$)", "", labels, perl = TRUE)
    }
    labels
}

### add # as first symbol to the total title

add_first_symbol_to_total_label = function(total_label, symbol = "#"){
    if(substr(total_label, 1, 1)!=symbol){
        paste0("#", total_label)       
    } else {
        total_label
    }
}

#######
set_negative_and_na_to_zero = function(x){
    x[is.na(x) | (x<0)] = 0
    x
}

########## 

convert_multicolumn_object_to_vector  = function(x){
    if(is.matrix(x) || is.data.frame(x)){
        if(NCOL(x)==0){
            x = cbind(x, 'NA' = rep(NA, NROW(x))) # rep for zero rows objects
        }
        # we convert factors to labelled because further we will combine data.frame to single column and
        # for labelled value labels will be combined. It is not so for factors.
        varlab = var_lab(x)
        if(!is.matrix(x)){
            for(each in seq_along(x)){
                if(is.factor(x[[each]])) x[[each]] = as.labelled(x[[each]])
            }
        }
        vallab = val_lab(x)
        x = c(x, recursive = TRUE)
        val_lab(x) = vallab
        var_lab(x) = varlab
    } 
    x
}

#################################

long_datatable_to_table = function(dtable, rows, columns, values){
    
    rows_columns = which(colnames(dtable) %in% c(rows, columns))
    for (each in rows_columns){
        curr_col = dtable[[each]]
        if(is.labelled(curr_col)) {
            if(any(!is.na(curr_col)) || length(val_lab(curr_col))>0){
                dtable[[each]] = fctr(curr_col, drop_unused_labels = FALSE, 
                                      prepend_var_lab = FALSE)
            } else {
                dtable[[each]] = unlab(curr_col)
            }
        } 
        # dcast doesn't work when we have factor without levels
        if(is.factor(curr_col) && length(levels(curr_col))==0){
            dtable[[each]] = as.character(curr_col)
        }
    }
    if(nrow(dtable)==0){
        # we need at least one row in our table
        # and we want NA only when there are no other levels in the variable
        empty_dt = data.table(NA)
        empty_dt = set_names(empty_dt, rows[[1]]) 
        dtable = rbind(dtable, empty_dt, use.names = TRUE, fill = TRUE)  
        
        for (each in rows_columns){
            curr_levels = levels(dtable[[each]])
            if(length(curr_levels)>0){
                dtable[[each]][] = curr_levels[1]
            }
        }
    }
    setkeyv(dtable, cols = c(rows, columns), verbose = FALSE)
    frm = as.formula(paste(paste(rows, collapse = "+"), "~", paste(columns, collapse = "+")))
    mess = utils::capture.output(
        {res = dcast(dtable, frm, value.var = values, drop = FALSE, fill = NA, sep = "|")},
        type = "message"
    )
    stopif(length(mess)>0,
           paste0("Something is going wrong - several elements in one cell in the table (",mess,").")
    )
    if(length(values)>1){
        # dcast place 'values' on top of all other grouping variable
        # we doesn't need it so we change it
        regx = gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", values, perl = TRUE)
        regx = paste(regx, collapse = "|")
        regx = paste0("^(",regx,")\\|(.*)$")
        setnames(res, gsub(regx, "\\2|\\1", colnames(res), perl = TRUE))
        old_order = seq_len(ncol(res))[-seq_along(rows)]
        other_order = rep(seq_len(length(old_order)/length(values)), length(values))
        old_var_order = rep(seq_along(values), each = length(old_order)/length(values))
        new_order = c(seq_along(rows), order(other_order, old_var_order) + length(rows))
        res = res[, new_order, with = FALSE]
    }
    for(each in rows){
        res[[each]] = as.character(res[[each]])
        res[[each]][is.na(res[[each]])] = ""
    }
    res
}
#####
# convert mdsets to categories, accepts lists
dichotomy_to_category_encoding = function(x){
    lapply(x, function(item){
        if(is.list(item) && !is.data.frame(item)){
            dichotomy_to_category_encoding(item)
        } else {
            if(is.dichotomy(item)){
                as.category(item, compress = FALSE)
            } else {
                if(is.data.frame(item) & !is.category(item)){
                    as.list(item)
                } else {
                    item    
                }
                
            }
        }
    })    
}

# convert mdsets to categories, accepts lists
multiples_to_single_columns_with_dummy_encoding = function(x){
    lapply(x, function(item){
        if(is.list(item) && !is.data.frame(item)){
            multiples_to_single_columns_with_dummy_encoding(item)
        } else {
            if(is.dichotomy(item)){
                if(is.matrix(item)) item = as.data.frame(item)
                na_if(item) = 0
                as.list(make_value_labels_from_names(item))
            } else {
                if(is.category(item)){
                    item = as.dichotomy(item, keep_unused = TRUE, use_na = TRUE)
                    na_if(item) = 0
                    as.list(make_value_labels_from_names(item))
                } else {
                    if(is.data.frame(item)){
                        as.list(item)
                    } else {
                        item    
                    }
                }
            }
        }
    })    
}
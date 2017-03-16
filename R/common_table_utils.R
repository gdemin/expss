### leave only valid cases
valid_dttbl = function(dttbl, ..., weight_name = NULL){
    stopif(!is.data.table(dttbl), "'dttbl' should be data.table.")
    args = list(...)
    args = args[lengths(args)>0]
    if(is.null(weight_name)){
        valid_vars = TRUE
    }  else {
        valid_vars = !is.na(dttbl[[weight_name]]) & dttbl[[weight_name]]>0
    }
    for (each in args){
        valid_vars = valid_vars & valid(dttbl[ , each, with = FALSE])
    }
    dttbl[valid_vars]

}
#############################################################

#############################################################

# add to columns ... in dttbl absent values from labels
# other columns will be copied from present values
# except fill_na - which will be filled... by NA

add_unused_labels = function(dttbl, varname, fill_na = NULL){
    unused_values = setdiff(val_lab(dttbl[[varname]]), dttbl[[varname]])
    unused_len = length(unused_values)
    if(unused_len == 0) return(dttbl)
    if(nrow(dttbl)>0){

        if(is.null(fill_na)){
            fill_na = colnames(dttbl) %d% varname
        } else {
            fill_na = fill_na %d% varname
        }
        add_dttbl = unique(dttbl[, colnames(dttbl) %d% fill_na %d% varname, with = FALSE])
        uniqs_row = nrow(add_dttbl)
        add_dttbl = add_dttbl[rep(seq_len(nrow(add_dttbl)), unused_len), ]
        add_dttbl[[varname]] = rep(unused_values, each = uniqs_row)
        add_dttbl[, fill_na] = NA

    } else {
        storage.mode(unused_values) = storage.mode(dttbl[[varname]])
        class(unused_values) = class(dttbl[[varname]])
        add_dttbl = data.table(x = unused_values)
        setnames(add_dttbl, varname)
    }

    res = rbind(dttbl, add_dttbl, fill = TRUE)
    not_na_varname = !is.na(res[[varname]])
    res[not_na_varname, ]

}

###########################################

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

########

make_names = function(arg, prefix){
    if(!is.null(arg)){
        lapply(seq_along(arg),
               function(each) paste0(prefix, each, "_", seq_len(NCOL(arg[[each]])))
        )
    } else {
        list(NULL)
    }
}

#######

factors2characters = function(dfs){
    for (each in seq_along(dfs)){
        if(is.factor(dfs[[each]])){
            dfs[[each]] = as.character(dfs[[each]])
        }
    }
    dfs
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

    labels = gsub("\\.\\.\\.bbbaaaddd__\\d+", "", labels, perl = TRUE)
    labels = gsub("\\|+", "|", labels, perl = TRUE)
    labels = gsub("(^\\|)|(\\|$)", "", labels, perl = TRUE)
    labels
}

### add # as first symbol to the total title

add_first_symbol_to_total_title = function(total_title, symbol = "#"){
    if(substr(total_title, 1, 1)!=symbol){
        paste0("#", total_title)       
    } else {
        total_title
    }
}

#######
set_negative_and_na_to_zero = function(x){
    x[is.na(x) | (x<0)] = 0
    x
}

########## 

convert_multicolumn_object_to_vector  = function(x){
    if(NCOL(x)>1){
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
                dtable[[each]] = to_fac(curr_col, drop_unused = FALSE, prepend_var_lab = FALSE)
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
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

pack_data.table = function(...){
    args = list(...)
    nulls = vapply(args, is.null, FUN.VALUE = logical(1))
    args = args[!nulls]
    res = as.data.table(do.call(c, args))
    names_res = names(res)
    bad_names = is.na(names_res) | (names_res=="") | duplicated(names_res)
    if(sum(bad_names)>0){
        names(res)[bad_names] = paste0("...bbbaaaddd__", 1:sum(bad_names)) 
    }
    res
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
    stopif(!all(sizes %in% c(1, max(sizes))),
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


#######
set_negative_and_na_to_zero = function(x){
    x[is.na(x) | (x<0)] = 0
    x
}

#####
# convert mdsets to categories, accepts lists
dichotomy_to_category_encoding = function(x){
    lapply(x, function(item){
        if(is.list(item) && !is.data.frame(item)){
            dichotomy_to_category_encoding(item)
        } else {
            if(is.dichotomy(item)){
                category_df(item, use_var_lab = TRUE, compress = FALSE)
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
                val_lab(item) = setNames(1, "")
                as.list(make_labels_from_names(item))
            } else {
                if(is.category(item)){
                    item = dichotomy_df(item, keep_unused = TRUE, use_na = TRUE)
                    na_if(item) = 0
                    
                    val_lab(item) = setNames(1, "")
                    as.list(make_labels_from_names(item))
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
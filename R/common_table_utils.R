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
    args = lapply(args, flat_list, flat_df = TRUE)
    as.data.table(do.call(c, args))
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
    args = flat_list(list(...))
    nulls = vapply(args, is.null, FUN.VALUE = logical(1))
    sizes = vapply(args[!nulls], NROW, FUN.VALUE = numeric(1))
    stopif(!all(sizes %in% c(1, max(sizes))),
           caller_name, ": all variables should be of the same length or length 1.")
    invisible(TRUE)
}



######
# remove duplicated or trailing splitters ('|') from labels
remove_unnecessary_splitters = function(labels){

    labels = gsub("\\|+", "|", labels, perl = TRUE)
    labels = gsub("(^\\|)|(\\|$)", "", labels, perl = TRUE)
    labels
}




index_name = function(x){
    paste0("...", x, "___", "index")
}

if_null = function(x, value){
    if(is.null(x)){
        value
    } else {
        x
    }
}





long_table_summary = function( intermediate_table,
                               fun_value_labels,
                               fun
){
    
    col_vars = if_null(intermediate_table[[COL_VAR]], list(""))
    row_vars = if_null(intermediate_table[[ROW_VAR]], list(""))
    cell_vars = if_null(intermediate_table[[CELL_VAR]], list(NA))
    weight = intermediate_table[[WEIGHT]]
    subgroup = intermediate_table[[SUBGROUP]]
    fun = match.fun(fun)
    
    if(!is.null(weight)){
        stopif(!("weight" %in% names(formals(fun))),
               "`weight` is provided but function doesn't have formal `weight` argument.")
    }
    
    
    ###### main data.table #######
   
    
    check_sizes("stat", col_vars, row_vars, cell_vars,  weight, subgroup)
    
    
    curr_dt = pack_data.table(col_vars, row_vars, cell_vars, subset = subgroup)
    
    ####### data.table names ###############
    
    new_cols_names = strange_name(paste0("r", seq_along(col_vars)))
    new_rows_names = strange_name(paste0("c", seq_along(col_vars)))
    colnames(curr_dt)[seq_along(c(new_rows_names, new_cols_names, recursive = TRUE))] = 
        c(new_rows_names, new_cols_names, recursive = TRUE)
    
    cell_names_indexes = lapply(cell_vars, function(item) seq_len(NCOL(item)))
    for(each in seq_along(cell_names_indexes)[-1]){
        cell_names_indexes[[each]] = max(cell_names_indexes[[each-1]]) + cell_names_indexes[[each]]
    } 
    new_cell_names = colnames(curr_dt)[-seq_along(c(new_vars_names, new_bans_names, recursive = TRUE))]
    
    
    ######## weights ########
    if (!is.null(weight)) {
        weight_name = WEIGHT
        curr_dt[[WEIGHT]] = weight
    } else {
        weight_name = NULL
    }
    
    ######## calculations #######################
    #### each row_var
    res_rows = lapply(seq_along(new_rows_names), function(rows_var_num){
        ##### each col_var
        res_cols = lapply(seq_along(new_cols_names), function(cols_var_num){
            #### each cell vars
            res_cells = lapply(cell_names_indexes, function(cells_var_num){
                
                #### fun
                res = elementary_summary(curr_dt,
                                         cell_names = new_cell_names[cells_var_num],
                                         col_names = new_cols_names[[ban_num]],
                                         row_names = new_rows_names[[var_num]],
                                             fun = fun,
                                             fun_value_labels = fun_value_labels,
                                             weight_name = weight_name
                                             
                    )
                res

            })
            
            res_cells = rbindlist(res_cells, use.names = TRUE, fill = TRUE)
            res_cells[[COL_VAR_NUM]]= cols_var_num
            res_cells
            
        })
        
        res_cols = rbindlist(res_cols, use.names = TRUE, fill = TRUE)
        res_cols[[ROW_VAR_NUM]] = rows_var_num
        res_cols
    })
    
    res_rows = rbindlist(res_rows, use.names = TRUE, fill = TRUE)
    res_rows

}

#############
elementary_summary = function(dttbl,
                              cell_names,
                              col_names,
                              row_names,
                              fun,
                              fun_value_labels,
                              weight_name = NULL
){
    # to pass CRAN check
    ..weight__ = NULL
    ..res_num__ = NULL
    ..weight__ = NULL
    ..bn__vallabs = NULL
    ..bn__ = NULL
    ..vr__vallabs = NULL
    ..vr__ = NULL
    ..bn__order = NULL
    ..vr__order = NULL
    ..bn__label = NULL
    ..vr__label = NULL
    
    #####
    с_vallab = val_lab(dttbl[, col_names, with = FALSE])
    с_varlab = var_lab(dttbl[, col_names, with = FALSE][[1]])
    r_vallab = val_lab(dttbl[, row_names, with = FALSE][[1]]) # vars should be only single column
    r_varlab = var_lab(dttbl[, row_names, with = FALSE][[1]])
    
    if(length(col_names)>1){
        # if banner is data.frame (multiple choice) we convert dttbl to long form
        for_calc = data.table(..bn__ =  unlist(dttbl[, col_names, with = FALSE]),
                              dttbl[ , c(row_names, cell_names), with = FALSE])
    } else {
        for_calc = dttbl[, c(col_names, row_names, cell_names), with = FALSE]
        colnames(for_calc)[1] = "..bn__"
    }
    
    # new colnames for col_vars
    colnames(for_calc)[2] = "..vr__"
    by_string = "..vr__,..bn__"
    if(is.null(weight_name)){
        # we need at least one rows because with zero rows all rows from 'fun' will be ignored
        if(nrow(for_calc)==0){
            for_calc = rbind(for_calc, data.table(..bn__ = NA), fill = TRUE, use.names = TRUE)
        } 
        for_calc = for_calc[ , fun(.SD), by = by_string]
    } else {
        for_calc[["..weight__"]] = dttbl[[weight_name]]
        # we need at least one rows because with zero rows all rows from 'fun' will be ignored
        if(nrow(for_calc)==0){
            for_calc = rbind(for_calc, data.table(..bn__ = NA), fill = TRUE, use.names = TRUE)
        } 
        for_calc = for_calc[ , fun(.SD[,-"..weight__"], weight = ..weight__), by = by_string]
    }
    ### if 'fun' return values with zero rows we will have for_calc with zero rows
    ### so we fix it
    if(nrow(for_calc)==0){
        for_calc = rbind(for_calc, data.table(..bn__ = NA), fill = TRUE, use.names = TRUE)
    } 
    # construct template with all combinations of possible values
    possible_ban_values = sort(unique(c(b_vallab, for_calc[["..bn__"]])))
    possible_var_values = sort(unique(c(v_vallab, for_calc[["..vr__"]])))
    if(length(possible_ban_values)==0) possible_ban_values = NA
    if(length(possible_var_values)==0) possible_var_values = NA
    if(length(custom_labels)){
        missed = setdiff(custom_labels, colnames(for_calc))
        stopif(length(missed)>0,
               "some custom labels are missed in the result: ", paste(missed, collapse = ", ")
        )
        custom_params = unique(for_calc[, custom_labels, with = FALSE])
        ..custom_param_number__ = seq_len(nrow(custom_params))
        custom_params[, ..custom_param_number__ := ..custom_param_number__]
        template = CJ(
            ..custom_param_number__ = ..custom_param_number__ ,
            ..bn__ = possible_ban_values,
            ..vr__ = possible_var_values,
            sorted = TRUE,
            unique = TRUE
        )
        template = custom_params[template, on = "..custom_param_number__", nomatch = NA]
        template = template[, -"..custom_param_number__"]
    } else {
        template = CJ(..bn__ = possible_ban_values,
                      ..vr__ = possible_var_values,
                      sorted = TRUE,
                      unique = TRUE
        )
    }
    
    
    template[, ..bn__vallabs := values2labels(set_val_lab(..bn__, b_vallab))]
    template[, ..vr__vallabs := values2labels(set_val_lab(..vr__, v_vallab))]
    template[, ..bn__order := match(..bn__, possible_ban_values)]
    template[, ..vr__order := match(..vr__, possible_var_values)]
    
    
    if(is.null(b_varlab)) b_varlab = ""
    if(is.null(v_varlab)) v_varlab = ""
    template[, ..bn__label := b_varlab]
    template[, ..vr__label := v_varlab]
    if(length(custom_labels)){
        for_calc = for_calc[template, on = c("..vr__","..bn__", custom_labels),nomatch = NA]
        # for_calc = merge(as.dtfrm(template), for_calc,
        #                  by = c("..vr__","..bn__", custom_labels), 
        #                  all.x = TRUE, all.y = FALSE)
    } else {
        for_calc = for_calc[template, on = c("..vr__","..bn__"),nomatch = NA]
        # for_calc = merge(as.dtfrm(template), for_calc, 
        #                  by = c("..vr__","..bn__"), 
        #                  all.x = TRUE, all.y = FALSE)
    }
    # for_calc = as.data.table(for_calc)
    nas = !is.na(for_calc[["..vr__"]]) & !is.na(for_calc[["..bn__"]])
    ### generally we always need data.table with at least one rows
    if(sum(nas)>0) {
        for_calc = for_calc[nas, ]
    } else {
        should_be_na = names(for_calc) %d% c("..vr__", 
                                             "..bn__", 
                                             "..bn__vallabs",
                                             "..vr__vallabs",
                                             "..bn__order", 
                                             "..vr__order", 
                                             "..bn__label", 
                                             "..vr__label",
                                             custom_labels)
        if(length(should_be_na)>0) {
            for_calc[, (should_be_na) := NA]
        }
    }
    for_calc[, ..res_num__ :=  seq_len(.N), by = by_string]
    for_calc
    
}

long_to_wide_summary = function(long, col_indexes, col_labels, row_indexes, row_labels){
    ## indexes are needed to prevent possible collapse of categories with same name
    setkeyv(long, cols = c(row_indexes, col_indexes), verbose = FALSE)
    lhs = paste(c(row_indexes, row_labels),collapse = "+")
    rhs = paste(c(col_indexes, col_labels),collapse = "+")
    frm = paste(lhs, "~", rhs)
    value_var = colnames(long) %d% c(row_indexes, row_labels,
                                     col_indexes, col_labels)
    mess = utils::capture.output(
        {res = dcast.data.table(long, frm, sep = "|",  value.var = value_var, fill = NA)},
        type = "message"
    )
    stopif(length(mess)>0,
           paste0("'pivot' - possibly you set 'use_result_row_order' to FALSE without providing your own row index (",mess,").")
    )
    if(length(row_indexes)) res = res[, -seq_along(row_indexes), with = FALSE]
    if(length(row_labels)) {
        rows = as.list(res[, seq_along(row_labels), with = FALSE] )
        rows = do.call(paste, c(rows, sep = "|"))
        res = cbind(row_labels = rows, res[, -seq_along(row_labels), with = FALSE])
    } else {
        res = cbind(row_labels = "", res)
    }
    res$row_labels = remove_unnecessary_splitters(res$row_labels)
    if(length(value_var)>1){
        # dcast place 'value_var' on top of all other grouping variable
        # we doesn't need it sowe change it
        regx = paste(value_var, collapse = "|")
        regx = paste0("^(",regx,")\\|(.*)$")
        setnames(res, gsub(regx, "\\2|\\1", colnames(res), perl = TRUE))
        old_order = seq_len(ncol(res))[-1]
        other_order = rep(seq_len(length(old_order)/length(value_var)), length(value_var))
        old_var_order = rep(seq_along(value_var), each = length(old_order)/length(value_var))
        new_order = c(1, order(other_order, old_var_order) + 1)
        res = res[,new_order, with = FALSE]
    }
    # remove indexes from column names
    if(length(col_indexes)){
        regx = rep("(\\d+|NA)\\|", length(col_indexes))
        regx = c("^", regx, "?")
        regx = paste(regx, collapse = "")
        clnm = gsub(regx, "", colnames(res), perl = TRUE)
        clnm = remove_unnecessary_splitters(clnm)
        setnames(res, clnm)
    }
    res
    
}



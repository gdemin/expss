#############################################################

### dttbl should be after 'valid_dttbl'
elementary_cases = function(dttbl, var_names, ban_names, weight_name = NULL){
    # to pass CRAN check
    ban = NULL
    cases = NULL
    #####
    if(!is.null(weight_name)){
        count_cases = function(bname, vname){
            group = parse(text = sprintf("list(var = %s, ban = %s)", vname, bname))
            expr = parse(text = sprintf("sum(%s, na.rm = TRUE)", weight_name))
            dttbl[, list(cases = eval(expr)), by = group]
        }
    } else {
        count_cases = function(bname, vname){
            group = parse(text = sprintf("list(var = %s, ban = %s)", vname, bname))
            dttbl[, list(cases = .N), by = group]
        }
    }
    res = lapply(var_names, function(vname){
        lapply(ban_names, count_cases, vname)
    })
    # if we don't convert results to data.frame we will have problems with
    # rbind of columns of different types
    res = unlist(res, recursive = FALSE)
    # res = setDT(do.call(rbind, res))
    res = rbindlist(res, fill = TRUE, use.names = TRUE)
    # setkeyv(res, c("var", "ban"), verbose = FALSE)
    res = res[!is.na(var) & !is.na(ban)]
    res = res[, list(cases = sum(cases, na.rm = TRUE)), by = "var,ban"]
    res
}

#############################################################

### dttbl should be after 'valid_dttbl'
get_total = function(dttbl, ban_names, weight_name){
    # to pass CRAN check
    ban = NULL
    unweighted_cases = NULL
    cases = NULL
    #####

    if (is.null(weight_name)){
        res_total = lapply(ban_names, function(bname){
            group = parse(text = sprintf("list(ban = %s)", bname))
            dttbl[, list(cases = .N), by = eval(group)]
        })
    } else {
        res_total =  lapply(ban_names, function(bname){
            group = parse(text = sprintf("list(ban = %s)", bname))
            expr = parse(text = sprintf("sum(%s, na.rm = TRUE)", weight_name))
            dttbl[, list(cases = eval(expr), unweighted_cases = .N), by = eval(group)]
        })
    }
    res_total = rbindlist(res_total, use.names = FALSE)
    if(is.null(weight_name)){
        res_total = res_total[, list(cases = sum(cases, na.rm = TRUE)), by = "ban"]
        res_total[, unweighted_cases:=cases]
    } else {
        res_total = res_total[, list(cases = sum(cases, na.rm = TRUE), unweighted_cases = sum(unweighted_cases, na.rm = TRUE)), by = "ban"]
    }
    res_total[!is.na(ban)]
}



#############################################################

add_total = function(cases, total,
                     total_row_position = c("below", "above", "none"),
                     weighted_total = FALSE
                     ){


    if(!nrow(total)){
        total = dtfrm(ban = NA, cases = NA, unweighted_cases = NA)
        storage.mode(total$ban) = storage.mode(cases$ban)
        storage.mode(total$cases) = storage.mode(cases$cases)
        storage.mode(total$unweighted_cases) = storage.mode(cases$cases)
        val_lab(total$ban) = val_lab(cases$ban)
        class(total$ban) = class(cases$ban)
        class(total$cases) = class(cases$cases)
        class(total$unweighted_cases) = class(cases$cases)
        total = as.data.table(total)
    }

    table_total = data.table(ban = total$ban, ban_total = total$cases)
    # setkeyv(table_total, cols = c("ban"), verbose = FALSE)
    cases = table_total[cases, on = "ban"]

    if(is.null(val_lab(total$ban))){
        total$ban_vallabs = unlab(total$ban)
    } else {
        total$ban_vallabs = unlab(unclass(values2labels(total$ban)))
    }

    total_row_position = match.arg(total_row_position)

    if (total_row_position=="below") {
        max_var_order = suppressWarnings(max(cases$var_order, na.rm = TRUE))
        if(!is.finite(max_var_order)) max_var_order = 0
        if(nrow(total)>0){
            total$var_order = max_var_order + 1
        } else {
            total$var_order = numeric(0)
        }
    } else if (total_row_position=="above"){
        if(nrow(total)>0){
            total$var_order = 0
        } else {
            total$var_order = numeric(0)
        }
    }
    if (total_row_position!="none"){
        if(weighted_total){
            total$unweighted_cases = NULL
        } else {
            total$cases = total$unweighted_cases
            total$unweighted_cases = NULL
        }
        var_label =  var_lab(cases$var)
        ban_label =  var_lab(cases$ban)
        if(is.null(var_label)){
            total$var_label = ""
        } else {
            total$var_label = var_label
        }
        if(is.null(ban_label)){
            total$ban_label = ""
        } else {
            total$ban_label = ban_label
        }
        total$is_total_row = TRUE
        cases = rbind(cases, total, fill = TRUE)
    }
    cases
}

#############################################################


label_elementary_cases = function(cases){


    # setkeyv(cases, cols = c("ban", "var"), verbose = FALSE)
    cases$var_order = match(cases$var, sort(unique(cases$var)))
    cases$ban_order = match(cases$ban, sort(unique(cases$ban)))

    cases$is_total_row = FALSE
    # if - temporary until bugfix in values2labels
    # after bugfix there will be no needs in unlab(unclass(...))
    if(is.null(val_lab(cases$var))){
        cases$var_vallabs = unlab(cases$var)
    } else {
        cases$var_vallabs = unlab(unclass(values2labels(cases$var)))
    }
    if(is.null(val_lab(cases$ban))){
        cases$ban_vallabs = unlab(cases$ban)
    } else {
        cases$ban_vallabs = unlab(unclass(values2labels(cases$ban)))
    }

    # var_label =  var_lab(dttbl[, var_names[1], with = FALSE])
    # ban_label =  var_lab(dttbl[, ban_names[1], with = FALSE])
    var_label =  var_lab(cases$var)
    ban_label =  var_lab(cases$ban)
    if(is.null(var_label)){
        cases$var_label = ""
    } else {
        cases$var_label = var_label
    }
    if(is.null(ban_label)){
        cases$ban_label = ""
    } else {
        cases$ban_label = ban_label
    }
    cases


}


#############################################################

long_table = function(row_vars, col_vars,
                      weight = NULL,
                      total_row_position = c("above", "below", "none"),
                      table_type = c("default", "rpct", "tpct"),
                      weighted_total = FALSE){

    row_vars = flat_list(dichotomy_to_category_encoding(row_vars), flat_df = FALSE) # process_mdsets
    row_vars = rapply(row_vars, as.labelled, classes = c("factor", "POSIXct"), how = "replace")

    col_vars = flat_list(dichotomy_to_category_encoding(col_vars), flat_df = FALSE) # process_mdsets
    col_vars = rapply(col_vars, as.labelled, classes = c("factor", "POSIXct"), how = "replace")
    
    check_sizes("table_cases", row_vars, col_vars, weight)

    new_vars_names = lapply(seq_along(row_vars),
               function(each) paste0("v",each, "_", seq_len(NCOL(row_vars[[each]])))
        )
    new_bans_names = lapply(seq_along(col_vars),
               function(each) paste0("b",each, "_", seq_len(NCOL(col_vars[[each]])))
        )
    row_vars = flat_list(lapply(row_vars, function(x) if(is.data.frame(x)) as.list(x) else x))
    col_vars = flat_list(lapply(col_vars, function(x) if(is.data.frame(x)) as.list(x) else x))
    curr_dt = as.data.table(c(row_vars, col_vars))
    colnames(curr_dt) = c(new_vars_names, new_bans_names, recursive = TRUE)
    if (!is.null(weight)) {
        curr_dt$weight = weight
        weight_name = "weight"
    } else {
        weight_name = NULL
    }
    total_row_position = match.arg(total_row_position)
    table_type = match.arg(table_type)
    res = lapply(seq_along(new_vars_names),
                 function(var_num){
                     setkeyv(curr_dt, cols = new_vars_names[[var_num]], verbose = FALSE, physical = TRUE)
            res0 = lapply(seq_along(new_bans_names), function(ban_num){
                dttbl = valid_dttbl(dttbl = curr_dt,
                                    new_vars_names[[var_num]],
                                    new_bans_names[[ban_num]],
                                    weight_name = weight_name)

                cases = elementary_cases(dttbl,
                                         var_names = new_vars_names[[var_num]],
                                         ban_names = new_bans_names[[ban_num]],
                                         weight_name = weight_name)

                cases = add_unused_labels(cases, "ban", fill_na = "cases")
                cases = add_unused_labels(cases, "var", fill_na = "cases")

                cases = label_elementary_cases(cases)

                total = get_total(dttbl, new_bans_names[[ban_num]], weight_name)
                if (table_type %in% "rpct"){
                    # browser()
                    var_total = get_total(dttbl, new_vars_names[[var_num]], weight_name)
                    table_total = data.table(var = var_total$ban, var_total = var_total$cases)
                    setkeyv(table_total, cols = c("var"), verbose = FALSE)
                    setkeyv(cases, cols = c("var"), verbose = FALSE)
                    cases = table_total[cases]
                    setkeyv(cases, cols = c("ban", "var"), verbose = FALSE)
                }
                if (table_type %in% "tpct"){
                    # browser()
                    if(is.null(weight_name)){
                        cases$table_total = nrow(dttbl)
                    } else {
                        cases$table_total = dttbl[, sum(weight, na.rm = TRUE)]
                    }

                }
                total = add_unused_labels(total, "ban")

                elementary_res = add_total(cases, total, total_row_position, weighted_total)

                for (each in seq_along(elementary_res)){
                    if(is.factor(elementary_res[[each]])){
                        elementary_res[[each]] = as.character(elementary_res[[each]])
                    }
                }

                elementary_res$var_num = var_num
                elementary_res$ban_num = ban_num

                elementary_res
        })
        rbindlist(res0, use.names = TRUE, fill = TRUE)
    })
    rbindlist(res, use.names = TRUE, fill = TRUE)
}











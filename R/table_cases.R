#' Title
#'
#' @param row_vars sdds
#' @param col_vars sdsd
#' @param weight sddssd
#' @param total_row_position sdsdds
#' @param total_row_title sdsdd
#' @param weighted_total dssds
#' @param variable_labels_position sddsds
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' data(mtcars)
#'
#' # apply labels
#' mtcars = modify(mtcars,{
#'     var_lab(cyl) = "Number of cylinders"
#'     var_lab(vs) = "Engine"
#'     val_lab(vs) = c("V-engine" = 0,
#'                     "Straight engine" = 1)
#'     var_lab(am) = "Transmission"
#'     val_lab(am) = c(automatic = 0,
#'                     manual=1)
#'     var_lab(gear) = "Number of forward gears"
#'     var_lab(carb) = "Number of carburetors"
#' })
#'
#' # simple frequency table
#' table_cases(mtcars$vs, mtcars$am)
#'
#' # Total column, several col_vars and variables, column percent
#' with(mtcars,
#'      table_cpct(list(cyl, gear, carb), list("#total", vs, am))
#'      )
#'
#' # the same table without total column and total row
#' with(mtcars,
#'      table_cpct(list(cyl, gear, carb), list(vs, am), total_row_position = "none")
#'      )
#'
table_cases = function(row_vars, col_vars, weight = NULL,
                       total_row_position = c("below", "above", "none"),
                       total_row_title = "#Total",
                       weighted_total = FALSE,
                       variable_labels_position = c("rows", "inside_columns", "outside_columns")
){
    if(!is_list(row_vars)) row_vars = list(row_vars)
    if(!is_list(col_vars)) col_vars = list(col_vars)
    total_row_position = match.arg(total_row_position)
    long = long_table(row_vars = row_vars, col_vars = col_vars,
                      weight = weight,
                      total_row_position = total_row_position,
                      weighted_total = weighted_total
    )
    if(total_row_position!="none"){
        long$var_vallabs[long$is_total_row] = total_row_title
    }
    long$ban_total = NULL
    long$stats = long$cases
    variable_labels_position = match.arg(variable_labels_position)
    res = long_to_wide(long, variable_labels_position = variable_labels_position)
    class(res) = union(c("table_cases", "etable"), class(res))
    res
}



#' @rdname table_cases
#' @export
table_cpct = function(row_vars, col_vars, weight = NULL,
                       total_row_position = c("below", "above", "none"),
                       total_row_title = "#Total",
                       weighted_total = FALSE,
                       variable_labels_position = c("rows", "inside_columns", "outside_columns")
){
    if(!is_list(row_vars)) row_vars = list(row_vars)
    if(!is_list(col_vars)) col_vars = list(col_vars)
    total_row_position = match.arg(total_row_position)
    long = long_table(row_vars = row_vars, col_vars = col_vars,
                      weight = weight,
                      total_row_position = total_row_position,
                      weighted_total = weighted_total
    )
    if(total_row_position!="none"){
        long$var_vallabs[long$is_total_row] = total_row_title
    }

    long$stats = ifelse(long$is_total_row, long$cases,
                        ifelse(long$ban_total>0, long$cases*100/long$ban_total, NA)
                        )
    long$ban_total = NULL
    variable_labels_position = match.arg(variable_labels_position)
    res = long_to_wide(long, variable_labels_position = variable_labels_position)
    class(res) = union(c("table_cpct", "etable"), class(res))
    res
}

#' @rdname table_cases
#' @export
table_rpct = function(row_vars, col_vars, weight = NULL,
                      total_row_position = c("below", "above", "none"),
                      total_row_title = "#Total",
                      weighted_total = FALSE,
                      variable_labels_position = c("rows", "inside_columns", "outside_columns")
){
    if(!is_list(row_vars)) row_vars = list(row_vars)
    if(!is_list(col_vars)) col_vars = list(col_vars)

    total_row_position = match.arg(total_row_position)
    long = long_table(row_vars = row_vars, col_vars = col_vars,
                      weight = weight,
                      total_row_position = total_row_position,
                      table_type = "rpct",
                      weighted_total = weighted_total
    )

    if(total_row_position!="none"){
        long$var_vallabs[long$is_total_row] = total_row_title
    }

    long$stats = ifelse(long$is_total_row, long$cases,
                                     ifelse(long$var_total>0, long$cases*100/long$var_total, NA)
    )
    long$ban_total = NULL
    long$var_total = NULL
    variable_labels_position = match.arg(variable_labels_position)
    res = long_to_wide(long, variable_labels_position = variable_labels_position)
    class(res) = union(c("table_rpct", "etable"), class(res))
    res
}


#' @rdname table_cases
#' @export
table_tpct = function(row_vars, col_vars, weight = NULL,
                      total_row_position = c("below", "above", "none"),
                      total_row_title = "#Total",
                      weighted_total = FALSE,
                      variable_labels_position = c("rows", "inside_columns", "outside_columns")
){
    if(!is_list(row_vars)) row_vars = list(row_vars)
    if(!is_list(col_vars)) col_vars = list(col_vars)

    total_row_position = match.arg(total_row_position)
    long = long_table(row_vars = row_vars, col_vars = col_vars,
                      weight = weight,
                      total_row_position = total_row_position,
                      table_type = "tpct",
                      weighted_total = weighted_total
    )

    if(total_row_position!="none"){
        long$var_vallabs[long$is_total_row] = total_row_title
    }

    long$stats = ifelse(long$is_total_row, long$cases,
                        ifelse(long$table_total>0, long$cases*100/long$table_total, NA)
    )
    long$ban_total = NULL
    long$table_total = NULL
    variable_labels_position = match.arg(variable_labels_position)
    res = long_to_wide(long, variable_labels_position = variable_labels_position)
    class(res) = union(c("table_tpct", "etable"), class(res))
    res
}


long_to_wide = function(long, variable_labels_position = c("rows", "inside_columns", "outside_columns")){
    long = as.data.table(long)
    setkeyv(long, cols = c("var_num", "ban_num", "var_order"), verbose = FALSE)
    if (variable_labels_position == "rows"){
        res = dcast.data.table(long, var_num + var_order + var_label + var_vallabs ~ ban_num + ban + ban_label + ban_vallabs, sep = "|",
                         value.var = list("stats"), fill = NA)
        colnames(res) = gsub("^[\\d]+\\|.*?\\|", "", colnames(res), perl = TRUE)
        row_labels = paste0(res$var_label, "|", res$var_vallabs)
        res$var_num = NULL
        res$var_order = NULL
        res$var_label = NULL
        res$var_vallabs = NULL
    } else {
        # we recalculate 'var_order' for variable_labels_position in c("inside_columns", "outside_columns")
        # because we need that same value in each variable has the same var_order.
        # In opposite case different values will erroneously placed in one row.
        # We could make this operation for all table types in 'long_table' function
        # but then we will have some problems in var_ordering in case of mixed
        # types variables (e. g. one variable is numeric and other is character).
        var_not_total = long$var[!long$is_total_row]
        new_var_order = match(var_not_total, sort(unique(var_not_total)))
        long$var_order[!long$is_total_row] = new_var_order
        if(any(long$is_total_row)){
            # if total_row_position = "below"
            if(long$var_order[long$is_total_row][1] > 0){
                max_var_order = suppressWarnings(max(new_var_order, na.rm = TRUE))
                long$var_order[long$is_total_row] = ifelse(is.finite(max_var_order), max_var_order + 1, 1)
            }

        }
    }
    if (variable_labels_position=="inside_columns"){
        res = dcast.data.table(long, var_order  + var_vallabs ~ ban_num + ban + var_num + ban_label + ban_vallabs  + var_label,
                               sep = "|",
                               value.var = list("stats"), fill = NA)
        row_labels = res$var_vallabs
        res$var_order = NULL
        res$var_vallabs = NULL
        colnames(res) = gsub("^[\\d]+\\|.*?\\|[\\d]+\\|", "", colnames(res), perl = TRUE)
    }
    if (variable_labels_position=="outside_columns"){
        res = dcast.data.table(long, var_order  + var_vallabs ~ var_num + ban_num + ban + var_label + ban_label + ban_vallabs,
                               sep = "|",
                               value.var = list("stats"), fill = NA)
        row_labels = res$var_vallabs
        res$var_order = NULL
        res$var_vallabs = NULL
        colnames(res) = gsub("^[\\d]+\\|[\\d]+\\|.*?\\|", "", colnames(res), perl = TRUE)
    }
    res = as.dtfrm(res)
    colnames(res) = remove_unnecessary_splitters(colnames(res))
    row_labels = remove_unnecessary_splitters(row_labels)
    res = dtfrm(row_labels, res)
    class(res) = union("etable", class(res))
    res
}



#' Title
#'
#' @param summary_vars vector/data.frame/list - variables for summary. If ommited
#'   then variables will be used as summary variables.
#' @param col_vars fss
#' @param fun the function to be applied to summary variables
#' @param weight dsvfv
#' @param row_vars fdfg
#' @param stat_names cdsdc
#' @param row_labels csdsd
#' @param col_labels dcdscds
#' @param hide dcsdc
#'
#' @return table
#' @export
#'
#' @examples
#' a = 1
table_summary = function(summary_vars,
                         col_vars,
                         fun,
                         weight = NULL,
                         row_vars = NULL,
                         stat_names = NULL,
                         row_labels = c("row_vars", "row_vars_values", "summary_vars", "stat_names"),
                         col_labels = c("col_vars", "col_vars_values"),
                         hide = NULL
){
    if(is.null(stat_names)){
        if(is.character(fun)){
            stat_names = fun    
        } else {
            stat_names = deparse(substitute(fun))    
        }
    }
    fun = match.fun(fun)
    # stopif(!is.function(fun), "`fun` should be function.")
    if(!is.null(weight)){
        stopif(!("weight" %in% names(formals(fun))),
               "`weight` is provided but `fun` doesn't have formal `weight` argument.")
    }
 

    stopif(!length(row_labels), "`row_labels` should have at least one item.")
    stopif(!length(col_labels), "`col_labels` should have at least one item.")
    stopif(anyDuplicated(row_labels),
           "`row_labels` has duplicated values: ",
           paste(row_labels[duplicated(row_labels)], collapse = ", "))
    stopif(anyDuplicated(col_labels),
           "`col_labels` has duplicated values: ",
           paste(col_labels[duplicated(col_labels)], collapse = ", "))
    possible_values =  c("row_vars", "row_vars_values", "summary_vars",
                         "stat_names", "col_vars", "col_vars_values")
    unknowns = setdiff(row_labels, possible_values)
    stopif(length(unknowns),
           "unknown values in `row_labels`: ", paste(unknowns, collapse = ", "))
    unknowns = setdiff(col_labels, possible_values)
    stopif(length(unknowns),
           "unknown values in `col_labels`: ", paste(unknowns, collapse = ", "))
    unknowns = setdiff(hide, possible_values)
    stopif(length(unknowns),
           "unknown values in `hide`: ", paste(unknowns, collapse = ", "))
    unknowns = setdiff(possible_values, c(row_labels, col_labels))
    stopif(length(unknowns),
           "some items are missing in `col_labels` or `row_labels`:", paste(unknowns, collapse = ", "))
    recode(row_labels) = c("summary_vars" ~ "..summary_vars__", "stat_names" ~ "..stat_names__")
    recode(col_labels) = c("summary_vars" ~ "..summary_vars__", "stat_names" ~ "..stat_names__")
    recode(hide) = c("summary_vars" ~ "..summary_vars__", "stat_names" ~ "..stat_names__")
    hide = c(hide, "..stat_names__index", "..summary_vars__index")
    insert_value_before(row_labels, "..summary_vars__") = "..summary_vars__index"
    insert_value_before(col_labels, "..summary_vars__") = "..summary_vars__index"
    insert_value_before(row_labels,  "..stat_names__") = "..stat_names__index"
    insert_value_before(col_labels,  "..stat_names__") = "..stat_names__index"

    custom_fun = function(stat_names, int_fun){
        ..weight__ = NULL  # to pass CRAN check
        force(stat_names)
        force(int_fun)
        function(dttbl, weight = NULL){
            dttbl = names2labels(dttbl)
            if(is.null(weight)){
                dttbl = dttbl[, lapply(.SD, int_fun)]
            } else {
                dttbl[["..weight__"]] = weight
                dttbl = dttbl[, lapply(.SD[, -"..weight__"], int_fun, weight = ..weight__)]
            }
            if(nrow(dttbl)==0){
                dttbl = rbind(dttbl, set_names(data.table(NA), names(dttbl)[1]), fill = TRUE, use.names = TRUE)
            }
            nrow_dttbl = nrow(dttbl)
            ncol_dttbl = ncol(dttbl)
            dttbl = cbind(dttbl, "..stat_names__" = stat_names)
           # browser()
            dttbl[["..stat_names__index"]] = seq_len(nrow_dttbl)
            res = suppressWarnings(melt(dttbl, id.vars = c("..stat_names__index", "..stat_names__"),
                                        variable.name = "..summary_vars__",
                                        value.name = "value",
                                        na.rm = FALSE,
                                        variable.factor = FALSE,
                                        value.factor = FALSE,
                                        verbose = FALSE)
            )
            res[["..summary_vars__index"]] = rep(seq_len(ncol_dttbl), each = nrow_dttbl)
            res
        }
    }
    fun = custom_fun(stat_names = stat_names, int_fun = fun)
    res = table_summary_df(summary_vars = summary_vars,
                           col_vars = col_vars,
                           fun = fun,
                           weight = weight,
                           row_vars = row_vars,
                           row_labels = row_labels,
                           col_labels = col_labels,
                           hide = hide,
                           use_result_row_order = FALSE
                           )
    class(res) = union("table_summary", class(res) %d% "table_summary_df")
    res

}

#' @export
#' @rdname table_summary
table_means = function(summary_vars,
                         col_vars,
                         weight = NULL,
                         row_vars = NULL,
                         stat_names = c("Mean", "Std. Dev.", "N")
){
    stopif(length(stat_names)!=3L, "Length of `stat_names`` should be equal to 3.")
    custom_fun  = function(x, weight = NULL){
        c(  w_mean(x, weight = weight),
            w_sd(x, weight = weight),
            w_n(x, weight = NULL)
            )
    }
    res = table_summary(
        summary_vars = summary_vars,
        col_vars = col_vars,
        fun = custom_fun,
        weight = weight,
        row_vars = row_vars,
        stat_names = stat_names
    )
    class(res) = union("table_means", class(res))
    res

}

#' @export
#' @rdname table_summary
table_medians = function(summary_vars,
                       col_vars,
                       weight = NULL,
                       row_vars = NULL,
                       stat_names = c("Median", "N")
){
    stopif(length(stat_names)!=2L, "Length of `stat_names`` should be equal to 3.")
    custom_fun  = function(x, weight = NULL){
        c(  w_median(x, weight = weight),
            w_n(x, weight = NULL)
        )
    }
    res = table_summary(
        summary_vars = summary_vars,
        col_vars = col_vars,
        fun = custom_fun,
        weight = weight,
        row_vars = row_vars,
        stat_names = stat_names
    )
    class(res) = union("table_medians", class(res))
    res

}

#' @export
#' @rdname table_summary
table_sums = function(summary_vars,
                         col_vars,
                         weight = NULL,
                         row_vars = NULL,
                         stat_names = c("Sum", "N")
){
    stopif(length(stat_names)!=2L, "Length of `stat_names`` should be equal to 3.")
    custom_fun  = function(x, weight = NULL){
        c(  w_sum(x, weight = weight),
            w_n(x, weight = NULL)
        )
    }
    res = table_summary(
        summary_vars = summary_vars,
        col_vars = col_vars,
        fun = custom_fun,
        weight = weight,
        row_vars = row_vars,
        stat_names = stat_names
    )
    class(res) = union("table_sums", class(res))
    res

}














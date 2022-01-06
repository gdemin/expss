#' Add rows to data.frame/matrix/table
#'
#' Take a sequence of vector, matrix or data-frame arguments and combine by
#' rows. Contrary to \link[base:cbind]{rbind} it handles non-matching column
#' names. 
#' There is also special method for the results of
#' \link{cro}/\link{cro_fun}/\link{tables}/\link{fre}.
#'
#' @param ... data.frame/matrix/table for binding
#' @param nomatch_columns action if there are non-matching columns between
#'   data.frames. Possible values are \code{"add"}, \code{"drop"}, \code{"stop"}.
#'   \code{"add"} will combine all columns, \code{"drop"} will leave only common
#'   columns, \code{"stop"} will raise an error.
#'
#' @return See \link[base:cbind]{rbind}, \link{cro}, \link{cro_fun}, \link{fre}, \link{tables} 
#' @export
#'
#' @examples
#' a = data.frame(x = 1:5, y = 6:10)
#' b = data.frame(y = 6:10, z = 11:15)
#'
#' add_rows(a, b) # x, y, z
#'
#' add_rows(a, b, nomatch_columns = "drop")  # y
#'
add_rows = function(...){
    UseMethod("add_rows")
}

#' @rdname add_rows
#' @export
add_rows.data.frame = function(..., nomatch_columns = c("add", "drop", "stop")){
    args = list(...)
    is_dt = unlist(lapply(args, is.data.table), use.names = FALSE)
    if(any(is_dt)){
        # not optimal solution for data.table 
        # TODO all should be rewritten with rbindlist
        args = lapply(list(...), as.sheet)        
    }
    
    f = function(x, y){
        force(nomatch_columns)
        add_rows1(x, y, nomatch_columns = nomatch_columns)
    }
    Reduce(f, args)
}

#' @export
add_rows.huxtable = function(...){
    huxtable::add_rows(...)
}


#' @export
add_rows.default =  function(..., nomatch_columns = c("add", "drop", "stop")){
    rbind(...)
}



#' @export
add_rows.etable = function(..., nomatch_columns = c("add", "drop", "stop")){
    args = list(...)
    all_names = lapply(args, function(x) {
        clnm = colnames(x)
        if(!is.null(clnm)){
            clnm[1]
        } else {
            NULL
        }
    })
    all_names = unique(unlist(all_names))
    if(length(all_names)>1){
        for(each in seq_along(args)){
            if(!is.null(colnames(args[[each]]))){
                colnames(args[[each]])[1] = "row_labels"
            }
        }
    }
    classes = lapply(args, class)
    new_class = Reduce('%i%', classes)
    res = do.call(add_rows.data.frame, c(args, list(nomatch_columns = nomatch_columns)))
    if (!("data.frame" %in% new_class)) new_class = union("data.frame", new_class)
    if (!("etable" %in% new_class)) new_class = union("etable", new_class)
    rownames(res) = NULL
    class(res) = new_class
    res
}



add_rows1 = function(x, y, nomatch_columns = c("add", "drop", "stop")){
    nomatch_columns = match.arg(nomatch_columns)
    if(is.matrix(y)) y = as.sheet(y)
    if(is.data.frame(y)){
        true_names_x = colnames(x)
        true_names_y = colnames(y)

        colnames(x) = make.names(colnames(x), unique = TRUE)
        colnames(y) = make.names(colnames(y), unique = TRUE)
        temp_names_x = colnames(x)
        temp_names_y = colnames(y)
        temp_names = c(temp_names_x, temp_names_y)
        true_names = c(true_names_x, true_names_y)[!duplicated(temp_names)]
        temp_names = temp_names[!duplicated(temp_names)]
        
        new_in_y = colnames(x) %d% colnames(y)
        new_in_x = colnames(y) %d% colnames(x)
        if(length(new_in_y)>0 | length(new_in_x)>0){
            if(nomatch_columns == "add"){
                x[, new_in_x] = NA
                y[, new_in_y] = NA
                for (each_new in new_in_x){
                    val_lab(x[[each_new]]) = val_lab(y[[each_new]])
                    var_lab(x[[each_new]]) = var_lab(y[[each_new]])
                }

            }
            if(nomatch_columns == "drop"){
                x = x[, colnames(x) %d% new_in_y, drop = FALSE]
                y = y[, colnames(y) %d% new_in_x, drop = FALSE]

            }
            stopif(nomatch_columns == "stop", "Different column names in 'x' and 'y'.")
        }
        res = rbind.data.frame(x, y, stringsAsFactors = FALSE)
        recode(colnames(res)) = from_to(from = temp_names, to = true_names)
        res
    } else {
        rbind.data.frame(x, y, stringsAsFactors = FALSE)
    }
}


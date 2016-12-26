#' Add rows to data.frame/matrix/table
#'
#' \code{add_rows} is similar to \link[base]{rbind} but it handles non-matching 
#' column names. \code{\%add_rows\%} is an infix version of \code{add_rows}. 
#' There is also special method for the results of \code{cro_*}/\code{fre}. 
#' \code{.add_rows} is version for addding rows to default dataset. See
#' \link{default_dataset}.
#'
#' @param ... data.frame/matrix/table for binding
#' @param x data.frame/matrix/table for binding
#' @param y data.frame/matrix/table for binding
#' @param nomatch_columns action if there are non-matching columns between
#'   data.frames. Possible values are \code{"add"}, \code{"drop"}, \code{"stop"}.
#'   \code{"add"} will combine all columns, \code{"drop"} will leave only common
#'   columns, \code{"stop"} will raise an error.
#'
#' @return See \link[base]{rbind}, \link{cro}, \link{fre}
#' @export
#'
#' @examples
#' a = data.frame(x = 1:5, y = 6:10)
#' b = data.frame(y = 6:10, z = 11:15)
#'
#' add_rows(a, b) # x, y, z
#' a %add_rows% b # the same result
#'
#' add_rows(a, b, nomatch_columns = "drop")  # y
#'
#' # simple tables
#' data(mtcars)
#' 
#' mtcars = modify(mtcars, {
#'             var_lab(mpg) = "Miles/(US) gallon"
#'             var_lab(vs) = "vs"
#'             val_lab(vs) = c("V-engine" = 0, "Straight engine" = 1)
#'             var_lab(am) = "am"
#'             val_lab(am) = c("automatic transmission" = 1, "manual transmission" = 0)
#'             var_lab(gear) = "gear"
#'             var_lab(carb) = "carb"
#' })
#' 
#' tab_mean = with(mtcars, cro_mean(mpg, am))
#' tab_percent = with(mtcars, cro_cpct(vs, am))
#' 
#' tab_mean %add_rows% tab_percent
#'
add_rows = function(...){
    UseMethod("add_rows")
}

#' @rdname add_rows
#' @export
add_rows.data.frame = function(..., nomatch_columns = c("add", "drop", "stop")){
    f = function(x, y){
        force(nomatch_columns)
        add_rows1(x, y, nomatch_columns = nomatch_columns)
    }
    Reduce(f, list(...))
}

#' @export
add_rows.default =  function(...){
    rbind(...)
}



#' @export
add_rows.simple_table = function(..., nomatch_columns = c("add", "drop", "stop")){
    args = list(...)
    for(each in seq_along(args)){
        colnames(args[[each]])[1] = "row_labels"
    }
    classes = lapply(args, class)
    new_class = Reduce('%i%', classes)
    res = do.call(add_rows.data.frame, c(args, list(nomatch_columns = nomatch_columns)))
    if (!("data.frame" %in% new_class)) new_class = union("data.frame", new_class)
    if (!("simple_table" %in% new_class)) new_class = union("simple_table", new_class)
    
    class(res) = new_class
    res
}

#' @export
add_rows.summary_table = add_rows.simple_table

add_rows1 = function(x, y, nomatch_columns = c("add", "drop", "stop")){
    nomatch_columns = match.arg(nomatch_columns)
    if(is.matrix(y)) y = as.dtfrm(y)
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
        res = rbind(x, y, stringsAsFactors = FALSE)
        if_val(colnames(res), from = temp_names) = true_names
        res
    } else {
        rbind(x, y, stringsAsFactors = FALSE)
    }
}

#' @rdname add_rows
#' @export
'%add_rows%' = function(x,y){
    add_rows(x, y)
}

#' @rdname add_rows
#' @export
.add_rows = function(..., nomatch_columns = c("add", "drop", "stop")){
    reference = suppressMessages(default_dataset() )
    data = ref(reference)
    data = add_rows(data, ..., nomatch_columns = nomatch_columns)
    ref(reference) = data
    invisible(data)
}

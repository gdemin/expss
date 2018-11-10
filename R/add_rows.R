#' Add rows to data.frame/matrix/table
#'
#' \code{add_rows} is similar to \link[base]{rbind} but it handles non-matching 
#' column names. \code{\%add_rows\%} is an infix version of \code{add_rows}. 
#' There is also special method for the results of
#' \link{cro}/\link{cro_fun}/\link{tables}/\link{fre}. \code{.add_rows} is
#' version for adding rows to default dataset. See \link{default_dataset}.
#'
#' @param ... data.frame/matrix/table for binding
#' @param x data.frame/matrix/table for binding
#' @param y data.frame/matrix/table for binding
#' @param nomatch_columns action if there are non-matching columns between
#'   data.frames. Possible values are \code{"add"}, \code{"drop"}, \code{"stop"}.
#'   \code{"add"} will combine all columns, \code{"drop"} will leave only common
#'   columns, \code{"stop"} will raise an error.
#'
#' @return See \link[base]{rbind}, \link{cro}, \link{cro_fun}, \link{fre}, \link{tables} 
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
#' # apply labels
#' mtcars = apply_labels(mtcars,
#'                 mpg = "Miles/(US) gallon",
#'                 cyl = "Number of cylinders",
#'                 disp = "Displacement (cu.in.)",
#'                 hp = "Gross horsepower",
#'                 drat = "Rear axle ratio",
#'                 wt = "Weight (lb/1000)",
#'                 qsec = "1/4 mile time",
#'                 vs = "V/S",
#'                 vs = c("V-engine" = 0, "Straight engine" = 1),
#'                 am = "Transmission (0 = automatic, 1 = manual)",
#'                 am = c(automatic = 0, manual = 1),
#'                 gear = "Number of forward gears",
#'                 carb = "Number of carburetors"
#' )
#' 
#' tbl_mean = calculate(mtcars, cro_mean(cyl, am))
#' tbl_percent = calculate(mtcars, cro_cpct(cyl, am))
#' 
#' tbl_mean %add_rows% tbl_percent
#'
add_rows = function(..., nomatch_columns = c("add", "drop", "stop")){
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
        if_val(colnames(res), from = temp_names) = true_names
        res
    } else {
        rbind.data.frame(x, y, stringsAsFactors = FALSE)
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

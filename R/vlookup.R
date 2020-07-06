SPECIALS = c('row.names', 'rownames', 'names')

#' Look up values in dictionary.
#' 
#' \code{vlookup}/\code{vlookup_df} function is inspired by VLOOKUP spreadsheet
#' function. It looks for a \code{lookup_value} in the \code{lookup_column} of
#' the \code{dict}, and then returns values in the same rows from
#' \code{result_column}. \code{add_columns} inspired by MATCH FILES (Add
#' variables...) from SPSS Statistics. It works similar to SQL left join but
#' number of cases in the left part always remain the same. If there are
#' duplicated keys in the \code{dict} then error will be raised by default.
#' \code{.add_columns} is the same function for default dataset.
#' 
#' @param lookup_value Vector of looked up values
#' @param dict data.frame/matrix. Dictionary. Can be vector for
#'   \code{vlookup}/\code{vlookup_df}.
#' @param result_column numeric or character. Resulting columns of \code{dict}.
#'   There are special values: 'row.names', 'rownames', 'names'. If
#'   \code{result_column} equals to one of these special values and \code{dict}
#'   is matrix/data.frame then row names of \code{dict} will be returned. If
#'   \code{dict} is vector then names of vector will be returned. For
#'   \code{vlookup_df} default \code{result_column} is NULL and result will be
#'   entire rows. For \code{vlookup} defaut \code{result_column} is 2 - for
#'   frequent case of dictionary with keys in the first column and results in
#'   the second column.
#' @param lookup_column Column of \code{dict} in which lookup value will be
#'   searched. By default it is the first column of the \code{dict}. There are
#'   special values: 'row.names', 'rownames', 'names'. If lookup_column equals
#'   to one of these special values and \code{dict} is matrix/data.frame then
#'   values will be searched in the row names of \code{dict}. If \code{dict} is
#'   vector then values will be searched in names of the \code{dict}.
#' @param data data.frame to be joined with \code{dict}.
#' @param by character vector or NULL(default) or 1. Names of common variables
#'   in the \code{data} and \code{dict} by which we will attach \code{dict} to
#'   \code{data}. If it is NULL then common names will be used. If it is equals
#'   to 1 then we will use the first column from both dataframes. To add columns
#'   by different variables on \code{data} and \code{dict} use a named vector.
#'   For example, \code{by = c("a" = "b")} will match data.a to dict.b.
#' @param ignore_duplicates logical Should we ignore duplicates in the \code{by}
#'   variables in the \code{dict}? If it is TRUE than first occurence of duplicated
#'   key will be used.
#' @return \code{vlookup} always return vector, \code{vlookup_df} always returns
#'   data.frame. \code{row.names} in result of \code{vlookup_df} are not
#'   preserved.
#'   
#' @export
#' @examples
#' # with data.frame
#' dict = data.frame(num=1:26, small=letters, cap=LETTERS, stringsAsFactors = FALSE)
#' rownames(dict) = paste0('rows', 1:26)
#' identical(vlookup_df(1:3, dict), dict[1:3,]) # should be TRUE

#' vlookup(c(45,1:3,58), dict, result_column='cap')
#' vlookup_df(c('z','d','f'), dict, lookup_column = 'small')
#' vlookup_df(c('rows7', 'rows2', 'rows5'), dict, lookup_column = 'row.names')
#' 
#' # with vector
#' dict=1:26
#' names(dict) = letters
#' 
#' vlookup(c(2,4,6), dict, result_column='row.names')
#' 
#' # The same results
#' vlookup(c(2,4,6), dict, result_column='rownames')
#' vlookup(c(2,4,6), dict, result_column='names')
#' 
#' # example for 'add_columns' from base 'merge'
#' authors = sheet(
#'     surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
#'     nationality = c("US", "Australia", "US", "UK", "Australia"),
#'     deceased = c("yes", rep("no", 4))
#' )
#' 
#' books = sheet(
#'     surname = c("Tukey", "Venables", "Tierney",
#'                 "Ripley", "Ripley", "McNeil", "R Core"),
#'     title = c("Exploratory Data Analysis",
#'               "Modern Applied Statistics ...",
#'               "LISP-STAT",
#'               "Spatial Statistics", "Stochastic Simulation",
#'               "Interactive Data Analysis",
#'               "An Introduction to R")
#' )
#' 
#' add_columns(books, authors)
#' 
#' # Just for fun. Examples borrowed from Microsoft Excel.
#' # It is not the R way of doing things.
#' 
#' # Example 2
#' 
#' ex2 = utils::read.table(header = TRUE, text = "
#'     Item_ID Item Cost Markup 
#'     ST-340 Stroller 145.67  0.30  
#'     BI-567 Bib 3.56  0.40  
#'     DI-328 Diapers  21.45  0.35  
#'     WI-989 Wipes  5.12  0.40  
#'     AS-469 Aspirator 2.56  0.45 
#' ", stringsAsFactors = FALSE)
#' 
#' # Calculates the retail price of diapers by adding the markup percentage to the cost. 
#' vlookup("DI-328", ex2, 3) * (1 + vlookup("DI-328", ex2, 4)) # 28.9575
#' 
#' # Calculates the sale price of wipes by subtracting a specified discount from
#' # the retail price.
#' (vlookup("WI-989", ex2, "Cost") * (1 + vlookup("WI-989", ex2, "Markup"))) * (1 - 0.2)  # 5.7344
#' 
#' A2 = ex2[1, "Item_ID"]
#' A3 = ex2[2, "Item_ID"]
#' 
#' # If the cost of an item is greater than or equal to $20.00, displays the string
#' # "Markup is nn%"; otherwise, displays the string "Cost is under $20.00".
#' ifelse(vlookup(A2, ex2, "Cost") >= 20, 
#'        paste0("Markup is " , 100 * vlookup(A2, ex2, "Markup"),"%"), 
#'        "Cost is under $20.00") # Markup is 30%
#' 
#' 
#' # If the cost of an item is greater than or equal to $20.00, displays the string
#' # Markup is nn%"; otherwise, displays the string "Cost is $n.nn".
#' ifelse(vlookup(A3, ex2, "Cost") >= 20, 
#'        paste0("Markup is: " , 100 * vlookup(A3, ex2, "Markup") , "%"), 
#'        paste0("Cost is $", vlookup(A3, ex2, "Cost"))) #Cost is $3.56
#' 
#' 
#' # Example 3
#' 
#' ex3 = utils::read.table(header = TRUE, text = "
#'     ID  Last_name  First_name  Title Birth_date  
#'     1 Davis Sara 'Sales Rep.'  12/8/1968 
#'     2 Fontana Olivier 'V.P. of Sales' 2/19/1952 
#'     3 Leal Karina 'Sales Rep.' 8/30/1963 
#'     4 Patten Michael 'Sales Rep.' 9/19/1958 
#'     5 Burke Brian 'Sales Mgr.' 3/4/1955 
#'     6 Sousa Luis 'Sales Rep.'  7/2/1963  
#' ", stringsAsFactors = FALSE)
#' 
#' # If there is an employee with an ID of 5, displays the employee's last name;
#' # otherwise, displays the message "Employee not found".
#' if_na(vlookup(5, ex3, "Last_name"), "Employee not found") # Burke
#'
#' # Many employees
#' if_na(vlookup(1:10, ex3, "Last_name"), "Employee not found") 
#' 
#' # For the employee with an ID of 4, concatenates the values of three cells into
#' # a complete sentence.
#' paste0(vlookup(4, ex3, "First_name"), " ",
#'        vlookup(4, ex3, "Last_name"), " is a ", 
#'        vlookup(4, ex3, "Title")) # Michael Patten is a Sales Rep.
vlookup = function(lookup_value, dict, result_column = 2, lookup_column = 1){
    stopif(length(result_column)>1, "result_column shoud be vector of length 1.")
    vlookup_internal(lookup_value = lookup_value, 
                     dict = dict, 
                     result_column = result_column, 
                     lookup_column = lookup_column, df = FALSE)
}


#' @export
#' @rdname vlookup
vlookup_df = function(lookup_value, dict, result_column = NULL, lookup_column = 1) {
    vlookup_internal(lookup_value = lookup_value, 
                     dict = dict, 
                     result_column = result_column, 
                     lookup_column = lookup_column, df = TRUE)
}




vlookup_internal = function(lookup_value, 
                            dict, 
                            result_column = NULL, 
                            lookup_column = 1, 
                            df = TRUE) {
    stopif(is.list(lookup_value) || NCOL(lookup_value)!=1,
           "'vlookup' - incorrect 'lookup_value'. 'lookup_value' should be vector but its class is ", 
           paste(class(lookup_value), collapse = ", "))
    # validate lookup_column
    stopif(length(lookup_column)!=1L,"'vlookup' - 'lookup_column' shoud be vector of length 1.")
    stopif(!is.numeric(lookup_column) && !is.character(lookup_column),
           "'vlookup' - 'lookup_column' shoud be character or numeric.")
    stopif(is.numeric(lookup_column) && max(lookup_column,na.rm = TRUE)>NCOL(dict),
           "'vlookup' - 'lookup_column' is greater than number of columns in the dict.")
    stopif(is.numeric(lookup_column) && any(lookup_column <= 0),
           "'vlookup' - 'lookup_column' should be positive.")
    stopif(is.character(lookup_column) && (is.data.frame(dict) || is.matrix(dict)) && 
               !all(setdiff(lookup_column, SPECIALS) %in% colnames(dict)),
           "'vlookup' - 'lookup_column' doesn't exists in column names of the dict.")
    
    
    # validate result_column
    stopif(!is.null(result_column) && any(is.na(result_column)), "NA's in result_column")
    
    stopif(is.numeric(result_column) && max(result_column,na.rm = TRUE)>NCOL(dict),
           "result_column is greater than number of columns in the dict.")
    stopif(is.character(result_column) && (is.data.frame(dict) || is.matrix(dict)) && 
               !all(setdiff(result_column, SPECIALS) %in% colnames(dict)),
           "some names in result_column doesn't exists in column names of the dict.")
    
    if(is.matrix(dict) || is.data.frame(dict)){
        dict_was_vector = FALSE    
    } else {
        dict_was_vector = TRUE    
    }    
    if(any(SPECIALS %in% result_column) || any(SPECIALS %in% lookup_column)){
        if(is.matrix(dict) || is.data.frame(dict)){
            curr_rowlabs = rownames(dict)
            
        } else {
            curr_rowlabs = names(dict)
            
        }
    }
    if(!is.data.frame(dict)) dict = as.sheet(dict)
    if(any(SPECIALS %in% result_column) || any(SPECIALS %in% lookup_column)){
        dict[["...RRRLLL..."]] = curr_rowlabs
        if(any(SPECIALS %in% result_column)) result_column[result_column %in% SPECIALS] = "...RRRLLL..."
        if(any(SPECIALS %in% lookup_column)) lookup_column[lookup_column %in% SPECIALS] = "...RRRLLL..."
    }
    # calculate index
    ind = fast_match(lookup_value, dict[[lookup_column]], NA_incomparable = FALSE)
    ### calculate result
    if(df){
        if (is.null(result_column)){
            result = subset_dataframe(dict, ind, drop = FALSE)
        } else {
            
            result = subset_dataframe(dict, ind, drop = FALSE)[, result_column, drop = FALSE]
            
        }
        colnames(result)[colnames(result) %in% "...RRRLLL..."] = "row_names"
        # if(dict_was_vector) rownames(result) = NULL
    } else {
        if (is.null(result_column)){
            result = ind
        } else {
            result = dict[[result_column]][ind]
        }
           
    }
    result
}



#' @export
#' @rdname vlookup
add_columns = function(data, dict, 
                       by = NULL, 
                       ignore_duplicates = FALSE
){
    UseMethod("add_columns")
    
}

#' @export
add_columns.data.frame = function(data, dict, 
                       by = NULL, 
                       ignore_duplicates = FALSE
){
    if(!is.data.frame(data)) data = as.sheet(data)
    if(!is.data.frame(dict)) dict = as.sheet(dict)
    # ..by_data = NULL
    # ..by = NULL
    colnames_data = colnames(data)
    colnames_dict = colnames(dict)
    if(is.null(by)){
        by = intersect(colnames_data, colnames_dict)
        stopif(length(by)==0, "'add_columns' - no common column names between 'data' and 'dict'.")
        message(paste0("Adding columns by ", paste(dQuote(by), collapse = ", ")))
    }
    if(identical(by, 1) || identical(by, 1L)){
        lookup_value = data[[1]]    
        lookup_column = dict[[1]]   
        col_nums_dict = 1
    } else {
        stopif(!is.character(by), "'add_columns' - 'by' should be character, NULL or 1.")
        if(!is.null(names(by))){
            by_data = names(by)
            by_data[by_data==""] = by[by_data==""]
        } else {
            by_data = by
        }
        stop_if_columns_not_exist(colnames_data, by_data)
        stop_if_columns_not_exist(colnames_dict, by)
        if(length(by)>1){
            stopif(anyDuplicated(by), "'add_columns'- duplicated variable names in 'by': ", 
                   paste(dQuote(by[duplicated(by)]), collapse = ", "))
            stopif(anyDuplicated(by_data), "'add_columns'- duplicated variable names in 'by': ", 
                   paste(dQuote(by_data[duplicated(by_data)]), collapse = ", "))
            if(is.data.table(data)){
                lookup_value = data[ , by_data, with = FALSE]     
            } else {
                lookup_value = data[ , by_data]
            }
            if(is.data.table(dict)){
                lookup_column = dict[ , by, with = FALSE]                     
            } else {
                lookup_column = dict[ , by] 
            }
            lookup_value = do.call("paste", c(unlab(lookup_value), sep = "\r"))
            lookup_column = do.call("paste", c(unlab(lookup_column), sep = "\r"))
        } else {
            lookup_value = data[[by_data]]    
            lookup_column = dict[[by]]  
        }
        col_nums_dict = match(by, colnames_dict)
    }
    if(!ignore_duplicates){
        stopif(anyDuplicated(lookup_column), 
               "'add_columns' - duplicated values in 'by' columns in 'dict'")
    }
    # calculate index
    ind = fast_match(lookup_value, lookup_column, NA_incomparable = FALSE)
    if(is.data.table(dict)){
        res = subset_dataframe(dict[,-col_nums_dict, with = FALSE], ind, drop = FALSE)    
    } else {
        res = subset_dataframe(dict[,-col_nums_dict, drop = FALSE], ind, drop = FALSE)    
    }
    
    # make unique names in res
    colnames_res = colnames(res)
    dupl = intersect(colnames_res, colnames_data)
    if(length(dupl)>0){
        warning(
            paste0("'add_columns' - some names in 'dict' duplicate names in 'data': ",
                   paste(dupl, collapse = ", ")
                   )
            )
        all_names = make.unique(c(colnames_data, colnames_res), sep = "_")
        # we change only dictionary names 
        colnames(res) = all_names[-seq_along(colnames_data)]
    }
    if(is.data.table(data)){
        data[, colnames(res):=res]    
    } else {
        data[, colnames(res)] = res    
    }
    
    data

}

#' @export
add_columns.huxtable = function(...){
    huxtable::add_columns(...)
}

#' @export
#' @rdname vlookup
.add_columns = function (dict, 
                         by = NULL, 
                         ignore_duplicates = FALSE) {
    reference = suppressMessages(default_dataset())
    data = ref(reference)
    data = add_columns(data, 
                       dict = dict, 
                       by = by, 
                       ignore_duplicates = ignore_duplicates)
    ref(reference) = data
    invisible(data)
}
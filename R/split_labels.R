#' Split character vector to matrix/split columns in data.frame
#' 
#' \code{split_labels}/\code{split_columns} are auxiliary functions for 
#' post-processing tables resulted from \link{cro}/\link{cro_fun} and etc. In
#' these tables all labels collapsed in the first column with "|" separator.
#' \code{split_columns} split first column into multiple columns with separator
#' (\code{split} argument). \code{split_table_to_df} split first column of table
#' and column names. Result of this operation is data.frame with character
#' columns.
#'
#' @param data data.frame vector which will be split
#' @param x character vector which will be split
#' @param columns character/numeric/logical  columns in the data.frame
#'   \code{data} which should be split
#' @param remove_repeated logical. Default is \code{TRUE}. Should we remove repeated labels? 
#' @param split character vector (or object which can be coerced to such)
#'   containing regular expression(s) (unless \code{fixed = TRUE}) to use for
#'   splitting.
#' @param fixed logical. If TRUE match split exactly, otherwise use regular
#'   expressions. Has priority over \code{perl}.
#' @param perl logical. Should Perl-compatible regexps be used?
#' @param number_of_columns integer. Number of columns from row labels which
#'   will be used as subheadings in table.
#' @param digits numeric. How many digits after decimal point should be left in
#'   \code{split_table_to_df}?
#' @return \code{split_labels} returns character matrix, \code{split_columns} returns
#'   data.frame with columns replaced by possibly multiple columns with split
#'   labels. \code{split_table_to_df} returns data.frame with character columns.
#' @export
#' @seealso \link[base]{strsplit}
#' @examples
#' data(mtcars)
#'
#' # apply labels
#' mtcars = apply_labels(mtcars,
#'     cyl = "Number of cylinders",
#'     vs = "Engine",
#'     vs = c("V-engine" = 0,
#'                     "Straight engine" = 1),
#'     am = "Transmission",
#'     am = c(automatic = 0,
#'                     manual=1),
#'     gear = "Number of forward gears",
#'     carb = "Number of carburetors"
#' )
#' 
#' # all row labels in the first column
#' tabl = mtcars %>% 
#'        calculate(cro_cpct(list(cyl, gear, carb), list(total(), vs, am)))
#' 
#' tabl # without subheadings
#' 
#' make_subheadings(tabl) # with subheadings
#'               
#' split_labels(tabl[[1]])
#' split_labels(colnames(tabl))
#' 
#' # replace first column with new columns 
#' split_columns(tabl) # remove repeated
#' 
#' split_columns(tabl, remove_repeated = FALSE)
#' 
#' split_columns(tabl)
#' 
#' split_table_to_df(tabl)
#' 
#' split_table_to_df(tabl)
#' 
#' 
split_labels = function(x, remove_repeated = TRUE, split = "\\|", fixed = FALSE, perl = FALSE){
    if(length(x)==0){
        return(matrix(NA, ncol=0, nrow = 0))
    }
    if(!is.character(x)) x = as.character(x)
    zero_length_strings = (x == "") & !is.na(x) # strange behaviour of strsplit with ""
    x_split = strsplit(x, split = split, fixed = fixed, perl = perl)  

    if(any(zero_length_strings)){
        x_split[zero_length_strings] = "" 
    }
    max_length = max(lengths(x_split))
    x_split = lapply(x_split, function(each) {
        if(length(each)<max_length){
            each = c(each, rep(NA, max_length - length(each)))
        }
        each
    })
    res = do.call(rbind, x_split)
    res[is.na(res)] = ""
    if (remove_repeated){
        for(i in rev(seq_len(nrow(res))[-1])){
            repeats = res[i-1, ] ==  res[i, ]
            first_no_repeat = which(!repeats)[1]-1
            if(is.na(first_no_repeat)) first_no_repeat = NCOL(res)
            if(!is.na(first_no_repeat) && first_no_repeat>0){
                res[i, 1:first_no_repeat] = ""
            }
            
        }
    }
    res    
}

#' @export
#' @rdname split_labels
split_columns  = function(data, 
                          columns = 1, 
                          remove_repeated = TRUE, 
                          split = "\\|", 
                          fixed = FALSE, 
                          perl = FALSE){
   UseMethod("split_columns")
}


#' @export
split_columns.intermediate_table = function(data, 
                                            columns = 1, 
                                            remove_repeated = TRUE, 
                                            split = "\\|", 
                                            fixed = FALSE, 
                                            perl = FALSE
                                            ){
    stop("No results for splitting. Use 'split_columns' after 'tab_pivot'.")
}


#' @export
split_columns.data.frame  = function(data, 
                                     columns = 1, 
                                     remove_repeated = TRUE, 
                                     split = "\\|", 
                                     fixed = FALSE, 
                                     perl = FALSE){
    stopif(!is.numeric(columns) && !is.character(columns) &&
               !is.logical(columns),
           "`columns` should be character, numeric or logical.")
    
    if(is.character(columns)) {
        stopif(!all(columns %in% colnames(data)), "some of the 'columns' not found: ",
               paste(columns %d% colnames(data), collapse = ", ")
        )
        columns = which(colnames(data) %in%  columns)
    }
    if(is.numeric(columns)) {
        stopif(!all(columns %in% seq_along(data)) , "some of the 'columns' not found: ",
               paste(columns %d% seq_along(data), collapse = ", ")
        )
        
    }
    if(is.logical(columns)){ 
        stopif(length(columns) != ncol(data),
               "length of the 'columns' not equals to number of columns in 'data'"
        )
        columns = which(columns)
    }
    class_data = class(data)
    first_column_name = colnames(data)[1]
    columns = sort(unique(columns))
    for(each_column in rev(columns)){
        curr_col = data[[each_column]]
        if(is.factor(curr_col)) curr_col = as.character(curr_col)
        new_columns = as.sheet(
            split_labels(curr_col, 
                         remove_repeated = remove_repeated, 
                         split = split, 
                         fixed = fixed, 
                         perl = perl)
        )

        colnames(new_columns) = paste0("..new_columns__",each_column,"_", seq_len(ncol(new_columns)))
        # to prevent name changes
        part1_names = colnames(data)[seq_len(each_column)[-each_column]]
        part2_names = colnames(data)[-seq_len(each_column)]
        data = sheet(
            setNames(data[, seq_len(each_column)[-each_column], drop = FALSE], part1_names), 
            new_columns, 
            setNames(data[, -seq_len(each_column), drop = FALSE], part2_names)
        )
    }
    recode(colnames(data)) = perl("^\\.\\.new_columns__\\d+_\\d+$") ~ ""
    if(NCOL(data)>0 && !(first_column_name %in% c(NA, "row_labels"))){
        colnames(data)[1] = first_column_name
    }
    class(data) = class_data %d% 'etable'
    data
}

#' @export
split_columns.matrix  = function(data, 
                                 columns = 1, 
                                 remove_repeated = TRUE, 
                                 split = "\\|", 
                                 fixed = FALSE, 
                                 perl = FALSE){
    
    matr_colnames = colnames(data)
    data = as.sheet(data)
    if(is.null(matr_colnames)){
        colnames(data) = rep("", NCOL(data))
    }
    split_columns(data, 
                  columns = columns, 
                  remove_repeated = remove_repeated, 
                  split = split, 
                  fixed = fixed, 
                  perl = perl
    )
}

#' @export
#' @rdname split_labels
split_table_to_df = function(data, 
                             digits = get_expss_digits(), 
                             remove_repeated = TRUE, 
                             split = "\\|", 
                             fixed = FALSE, 
                             perl = FALSE){
    UseMethod("split_table_to_df")
}

#' @export
split_table_to_df.intermediate_table = function(data, digits = get_expss_digits(), 
                                                remove_repeated = TRUE, 
                                                split = "\\|", 
                                                fixed = FALSE, 
                                                perl = FALSE){
    stop("No results for splitting. Use 'split_table_to_df' after 'tab_pivot'.")
}

#' @export
split_table_to_df.etable = function(data, 
                               digits = get_expss_digits(), 
                        remove_repeated = TRUE, 
                        split = "\\|", 
                        fixed = FALSE, 
                        perl = FALSE){
    if(NCOL(data) == 0) return(data)
    data = round_dataframe(data, digits = digits)
    cl_names = colnames(data)
    if(cl_names[1] == "row_labels") cl_names[1] = ""
    header = t(split_labels(cl_names, 
                            remove_repeated = remove_repeated,
                            split = split,
                            fixed = fixed,
                            perl = perl)
    )
    if(NROW(data)>0){
        digits = if_null(digits, 1)
        if(!is.na(digits)){
            for(i in seq_len(NCOL(data))[-1]){
                if(any(grepl(".", data[[i]], fixed = TRUE))){
                    data[[i]] = format(data[[i]], nsmall = digits, justify = "right")
                }
            }
        }
        recode(data) = NA ~ ""
        recode(data) = perl("^\\s*NA\\s*$") ~ ""
        cl_names = paste0("v", seq_len(NCOL(data)))
        colnames(header) = cl_names
        colnames(data) = cl_names
        data = rbind(header, data, stringsAsFactors = FALSE)
        old_ncol = NCOL(data)
        data = split_columns(data, 
                             columns = 1,
                             remove_repeated = remove_repeated,
                             split = split,
                             fixed = fixed,
                             perl = perl
        )
    } else {
        data = as.sheet(header)
    }
    rownames(data) = NULL
    data = setNames(data, rep("", NCOL(data)))
    class(data) = class(data) %d% 'etable'
    data
}

#' @export
#' @rdname split_labels
make_subheadings = function(data, number_of_columns = 1){
    UseMethod("make_subheadings")       
}

#' @export
make_subheadings.default = function(data, number_of_columns = 1){
    stopif(!isTRUE(number_of_columns>0), 
                   "Number of columns should be greater than zero but it equals to ",
           number_of_columns, ".")
    subheading_columns = data[, seq_len(number_of_columns), drop = FALSE]
    data[, seq_len(number_of_columns)] = NULL
    recode(subheading_columns) = c(NA ~ "", perl("^\\s*$") ~ "")
    last_nonempty_cell = ""
    for(col in rev(seq_len(NCOL(subheading_columns)))[-1]){
        for(row in seq_len(NROW(subheading_columns))){
            if(subheading_columns[row,col]!="") {
                last_nonempty_cell = subheading_columns[row,col]
            }
            if(subheading_columns[row,col]=="" & subheading_columns[row,col + 1]!=""){
                subheading_columns[row,col] = last_nonempty_cell   
            }
        }    
    }
    subheading_columns = do.call(paste, subheading_columns)
    has_value = !grepl("^\\s*$", subheading_columns, perl = TRUE)
    subheadings = subheading_columns[has_value]
    splitter = cumsum(has_value)
    subtables = split(data, splitter)
    add_subheader = function(x, y) {
        if(!(y[1,1] %in% c("", NA))){
            if(all(vapply(y, is.character, FUN.VALUE = NA))){
                y = add_rows("", y)
            } else {
                y = add_rows(NA, y)
            }
        }  
        y[1, 1] = x
        y
    }
    if(length(subheadings)<length(subtables)){
        data = mapply(add_subheader, subheadings, subtables[-1], 
                      SIMPLIFY = FALSE, 
                      USE.NAMES = FALSE) 
        data = do.call(add_rows, c(subtables[1], data))
    } else {
        data = mapply(add_subheader, subheadings, subtables, 
                      SIMPLIFY = FALSE, 
                      USE.NAMES = FALSE) 
        data = do.call(add_rows, data)
    }
    rownames(data) = NULL
    data
}

#' @export
make_subheadings.etable = function(data, number_of_columns = 1){
    res = split_columns(data)
    row_labels_width = NCOL(res) - NCOL(data) + 1
    if(row_labels_width==1) return(data)
    stopif(row_labels_width<number_of_columns, 
           "Too many columns for subheadings: ", number_of_columns, 
           ". Row labels occupy only ", row_labels_width, " columns.")
    res = make_subheadings(res, number_of_columns = number_of_columns)
    if(row_labels_width > number_of_columns){
        row_labels = res[, seq_len(row_labels_width - number_of_columns), drop = FALSE] # columns with row labels
        recode(row_labels) = NA ~ ""
        last_nonempty_cell = ""
        for(col in rev(seq_len(NCOL(row_labels)))[-1]){
            for(row in seq_len(NROW(row_labels))){
                if(row_labels[row,col]!="") {
                    last_nonempty_cell = row_labels[row,col]
                }
                if(row_labels[row,col]=="" & row_labels[row,col + 1]!=""){
                    row_labels[row,col] = last_nonempty_cell   
                }
            }    
        }
        res[, seq_len(row_labels_width - number_of_columns)] = NULL # columns with row labels
        row_labels = do.call(paste, c(as.list(row_labels), list(sep = "|")))
        row_labels = remove_unnecessary_splitters(row_labels)
        res = sheet(row_labels = row_labels, res)
    } 
    class(res) = class(data)
    res
}

split_all_in_etable_for_print = function(data, 
                               digits = get_expss_digits(), 
                               remove_repeated = TRUE, 
                               split = "\\|", 
                               fixed = FALSE, 
                               perl = FALSE){
    if(NCOL(data) == 0) return(data)
    
    # for tibbles and etc:
    data = as.data.frame(data, stringsAsFactors = FALSE, fix.empty.names = FALSE, check.names = FALSE)
    ##
    data_ncol = NCOL(data)
    data = round_dataframe(data, digits = digits)
    cl_names = colnames(data)
    if(cl_names[1] == "row_labels") cl_names[1] = ""
    header = t(split_labels(cl_names, 
                            remove_repeated = remove_repeated,
                            split = split,
                            fixed = fixed,
                            perl = perl)
    )
    if(NROW(data)>0){
        digits = if_null(digits, 1)
        if(!is.na(digits)){
            for(i in seq_len(NCOL(data))[-1]){
                if(any(grepl(".", data[[i]], fixed = TRUE))){
                    data[[i]] = format(data[[i]], nsmall = digits, justify = "right")
                }
            }
        }
        recode(data) = NA ~ ""
        recode(data) = perl("^\\s*NA\\s*$") ~ ""
        cl_names = paste0("v", seq_len(NCOL(data)))
        colnames(header) = cl_names
        colnames(data) = cl_names
        data = rbind(header, data, stringsAsFactors = FALSE)
        old_ncol = NCOL(data)
        data = split_columns(data, 
                             columns = 1,
                             remove_repeated = remove_repeated,
                             split = split,
                             fixed = fixed,
                             perl = perl
        )
        # if(remove_repeated && subheadings && (NCOL(data)>data_ncol)){
        #     subheading_column = data[[1]]
        #     data[[1]] = NULL
        # 
        #     has_value = !(subheading_column %in% c("", NA))
        #     subheadings = subheading_column[has_value]
        #     splitter = cumsum(has_value)
        #     subtables = split(data, splitter)
        #     add_subheader = function(x, y) {
        #         if(!(y[1,1] %in% c("", NA))){
        #             y = add_rows("", y)
        #             y = add_rows("", y)
        #         }
        #         y[2, 1] = x
        #         y
        #     }
        #     if(length(subheadings)<length(subtables)){
        #         data = mapply(add_subheader, subheadings, subtables[-1],
        #                       SIMPLIFY = FALSE,
        #                       USE.NAMES = FALSE)
        #         data = do.call(add_rows, c(subtables[1], data))
        #     } else {
        #         data = mapply(add_subheader, subheadings, subtables,
        #                       SIMPLIFY = FALSE,
        #                       USE.NAMES = FALSE)
        #         data = do.call(add_rows, data)
        #     }
        # 
        # }
    } else {
        data = as.sheet(header)
    }
    data[[1]] = paste0(" ", data[[1]], " ") # some extra space
    data[,-1] = lapply(data[,-1, drop = FALSE], function(x) paste0("", x, " ")) # some extra space
    width = sapply(as.sheet(lapply(data, nchar)), max, na.rm = TRUE) 
    width[!is.finite(width) | width<3 ] = 3
    delimiter = unlist(lapply(width[-1], function(x) {
        res = paste(rep("-", x  - 1), collapse = "")
        paste0(res," ")
    }))
    delimiter = c(paste0(" ", paste(rep("-", width[1]  - 2), collapse = ""), " "), delimiter)
    data = rbind(data[seq_len(NROW(header)), ,drop = FALSE], 
                 delimiter, 
                 data[-seq_len(NROW(header)), ,drop = FALSE],
                 stringsAsFactors = FALSE)
    
    
    for(i in seq_len(NCOL(data))){
        data[[i]] = format(data[[i]], width = width[i], justify =  "right")    
        data[[i]] = paste0(data[[i]], "|")    
    }

    data[[1]] = paste0("|", data[[1]])
    rownames(data) = NULL
    data = setNames(data, rep("", NCOL(data)))
    class(data) = class(data) %d% 'etable'
    data
}

# remove_middle_of_string = function(x, max_length){
#     need = (max_length - 5)/2
#     beg = substr(x, 1, need)
#     end = substr(x, nchar(x) - need, nchar(x))
#     paste0(beg, "<...>", end)
#     
# }
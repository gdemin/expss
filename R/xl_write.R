#' @export
xl_write = function(obj, wb, sheet, row = 1, col = 1, ...){
    if(require(openxlsx, quietly = TRUE, warn.conflicts = FALSE)){
        UseMethod("xl_write")
    } else {
        stop("xl_write: 'openxlsx' is required for this function. Please, install it with 'install.packages('openxlsx')'.")
    }
}

#' @export
xl_write.default = function(obj, wb, sheet, row = 1, col = 1, rownames = FALSE, colnames = TRUE, ...){
    if(!is.data.frame(obj)) {
        obj = as.sheet(obj)
    }
    openxlsx::writeData(wb = wb,
                        sheet = sheet,
                        x = obj,
                        startCol = col,
                        startRow = row,
                        colNames = colnames,
                        rowNames = rownames
                        )
    c(NROW(obj) + colnames, NCOL(obj) + rownames)
}



#' @export
xl_write.list = function(obj, wb, sheet, row = 1, col = 1, gap = 1, ...){
    col_shift = 0
    row_shift = 0
    for(each in obj){
        res = xl_write(each, wb = wb,
                       sheet = sheet,
                       row = row,
                       col = col,
                       ...)
        col_shift = max(col_shift, res[2])
        row_shift = row_shift + res[1] + gap
        row = row + res[1] + gap
    }

    c(row_shift - gap, col_shift)
}

#' @export
xl_write.etable = function(obj, 
                           wb, 
                           sheet, 
                           row = 1, 
                           col = 1, 
                           remove_repeated = c("all", "rows", "columns", "none"),
                           format_table = TRUE,
                           header_format = createStyle(
                               fgFill = "#EBEBEB",
                               halign = "left",
                               wrapText = FALSE
                           ),
                           main_format = createStyle(
                               halign = "right",
                               numFmt = format(0, nsmall = get_expss_digits())
                           ), 
                           row_labels_format = createStyle(
                               halign = "left"
                           ), 
                           total_format = createStyle(
                               fgFill = "#EBEBEB",
                               border = "TopBottom",
                               borderStyle = "thin",
                               halign = "right",
                               numFmt = "0"
                           ), 
                           total_row_labels_format = createStyle(
                               fgFill = "#EBEBEB",
                               border = "TopBottom",
                               borderStyle = "thin",
                               halign = "left"
                           ),
                           top_left_corner_format = header_format,
                           row_symbols_to_remove = NULL,
                           col_symbols_to_remove = NULL,
                           ...){
    # idea for future - custom formatting for selected rows/columns
    # other_rows_formats = NULL,
    # other_row_labels_formats = NULL,
    # other_cols_formats = NULL,
    # other_col_labels_formats = NULL,
    if(NCOL(obj)==0) return(invisible(c(NROW(obj), 0)))
    recode(obj) = is.nan ~ NA
    remove_repeated = match.arg(remove_repeated)

    header = t(split_labels(colnames(obj), remove_repeated = remove_repeated %in% c("all", "columns")))[,-1, drop = FALSE]
    row_labels = split_labels(obj[[1]], remove_repeated = remove_repeated %in% c("all", "rows"))
    if(NCOL(header)>0) {
        header = header[rowSums(!is.na(header) & (header != "")) > 
                        0, , drop = FALSE]
        for(pattern in col_symbols_to_remove){
            header[] = gsub(pattern, "", header, perl = TRUE)
        }
    }
    recode(header) = "" ~ NA

    for(pattern in row_symbols_to_remove){
        row_labels[] = gsub(pattern, "", row_labels, perl = TRUE)
    }
    
    # max(1, NCOL(row_labels)) for zero-rows table
    top_left_corner = matrix(NA, ncol = max(1, NCOL(row_labels)), nrow = NROW(header))
    if (!is.null(colnames(obj)) && !(colnames(obj)[1] %in% 
                                     c(NA, "row_labels", ""))) {
        top_left_corner[nrow(top_left_corner), 1] = colnames(obj)[1]
    }
    xy = c(col, row)
    writeData(wb, 
              sheet = sheet, 
              x = as.sheet(top_left_corner), 
              xy = xy, 
              keepNA = FALSE, 
              rowNames = FALSE, 
              colNames = FALSE)
    rng = c(xy[1] + NCOL(top_left_corner), xy[2] )
    writeData(wb, 
              sheet = sheet, 
              x = as.sheet(header), 
              xy = rng, 
              keepNA = FALSE, 
              rowNames = FALSE, 
              colNames = FALSE)
    rng = c(xy[1], xy[2] + NROW(top_left_corner))
    writeData(wb, 
              sheet = sheet, 
              x = as.sheet(row_labels), 
              xy = rng, 
              keepNA = FALSE, 
              rowNames = FALSE, 
              colNames = FALSE)
    rng = c(xy[1] + NCOL(top_left_corner), xy[2] + NROW(top_left_corner))
    writeData(wb, 
              sheet = sheet,
              x = obj[, -1, drop = FALSE],
              xy = rng,
              keepNA = FALSE,
              rowNames = FALSE,
              colNames = FALSE)
    if(format_table){
        table_structure = get_table_structure(obj)
        if(!is.null(header_format)){
            xl_format_header(wb, sheet, row, col,
                             table_structure,
                             header_format
            )
        }
        if(!is.null(top_left_corner_format)){
            xl_format_top_left_corner(wb, sheet, row, col,
                                      table_structure,
                                      top_left_corner_format
            )
        }
        if(!is.null(main_format)){
            xl_format_main_rows(wb, sheet, row, col,
                           table_structure,
                           table_structure$not_total_row_numbers,
                           main_format
            )
        }
        if(!is.null(row_labels_format)){
            xl_format_row_labels(wb, sheet, row, col,
                                 table_structure,
                                 table_structure$not_total_row_numbers,
                                 row_labels_format
            )
        }
        if(!is.null(total_format)){
            xl_format_main_rows(wb, sheet, row, col,
                                table_structure,
                                table_structure$total_row_numbers,
                                total_format
            )
        }
        if(!is.null(total_row_labels_format)){
            xl_format_row_labels(wb, sheet, row, col,
                                       table_structure,
                                       table_structure$total_row_numbers,
                                       total_row_labels_format
            )
        }
        xl_format_entire_table(wb, 
                               sheet, 
                               row, 
                               col,
                               table_structure,
                               "thin"
        )
        
    }
    invisible(c(NROW(obj) + NROW(top_left_corner), 
                NCOL(obj) + NCOL(top_left_corner) - 1)) # -1 because we don't need to count row_labels column 
}

#' @export
xl_write.with_caption = function(obj, 
                           wb, 
                           sheet, 
                           row = 1, 
                           col = 1, 
                           remove_repeated = c("all", "rows", "columns", "none"),
                           borders = TRUE,
                           header_format = NULL,
                           main_format = NULL, 
                           row_labels_format = NULL, 
                           total_format = NULL,
                           total_row_labels_format = NULL,
                           caption_format = NULL,
                           top_left_corner_format = header_format,
                           ...){
    xl_format_caption(str_range,
                      row_labels_width = table_structure$row_labels_width,
                      height = table_structure$header_height,
                      width = table_structure$header_width,
                      row_numbers = table_structure$caption_row_numbers,
                      progress_bar = progress_bar
    )
}


get_table_structure = function(tbl){
    header = t(split_labels(colnames(tbl), remove_repeated = TRUE))[,-1, drop = FALSE]
    table_structure = list(
        row_labels_width = max(1, NCOL(split_labels(tbl[[1]]))), # for zero-rows table
        header_height = NROW(header),
        header_width = NCOL(header)
    )
    table_structure = within(table_structure, {
        total_row_numbers = grep("#", tbl[[1]], fixed = TRUE)
        table_width = row_labels_width - 1 + NCOL(tbl)
        not_total_row_numbers = (1:NROW(tbl)) %d% total_row_numbers
        table_height = header_height + NROW(tbl)
        tbl_colnames = colnames(tbl)
        vertical_divisors = get_vertical_divisors(colnames(tbl))
    })    
    table_structure
}

get_vertical_divisors = function (tbl_colnames){
    if(length(tbl_colnames)<2) return(list())
    vapply(
        header_groups(tbl_colnames), 
        "[[", 
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE,
        1
    )  
}



xl_format_entire_table = function(wb, sheet, row, col, table_structure, borders){
    height = table_structure$table_height
    width = table_structure$table_width
    header_height = table_structure$header_height
    vertical_divisors = table_structure$vertical_divisors
    table_end_col = col + width - 1
    table_end_row = row + height - 1
    addStyle(wb = wb,
             sheet = sheet,
             rows = row, 
             cols = col:table_end_col, 
             createStyle(
                 borderStyle = borders,
                 border = "top"
             ), 
             gridExpand = TRUE,
             stack = TRUE)
    addStyle(wb = wb,
             sheet = sheet,
             rows = table_end_row, 
             cols = col:table_end_col, 
             createStyle(
                 borderStyle = borders,
                 border = "bottom"
             ), 
             gridExpand = TRUE,
             stack = TRUE)
    addStyle(wb = wb,
             sheet = sheet,
             rows = row:table_end_row, 
             cols = col, 
             createStyle(
                 borderStyle = borders,
                 border = "left"
             ), 
             gridExpand = TRUE,
             stack = TRUE)
    addStyle(wb = wb,
             sheet = sheet,
             rows = row:table_end_row, 
             cols = table_end_col, 
             createStyle(
                 borderStyle = borders,
                 border = "right"
             ), 
             gridExpand = TRUE,
             stack = TRUE)
    if((row + header_height)>=table_end_row) return(NULL)
    for(each in vertical_divisors){
        addStyle(wb = wb,
                 sheet = sheet,
                 rows = (row + header_height):table_end_row, 
                 cols = col + each + table_structure$row_labels_width - 1 - 1, 
                 createStyle(
                     borderStyle = borders,
                     border = "left"
                 ), 
                 gridExpand = TRUE,
                 stack = TRUE)
    }
    
}


xl_format_header = function(wb, sheet, row, col, table_structure, format){
    if(table_structure$table_width<=table_structure$row_labels_width) return(NULL)
    row_labels_width = table_structure$row_labels_width
    height = table_structure$header_height
    width = table_structure$header_width
    tbl_colnames = table_structure$tbl_colnames

    table_end_col = col + row_labels_width + width - 1
    header_end_row = row + height - 1
    header_start_col = col + row_labels_width
    # horizontal divizors
    hlines = lapply(strsplit(tbl_colnames[-1], split = "|", fixed = TRUE), function(each){
        each = trimws(each)
        # each!="" - if we have empty element then we don't draw line but move down
        res = seq_along(each)[c(each[-1]!="", FALSE)] 
        if(is.na(each[1]) || each[1]==""){
            # if first element is empty then we don't draw line after it
            res = res[-1]
        }
        res
        
    })
    for(i in seq_along(hlines)){
        if(length(hlines[i])>0){
            addStyle(wb = wb,
                     sheet = sheet,
                     rows = row + hlines[[i]] - 1, 
                     cols = header_start_col + i - 1, 
                     createStyle(
                         borderStyle = "thin",
                         border = "bottom"
                     ), 
                     gridExpand = TRUE,
                     stack = TRUE)
        }
    }
    if(width>1){
        if(header_end_row>row){
            for( i in header_end_row:(row + 1)){
                vertical_divisors = get_vertical_divisors(tbl_colnames)
                for(each in vertical_divisors){
                    addStyle(wb = wb,
                             sheet = sheet,
                             rows = i, 
                             cols = col + row_labels_width + each - 1 - 1,
                             style = createStyle(
                                 borderStyle = "thin",
                                 border = "left"                                 
                             ),
                             gridExpand = FALSE,
                             stack = TRUE)
                }
                tbl_colnames = gsub("\\|[^\\|]*$", "", tbl_colnames, perl = TRUE)
            }
        }
        for(each in seq_along(tbl_colnames)[-1]){
            if(tbl_colnames[each]!=tbl_colnames[each-1]){
                addStyle(wb = wb,
                         sheet = sheet, 
                         rows = row, 
                         cols = col + row_labels_width + each - 1 - 1,
                         style = createStyle(
                             borderStyle = "thin",
                             border = "left"                                 
                         ),
                         gridExpand = FALSE,
                         stack = TRUE)
            }
        }
    }
    addStyle(wb = wb,
             sheet = sheet,
             rows = header_end_row, 
             cols = header_start_col:table_end_col, 
             createStyle(
                 borderStyle = "thin",
                 border = "bottom"
             ), 
             gridExpand = TRUE,
             stack = TRUE)
    addStyle(wb = wb,
             sheet = sheet,
             rows = row:header_end_row, 
             cols = header_start_col:table_end_col, 
             format, 
             gridExpand = TRUE,
             stack = TRUE)
}


xl_format_top_left_corner = function(wb, sheet, row, col, table_structure, format){
    row_labels_width = table_structure$row_labels_width
    height = table_structure$header_height
    width = table_structure$header_width

    header_end_row = row + height - 1
    header_start_col = col + row_labels_width
    if(col<header_start_col){
    addStyle(wb = wb,
             sheet = sheet,
             rows = header_end_row, 
             cols = col:(header_start_col-1), 
             createStyle(
                 borderStyle = "thin",
                 border = "bottom"
             ), 
             gridExpand = TRUE,
             stack = TRUE)

    addStyle(wb = wb,
             sheet = sheet,
             rows = row:header_end_row, 
             cols = col:(header_start_col-1), 
             format, 
             gridExpand = TRUE,
             stack = TRUE)
    }
}

###########################
xl_format_row_labels = function(wb, 
                                sheet, 
                                row, 
                                col,
                                table_structure,
                                row_numbers,
                                format){
    start_col = col
    end_col = col + table_structure$row_labels_width - 1
    row_numbers = row_numbers + row + table_structure$header_height - 1
    addStyle(wb = wb,
             sheet = sheet,
             rows = row_numbers, 
             cols = start_col:end_col, 
             format, 
             gridExpand = TRUE,
             stack = TRUE)
}

##########################
xl_format_main_rows = function(wb, 
               sheet, 
               row, 
               col,
               table_structure,
               row_numbers,
               format){
    if(table_structure$table_width<=table_structure$row_labels_width) return(NULL)
    start_col = col + table_structure$row_labels_width
    end_col = col + table_structure$table_width - 1
    row_numbers = row_numbers + row + table_structure$header_height - 1
    addStyle(wb = wb,
             sheet = sheet,
             rows = row_numbers, 
             cols = start_col:end_col, 
             format, 
             gridExpand = TRUE,
             stack = TRUE)
}





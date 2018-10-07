#' @export
xl_write = function(obj, wb, sheet, row = 1, col = 1, ...){
    if(require(openxlsx, quietly = TRUE, warn.conflicts = FALSE)){
        UseMethod("xl_write")
    } else {
        stop("xl_write: 'openxlsx' is required for this function. Please, install it with 'install.packages('openxlsx')'.")
    }
}

#' @export
xl_write.default = function(obj, wb, sheet, row = 1, col = 1, ...){
    if(!is.data.frame(obj)) {
        obj = as.sheet(obj)
    }
    openxlsx::writeData(wb = wb,
                        sheet = sheet,
                        x = obj,
                        startCol = col,
                        startRow = row,
                        ...
                        )
    args = list(..)
    colnames = is.null(args[["colNames"]]) || args[["colNames"]]
    rownames = !is.null(args[["rowNames"]]) && args[["rowNames"]]
    c(NROW(obj) + colnames, NCOL(obj) + rownames)
}

#' @export
xl_write.etable = function(obj, 
                           wb, 
                           sheet, 
                           row = 1, 
                           col = 1, 
                           remove_repeated = c("all", "rows", "columns", "none"),
                           borders = TRUE,
                           header_format = NULL,
                           main_format = NULL, # get_expss_digits(),
                           row_labels_format = NULL, 
                           total_format = NULL,
                           total_row_labels_format = NULL,
                           top_left_corner_format = header_format,
                           ...){
    recode(obj) = is.nan ~ NA
    remove_repeated = match.arg(remove_repeated)
    header = t(split_labels(colnames(obj), remove_repeated = remove_repeated %in% c("all", "columns")))[, 
                                                                                                        -1, drop = FALSE]
    row_labels = split_labels(obj[[1]], remove_repeated = remove_repeated %in% c("all", "rows"))
    header = header[rowSums(!is.na(header) & (header != "")) > 
                        0, , drop = FALSE]
    recode(header) = "" ~ NA
    top_left_corner = matrix(NA, ncol = NCOL(row_labels), nrow = NROW(header))
    if (!is.null(colnames(obj)) && !(colnames(obj)[1] %in% 
                                     c(NA, "row_labels", ""))) {
        top_left_corner[nrow(top_left_corner), 1] = colnames(obj)[1]
    }
    obj = obj[, -1, drop = FALSE]
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
              x = as.sheet(obj),
              xy = rng,
              keepNA = FALSE,
              rowNames = FALSE,
              colNames = FALSE)
    xl_format_header(str_range,
                     row_labels_width = table_structure$row_labels_width,
                     height = table_structure$header_height,
                     width = table_structure$header_width,
                     tbl_colnames = table_structure$tbl_colnames
    )
    
    
    xl_format_numeric(str_range, 
                      row_labels_width = table_structure$row_labels_width,
                      height = table_structure$header_height,
                      width = table_structure$header_width,
                      row_numbers = table_structure$percent_row_numbers,
                      progress_bar = progress_bar
    )
    
    
    xl_format_total(str_range,
                    row_labels_width = table_structure$row_labels_width,
                    height = table_structure$header_height,
                    width = table_structure$header_width,
                    row_numbers = table_structure$total_row_numbers,
                    progress_bar = progress_bar
    )
    xl_format_entire_table(str_range,
                           height = table_structure$table_height,
                           width = table_structure$table_width,
                           header_height = table_structure$header_height,
                           vertical_divisors = table_structure$vertical_divisors
    )
    invisible(c(NROW(obj) + NROW(top_left_corner), 
                NCOL(obj) + NCOL(top_left_corner)))
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
                           main_format = NULL, # get_expss_digits(),
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
    header = t(split_labels(colnames(tbl)[-1], remove_repeated = TRUE))
    table_structure = list(
        total_row_numbers = grep("#", tbl[[1]], fixed = TRUE),
        row_labels_width = NCOL(split_labels(tbl[[1]])),
        header_height = NROW(header),
        header_width = NCOL(header)
    )
    table_structure = within(table_structure, {
        table_width = row_labels_width - 1 + NCOL(tbl)
        table_height = header_height + NROW(tbl)
        tbl_colnames = colnames(tbl)
        vertical_divisors = get_vertical_divisors(row_labels_width, colnames(tbl))
    })    
    table_structure
}

get_vertical_divisors = function (row_labels_width, tbl_colnames){
    vapply(
        header_groups(tbl_colnames), 
        "[[", 
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE,
        1
    ) + row_labels_width  
}



xl_format_entire_table = function(str_range,  height, width, header_height, vertical_divisors){
    rc = translate_character_range_to_rc(str_range)
    curr_row = rc[1]
    curr_col = rc[2]
    table_end_col = curr_col + width - 1
    table_end_row = curr_row + height - 1
    
    xl_apply_format(curr_row, curr_col:table_end_col, .XLSX_TOP_BORDER)
    xl_apply_format(table_end_row, curr_col:table_end_col, .XLSX_BOTTOM_BORDER)
    xl_apply_format(curr_row:table_end_row, curr_col, .XLSX_LEFT_BORDER)
    xl_apply_format(curr_row:table_end_row, table_end_col, .XLSX_RIGHT_BORDER)
    for(each in vertical_divisors){
        xl_apply_format((curr_row + header_height):table_end_row, each + curr_col - 1, .XLSX_LEFT_BORDER, gridExpand = FALSE)
    }
    
    setColWidths(.xl_activeworkbook, .xl_activesheet, cols = curr_col, 40)
}


xl_format_header = function(str_range, row_labels_width, height, width, tbl_colnames){
    rc = translate_character_range_to_rc(str_range)
    curr_row = rc[1]
    curr_col = rc[2]
    
    table_end_col = curr_col + row_labels_width + width - 1
    header_end_row = curr_row + height - 1
    header_start_col = curr_col + row_labels_width
    # horizontal divizors
    hlines = strsplit(tbl_colnames[-1], split = "|", fixed = TRUE) %>% lapply(function(each){
        each = trimws(each)
        # each!="" - if we have empty element then we don't draw line but move down
        res = seq_along(each)[c(each[-1]!="", FALSE)] 
        if(each[1]==""){
            # if first element is empty then we don't draw line after it
            res = res[-1]
        }
        res
        
    })
    for(i in seq_along(hlines)){
        if(length(hlines[i])>0){
            xl_apply_format(curr_row + hlines[[i]] - 1, header_start_col + i - 1, .XLSX_BOTTOM_BORDER, gridExpand = TRUE)
        }
    }
    if(width>1){
        if(header_end_row>curr_row){
            for( i in header_end_row:(curr_row + 1)){
                vertical_divisors = get_vertical_divisors(row_labels_width, tbl_colnames)
                for(each in vertical_divisors){
                    
                    xl_apply_format(i, each + curr_col - 1, .XLSX_LEFT_BORDER, gridExpand = FALSE)
                }
                tbl_colnames = gsub("\\|[^\\|]*$", "", tbl_colnames, perl = TRUE)
            }
        }
        for(i in seq_along(tbl_colnames)[-1]){
            if(tbl_colnames[i]!=tbl_colnames[i-1]){
                xl_apply_format(curr_row, curr_col + row_labels_width + i - 1 - 1, .XLSX_LEFT_BORDER, gridExpand = FALSE)
            }
        }
    }
    xl_apply_format(header_end_row, curr_col:table_end_col, .XLSX_BOTTOM_BORDER, gridExpand = TRUE)
    xl_apply_format(curr_row:header_end_row, curr_col:table_end_col, .XLSX_BANNER_FORMAT, gridExpand = TRUE)
    
}


xl_format_total = function(str_range, row_labels_width, height, width, row_numbers, progress_bar = TRUE){
    if(length(row_numbers)<1) return(NULL)

    curr_row = rc[1]
    curr_col = rc[2]
    start_col = curr_col
    end_col = curr_col + row_labels_width + width - 1
    row_numbers = row_numbers + height + curr_row - 1
    xl_apply_format(rows = row_numbers, columns = start_col:end_col, .XLSX_TOTAL_FORMAT, gridExpand = TRUE)
    xl_apply_format(rows = row_numbers, columns = start_col:(row_labels_width + curr_col - 1), c(.XLSX_TOTAL_FORMAT, 
                                                                                                 list(halign = "left", valign = "center")
    ),
    gridExpand = TRUE)
}




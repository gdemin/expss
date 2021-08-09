#' Write tables and other objects to an xlsx file with formatting
#' 
#' Note that \code{openxlsx} package is required for these functions. It can be
#' install by printing \code{install.packages('openxlsx')} in the console. On
#' Windows system you also may need to
#' install \href{https://cran.r-project.org/bin/windows/Rtools/}{rtools}. You
#' can export several tables at once by combining them in a list. See examples.
#' If you need to write all tables to the single sheet you can use
#' \code{xl_write_file}. It automatically creates workbook, worksheet and save
#' *.xlsx file for you.
#' @param obj \code{table} - result of \link{cro}, \link{fre} and etc.
#'   \code{obj} also can be data.frame, list or other objects.
#' @param wb xlsx workbook object, result of \link[openxlsx]{createWorkbook} function.
#' @param sheet character or numeric - worksheet name/number in the workbook \code{wb}
#' @param filename A character string naming an xlsx file. For \code{xl_write_file}. 
#' @param sheetname A character name for the worksheet. For \code{xl_write_file}.
#' @param row numeric - starting row for writing data
#' @param col numeric - starting column for writing data
#' @param rownames logical should we write data.frame row names? 
#' @param colnames logical should we write data.frame column names?
#' @param remove_repeated Should we remove duplicated row or column labels in
#'   the rows/columns of the etable? Possible values: "all", "rows", "columns", "none".
#' @param format_table logical should we format table? If FALSE all format arguments will be ignored.
#' @param borders list Style of the table borders. List with two named elements:
#'   \code{borderColour} and \code{borderStyle}. For details see
#'   \link[openxlsx]{createStyle} function. If it is NULL then no table borders will
#'   be produced.
#' @param header_format table header format - result of the \link[openxlsx]{createStyle} function.
#' @param main_format result of the \link[openxlsx]{createStyle} function.
#'   Format of the table main area except total rows. Total rows is rows which
#'   row labels contain '#'.
#' @param row_labels_format result of the \link[openxlsx]{createStyle} function.
#'   Format of the row labels area except total rows. Total rows is rows which
#'   row labels contain '#'.
#' @param total_format result of the \link[openxlsx]{createStyle} function.
#'   Format of the total rows in the table main area. Total rows is rows which
#'   row labels contain '#'. 
#' @param total_row_labels_format result of the \link[openxlsx]{createStyle} function.
#'   Format of the total rows in the row labels area. Total rows is rows which
#'   row labels contain '#'.  
#' @param top_left_corner_format result of the \link[openxlsx]{createStyle} function. 
#' @param row_symbols_to_remove character vector. Perl-style regular expressions
#'   for substrings which will be removed from row labels.
#' @param col_symbols_to_remove character vector. Perl-style regular expressions
#'   for substrings  which will be removed from column names.
#' @param other_rows_formats named list. Names of the list are perl-style
#'   regular expression patterns, items of the list are results of the
#'   \link[openxlsx]{createStyle} function. Rows in the main area which row
#'   labels contain pattern will be formatted according to the appropriate style.
#' @param other_row_labels_formats named list. Names of the list are perl-style
#'   regular expression patterns, items of the list are results of the
#'   \link[openxlsx]{createStyle} function. Rows in the row labels area which row
#'   labels contain pattern will be formatted according to the appropriate style.
#' @param other_cols_formats named list. Names of the list are perl-style
#'   regular expression patterns, items of the list are results of the
#'   \link[openxlsx]{createStyle} function. Columns in the main area which column
#'   labels contain pattern will be formatted according to the appropriate style.
#' @param other_col_labels_formats named list. Names of the list are perl-style
#'   regular expression patterns, items of the list are results of the
#'   \link[openxlsx]{createStyle} function. Columns in the header area which column
#'   labels contain pattern will be formatted according to the appropriate style. 
#' @param additional_cells_formats list Each item of the list is list which
#'   consists of two elements. First element is two columns matrix or data.frame
#'   with row number and column numbers in the main area of the table. Such
#'   matrix can be produced with code \code{which(logical_condition, arr.ind =
#'   TRUE)}. Instead of matrix one can use function which accepts original table
#'   (\code{obj}) and return such matrix. Second element is result of the
#'   \link[openxlsx]{createStyle} function. Cells in the main area will be
#'   formatted according to this style.
#' @param caption_format result of the \link[openxlsx]{createStyle} function.
#' @param gap integer. Number of rows between list elements.
#' @param ... further arguments for \code{xl_write}
#' @return invisibly return vector with rows and columns (\code{c(rows,
#'   columns)}) occupied by outputted object.
#' 
#' @examples 
#' \dontrun{
#' library(openxlsx)
#' data(mtcars)
#' # add labels to dataset
#' mtcars = apply_labels(mtcars,
#'                       mpg = "Miles/(US) gallon",
#'                       cyl = "Number of cylinders",
#'                       disp = "Displacement (cu.in.)",
#'                       hp = "Gross horsepower",
#'                       drat = "Rear axle ratio",
#'                       wt = "Weight (lb/1000)",
#'                       qsec = "1/4 mile time",
#'                       vs = "Engine",
#'                       vs = c("V-engine" = 0,
#'                              "Straight engine" = 1),
#'                       am = "Transmission",
#'                       am = c("Automatic" = 0,
#'                              "Manual"=1),
#'                       gear = "Number of forward gears",
#'                       carb = "Number of carburetors"
#' )
#' 
#' # create table with caption
#' mtcars_table = cross_cpct(mtcars,
#'                              cell_vars = list(cyl, gear),
#'                              col_vars = list(total(), am, vs)
#' ) %>% 
#'     set_caption("Table 1")
#' 
#' 
#' wb = createWorkbook()
#' sh = addWorksheet(wb, "Tables")
#' # export table
#' xl_write(mtcars_table, wb, sh)
#' saveWorkbook(wb, "table1.xlsx", overwrite = TRUE)
#' 
#' ## quick export
#' xl_write_file(mtcars_table, "table1.xlsx")
#' 
#' ## custom cells formatting
#' wb = createWorkbook()
#' sh = addWorksheet(wb, "Tables")
#' 
#' # we want to mark cells which are greater than total column
#' my_formatter = function(tbl){
#'     greater_than_total = tbl[,-1]>tbl[[2]]
#'     which(greater_than_total, arr.ind = TRUE)
#' }
#' # export table
#' xl_write(mtcars_table, wb, sh, 
#'     additional_cells_formats = list(
#'         list(my_formatter, createStyle(textDecoration =  "bold", fontColour = "blue"))
#'     )
#' )
#' saveWorkbook(wb, "table_with_additional_format.xlsx", overwrite = TRUE)
#' 
#' ## automated report generation on multiple variables with the same banner
#'  
#' banner = with(mtcars, list(total(), am, vs))
#' 
#' # create list of tables
#' list_of_tables = lapply(mtcars, function(variable) {
#'     if(length(unique(variable))<7){
#'         cro_cpct(variable, banner) %>% significance_cpct()
#'     } else {
#'         # if number of unique values greater than seven we calculate mean
#'         cro_mean_sd_n(variable, banner) %>% significance_means()
#'         
#'     }
#'     
#' })
#' 
#' 
#' wb = createWorkbook()
#' sh = addWorksheet(wb, "Tables")
#' # export list of tables with additional formatting
#' xl_write(list_of_tables, wb, sh, 
#'          # remove '#' sign from totals 
#'          col_symbols_to_remove = "#",
#'          row_symbols_to_remove = "#",
#'          # format total column as bold
#'          other_col_labels_formats = list("#" = createStyle(textDecoration = "bold")),
#'          other_cols_formats = list("#" = createStyle(textDecoration = "bold")),
#' )
#' saveWorkbook(wb, "report.xlsx", overwrite = TRUE)
#' }
#' @export
xl_write = function(obj, wb, sheet, row = 1, col = 1, ...){
    if(!requireNamespace("openxlsx", quietly = TRUE)){
        stop("xl_write: 'openxlsx' is required for this function. Please, install it with 'install.packages('openxlsx')'.")
    }    
    UseMethod("xl_write")

}

#' @export
#' @rdname xl_write
xl_write_file = function(obj, filename, sheetname = "Tables", ...){
    wb = openxlsx::createWorkbook()
    sh = openxlsx::addWorksheet(wb, sheetName = sheetname)
    xl_write(obj, wb = wb, sheet = sh, ...)
    openxlsx::saveWorkbook(wb, file = filename, overwrite = TRUE)
}

#' @export
#' @rdname xl_write
xl_write.default = function(obj, wb, sheet, row = 1, col = 1, rownames = FALSE, colnames = !is.atomic(obj), ...){
    force(colnames)
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
    invisible(c(NROW(obj) + colnames, NCOL(obj) + rownames))
}



#' @export
#' @rdname xl_write
xl_write.list = function(obj, wb, sheet, row = 1, col = 1, gap = 1, ...){
    stopifnot(
        is.numeric(gap),
        length(gap)==1,
        !is.na(gap),
        gap>0
    )
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

    invisible(c(row_shift - gap, col_shift))
}

#' @export
#' @rdname xl_write
xl_write.etable = function(obj, 
                           wb, 
                           sheet, 
                           row = 1, 
                           col = 1, 
                           remove_repeated = c("all", "rows", "columns", "none"),
                           format_table = TRUE,
                           borders = list(borderColour = "black", borderStyle = "thin"),
                           header_format = openxlsx::createStyle(
                               fgFill = "#EBEBEB",
                               halign = "left",
                               wrapText = FALSE
                           ),
                           main_format = openxlsx::createStyle(
                               halign = "right",
                               numFmt = format(0, nsmall = get_expss_digits())
                           ), 
                           row_labels_format = openxlsx::createStyle(
                               halign = "left"
                           ), 
                           total_format = openxlsx::createStyle(
                               fgFill = "#EBEBEB",
                               border = "TopBottom",
                               borderStyle = "thin",
                               halign = "right",
                               numFmt = "0"
                           ), 
                           total_row_labels_format = openxlsx::createStyle(
                               fgFill = "#EBEBEB",
                               border = "TopBottom",
                               borderStyle = "thin",
                               halign = "left"
                           ),
                           top_left_corner_format = header_format,
                           row_symbols_to_remove = NULL,
                           col_symbols_to_remove = NULL,
                           other_rows_formats = NULL,
                           other_row_labels_formats = NULL,
                           other_cols_formats = NULL,
                           other_col_labels_formats = NULL,
                           additional_cells_formats = NULL,
                           ...){
    if(NCOL(obj)==0) return(invisible(c(NROW(obj), 0)))
    recode(obj) = is.nan ~ NA
    if(getRversion()>="3.5.0"){
        obj = type.convert(obj, as.is = TRUE)
    } else {
        for(i in seq_along(obj)){
            if(is.character(obj[[i]])) obj[[i]] = type.convert(obj[[i]], as.is = TRUE)
        }
    }
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
    openxlsx::writeData(wb, 
              sheet = sheet, 
              x = as.sheet(top_left_corner), 
              xy = xy, 
              keepNA = FALSE, 
              rowNames = FALSE, 
              colNames = FALSE)
    rng = c(xy[1] + NCOL(top_left_corner), xy[2] )
    openxlsx::writeData(wb, 
              sheet = sheet, 
              x = as.sheet(header), 
              xy = rng, 
              keepNA = FALSE, 
              rowNames = FALSE, 
              colNames = FALSE)
    rng = c(xy[1], xy[2] + NROW(top_left_corner))
    openxlsx::writeData(wb, 
              sheet = sheet, 
              x = as.sheet(row_labels), 
              xy = rng, 
              keepNA = FALSE, 
              rowNames = FALSE, 
              colNames = FALSE)
    rng = c(xy[1] + NCOL(top_left_corner), xy[2] + NROW(top_left_corner))
    openxlsx::writeData(wb, 
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
                             header_format,
                             borders = borders
            )
        }
        if(!is.null(top_left_corner_format)){
            xl_format_top_left_corner(wb, sheet, row, col,
                                      table_structure,
                                      top_left_corner_format,
                                      borders = borders
            )
        }
        ### rows
        main_format = c("^[^#]*$" = main_format, "#" = total_format, other_rows_formats)
        for(pattern in names(main_format)){
            xl_format_main_rows(wb, sheet, row, col,
                                table_structure,
                                grep(pattern, obj[[1]], perl = TRUE),
                                main_format[[pattern]]
            )
        }
        
        row_labels_format = c("^[^#]*$" = row_labels_format, "#" = total_row_labels_format, other_row_labels_formats)
        for(pattern in names(row_labels_format)){
            xl_format_row_labels(wb, sheet, row, col,
                                table_structure,
                                grep(pattern, obj[[1]], perl = TRUE),
                                row_labels_format[[pattern]]
            )
        }
        ### columns
        
        for(pattern in names(other_cols_formats)){
            xl_format_main_cols(wb, sheet, row, col,
                                table_structure,
                                col_numbers = grep(pattern, colnames(obj)[-1], perl = TRUE),
                                other_cols_formats[[pattern]]
            )
        }
        
        for(pattern in names(other_col_labels_formats)){
            xl_format_col_labels(wb, sheet, row, col,
                                 table_structure,
                                 col_numbers = grep(pattern, colnames(obj)[-1], perl = TRUE),
                                 other_col_labels_formats[[pattern]]
            )
        }
        
        ### cells
        for(each in additional_cells_formats){
            coord_matrix = each[[1]]
            if(is.function(coord_matrix)){
                coord_matrix = coord_matrix(obj)
            }
            if(is.data.frame(coord_matrix)){
                coord_matrix = as.matrix(coord_matrix)
            }
            
            xl_format_cells(wb, sheet, row, col,
                            table_structure,
                            row_numbers = coord_matrix[,1],
                            col_numbers = coord_matrix[,2],
                            format = each[[2]]
            )
        }
        xl_format_entire_table(wb, 
                               sheet, 
                               row, 
                               col,
                               table_structure,
                               borders = borders
        )
        
    }
    invisible(c(NROW(obj) + NROW(top_left_corner), 
                NCOL(obj) + NCOL(top_left_corner) - 1)) # -1 because we don't need to count row_labels column  
}


#' @export
#' @rdname xl_write
xl_write.with_caption = function(obj, 
                                 wb, 
                                 sheet, 
                                 row = 1, 
                                 col = 1, 
                                 remove_repeated = c("all", "rows", "columns", "none"),
                                 format_table = TRUE,
                                 borders = list(borderColour = "black", borderStyle = "thin"),
                                 header_format = openxlsx::createStyle(
                                     fgFill = "#EBEBEB",
                                     halign = "left",
                                     wrapText = FALSE
                                 ),
                                 main_format = openxlsx::createStyle(
                                     halign = "right",
                                     numFmt = format(0, nsmall = get_expss_digits())
                                 ), 
                                 row_labels_format = openxlsx::createStyle(
                                     halign = "left"
                                 ), 
                                 total_format = openxlsx::createStyle(
                                     fgFill = "#EBEBEB",
                                     border = "TopBottom",
                                     borderStyle = "thin",
                                     halign = "right",
                                     numFmt = "0"
                                 ), 
                                 total_row_labels_format = openxlsx::createStyle(
                                     fgFill = "#EBEBEB",
                                     border = "TopBottom",
                                     borderStyle = "thin",
                                     halign = "left"
                                 ),
                                 top_left_corner_format = header_format,
                                 row_symbols_to_remove = NULL,
                                 col_symbols_to_remove = NULL,
                                 other_rows_formats = NULL,
                                 other_row_labels_formats = NULL,
                                 other_cols_formats = NULL,
                                 other_col_labels_formats = NULL,
                                 additional_cells_formats = NULL,
                                 caption_format = openxlsx::createStyle(
                                     textDecoration = "bold",
                                     halign = "left"
                                 ),
                                 ...){
    caption_res = xl_write(get_caption(obj), 
                   wb = wb,
                   sheet = sheet,
                   row = row,
                   col = col
                   )
    if(!is.null(caption_format)){
        openxlsx::addStyle(
            wb = wb,
            sheet = sheet,
            style = caption_format,
            rows = row:(row + caption_res[1] - 1),
            cols = col:(col + caption_res[2] - 1),
            gridExpand = TRUE,
            stack = TRUE
        )
    }
    table_row = row + caption_res[1]
    obj = set_caption(obj, NULL)
    table_res = xl_write(obj, 
                         wb = wb, 
                         sheet = sheet, 
                         row = table_row, 
                         col = col, 
                         remove_repeated = remove_repeated,
                         format_table = format_table,
                         borders = borders,
                         header_format = header_format,
                         main_format = main_format, 
                         row_labels_format = row_labels_format, 
                         total_format = total_format, 
                         total_row_labels_format = total_row_labels_format,
                         top_left_corner_format = top_left_corner_format,
                         row_symbols_to_remove = row_symbols_to_remove,
                         col_symbols_to_remove = col_symbols_to_remove,
                         other_rows_formats = other_rows_formats,
                         other_row_labels_formats = other_row_labels_formats,
                         other_cols_formats = other_cols_formats,
                         other_col_labels_formats = other_col_labels_formats,
                         additional_cells_formats = additional_cells_formats
    )
    invisible(c(
        caption_res[1] + table_res[1], 
        max(caption_res[2], table_res[2], na.rm = TRUE)
    ))
    
}

###########################
get_table_structure = function(tbl){
    # to pass CRAN check
    row_labels_width = NULL
    header_height = NULL
    header_width = NULL
    #####
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
    if(is.null(borders)){
        borders = list(borderStyle = "none", borderColour = "black")
    }
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = row, 
             cols = col:table_end_col, 
             openxlsx::createStyle(
                 borderStyle = borders$borderStyle,
                 borderColour = borders$borderColour,
                 border = "top"
             ), 
             gridExpand = TRUE,
             stack = TRUE)
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = table_end_row, 
             cols = col:table_end_col, 
             openxlsx::createStyle(
                 borderStyle = borders$borderStyle,
                 borderColour = borders$borderColour,
                 border = "bottom"
             ), 
             gridExpand = TRUE,
             stack = TRUE)
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = row:table_end_row, 
             cols = col, 
             openxlsx::createStyle(
                 borderStyle = borders$borderStyle,
                 borderColour = borders$borderColour,
                 border = "left"
             ), 
             gridExpand = TRUE,
             stack = TRUE)
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = row:table_end_row, 
             cols = table_end_col, 
             openxlsx::createStyle(
                 borderStyle = borders$borderStyle,
                 borderColour = borders$borderColour,
                 border = "right"
             ), 
             gridExpand = TRUE,
             stack = TRUE)
    if((row + header_height)>=table_end_row) return(NULL)
    for(each in vertical_divisors){
        openxlsx::addStyle(wb = wb,
                 sheet = sheet,
                 rows = (row + header_height):table_end_row, 
                 cols = col + each + table_structure$row_labels_width - 1 - 1, 
                 openxlsx::createStyle(
                     borderStyle = borders$borderStyle,
                     borderColour = borders$borderColour,
                     border = "left"
                 ), 
                 gridExpand = TRUE,
                 stack = TRUE)
    }
    
}


xl_format_header = function(wb, sheet, row, col, table_structure, format, borders){
    if(table_structure$table_width<=table_structure$row_labels_width) return(NULL)
    row_labels_width = table_structure$row_labels_width
    height = table_structure$header_height
    width = table_structure$header_width
    tbl_colnames = table_structure$tbl_colnames
    table_end_col = col + row_labels_width + width - 1
    header_end_row = row + height - 1
    header_start_col = col + row_labels_width
    if(is.null(borders)){
        borders = list(borderStyle = "none", borderColour = "black")
    }
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
            openxlsx::addStyle(wb = wb,
                     sheet = sheet,
                     rows = row + hlines[[i]] - 1, 
                     cols = header_start_col + i - 1, 
                     openxlsx::createStyle(
                         borderStyle = borders$borderStyle,
                         borderColour = borders$borderColour,
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
                    openxlsx::addStyle(wb = wb,
                             sheet = sheet,
                             rows = i, 
                             cols = col + row_labels_width + each - 1 - 1,
                             openxlsx::createStyle(
                                 borderStyle = borders$borderStyle,
                                 borderColour = borders$borderColour,
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
                openxlsx::addStyle(wb = wb,
                         sheet = sheet, 
                         rows = row, 
                         cols = col + row_labels_width + each - 1 - 1,
                         openxlsx::createStyle(
                             borderStyle = borders$borderStyle,
                             borderColour = borders$borderColour,
                             border = "left"
                         ), 
                         gridExpand = FALSE,
                         stack = TRUE)
            }
        }
    }
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = header_end_row, 
             cols = header_start_col:table_end_col, 
             openxlsx::createStyle(
                 borderStyle = borders$borderStyle,
                 borderColour = borders$borderColour,
                 border = "bottom"
             ), 
             gridExpand = TRUE,
             stack = TRUE)
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = row:header_end_row, 
             cols = header_start_col:table_end_col, 
             format, 
             gridExpand = TRUE,
             stack = TRUE)
}


xl_format_top_left_corner = function(wb, sheet, row, col, table_structure, format, borders){
    row_labels_width = table_structure$row_labels_width
    height = table_structure$header_height
    width = table_structure$header_width
    header_end_row = row + height - 1
    header_start_col = col + row_labels_width
    if(is.null(borders)){
        borders = list(borderStyle = "none", borderColour = "black")
    }
    if(col<header_start_col){
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = header_end_row, 
             cols = col:(header_start_col-1), 
             openxlsx::createStyle(
                 borderStyle = borders$borderStyle,
                 borderColour = borders$borderColour,
                 border = "bottom"
             ), 
             gridExpand = TRUE,
             stack = TRUE)

    openxlsx::addStyle(wb = wb,
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
    end_col = col + table_structure$row_labels_width - 1
    row_numbers = row_numbers + row + table_structure$header_height - 1
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = row_numbers, 
             cols = col:end_col, 
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
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = row_numbers, 
             cols = start_col:end_col, 
             format, 
             gridExpand = TRUE,
             stack = TRUE)
}

###########################
xl_format_col_labels = function(wb, 
                                sheet, 
                                row, 
                                col,
                                table_structure,
                                col_numbers,
                                format){
    end_row = col + table_structure$header_height - 1
    col_numbers = col + col_numbers + table_structure$row_labels_width - 1
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = row:end_row, 
             cols = col_numbers, 
             format, 
             gridExpand = TRUE,
             stack = TRUE)
}


##########################
xl_format_main_cols = function(wb, 
                               sheet, 
                               row, 
                               col,
                               table_structure,
                               col_numbers,
                               format){
    if(table_structure$table_height<=table_structure$header_height) return(NULL)
    start_row = row + table_structure$header_height
    end_row = row + table_structure$table_height - 1
    col_numbers = col + col_numbers + table_structure$row_labels_width - 1
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = start_row:end_row, 
             cols = col_numbers, 
             format, 
             gridExpand = TRUE,
             stack = TRUE)
}

########
xl_format_cells = function(wb, 
                           sheet, 
                           row, 
                           col,
                           table_structure,
                           row_numbers,
                           col_numbers,
                           format
){
    col_numbers = col_numbers + col + table_structure$row_labels_width - 1
    row_numbers = row_numbers + row + table_structure$header_height - 1
    openxlsx::addStyle(wb = wb,
             sheet = sheet,
             rows = row_numbers, 
             cols = col_numbers, 
             format, 
             gridExpand = FALSE,
             stack = TRUE)
}


#' Create an HTML table widget for usage with Shiny
#' 
#' This is method for rendering results of \link{tables}/\link{fre}/\link{cro}
#' in Shiny. \code{DT} package should be installed for this
#' feature (\code{install.packages('DT')}). For detailed description of function
#' and its arguments see \link[DT]{datatable}.
#'
#' @param data a data object (result of \link{tables}/\link{fre}/\link{cro}).
#' @param repeat_row_labels logical Should we repeat duplicated row labels in
#'   the every row? Default is FALSE.
#' @param show_row_numbers logical Default is FALSE.
#' @param digits integer By default, all numeric columns are rounded to one digit after
#'   decimal separator. Also you can set this argument by option 'expss.digits'
#'   - for example, \code{expss_digits(2)}. If it is NA than all
#'   numeric columns remain unrounded.
#' @param ... further parameters for \link[DT]{datatable}
#'
#' @return Object of class \link[DT]{datatable}
#' @seealso \link[htmlTable]{htmlTable} for knitting
#' @export
#'
#' @examples
#' \dontrun{ 
#' 
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       mpg = "Miles/(US) gallon",
#'                       cyl = "Number of cylinders",
#'                       disp = "Displacement (cu.in.)",
#'                       hp = "Gross horsepower",
#'                       drat = "Rear axle ratio",
#'                       wt = "Weight (1000 lbs)",
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
#' mtcars_table = mtcars %>% 
#'                  tab_cols(total(), am %nest% vs) %>% 
#'                  tab_cells(mpg, hp) %>% 
#'                  tab_stat_mean() %>% 
#'                  tab_cells(cyl) %>% 
#'                  tab_stat_cpct() %>% 
#'                  tab_pivot() %>% 
#'                  set_caption("Table 1. Some variables from mtcars dataset.")
#'
#' library(shiny)
#' shinyApp(
#'     ui = fluidPage(fluidRow(column(12, DT::dataTableOutput('tbl')))),
#'     server = function(input, output) {
#'         output$tbl = DT::renderDataTable(
#'             as.datatable_widget(mtcars_table)
#'         )
#'     }
#' )
#' }
as.datatable_widget = function(data, ...){
    stopif(!requireNamespace("DT", quietly = TRUE) || !requireNamespace("htmltools", quietly = TRUE), 
           "DT and htmltools packages are required for 'as.datatable_widget' function. Please, install it with 'install.packages(\"DT\")'"
           )
    UseMethod("as.datatable_widget")
}  


#' @export
as.datatable_widget.default = function(data, ...){
    DT::datatable(data, ...)
}


#' @export
#' @rdname as.datatable_widget
as.datatable_widget.etable = function(data, 
                                      ...,
                            repeat_row_labels = FALSE, 
                            show_row_numbers = FALSE,
                            digits = get_expss_digits()
                            ){
    data = round_dataframe(data, digits = digits)
    if(NCOL(data)>0){
        first_lab = colnames(data)[1]
        row_labels = data[[1]]
        data[[1]] = NULL # remove first column. This method is needed to prevent column names damaging
        header = t(split_labels(colnames(data), split = "|", fixed = TRUE, remove_repeated = FALSE))
        row_labels = split_labels(row_labels, split = "|", fixed = TRUE, remove_repeated = !repeat_row_labels)
        if(length(row_labels)){
            row_labels = sheet(row_labels)    
        } else {
            row_labels = sheet(matrix("", nrow = nrow(data), ncol = 1))
        }
        
        if(show_row_numbers) {
            row_labels = sheet(seq_len(nrow(row_labels)), row_labels)    
        }
        colnames(row_labels) = rep("", ncol(row_labels))
        if(nrow(header)>0){
            empty_corner = matrix("", nrow = nrow(header) , ncol = ncol(row_labels))
        } else {
            empty_corner = matrix("", nrow = 1, ncol = ncol(row_labels))
        }
        if(is.na(first_lab) || first_lab=="row_labels") first_lab = ""
        empty_corner[1, 1] = first_lab    
        header = matrix_header_to_html(empty_corner, header)
        data = cbind(row_labels, data)
    } else {
        if(show_row_numbers) {
            row_labels = sheet(seq_len(nrow(data))) 
            
        } else {
            row_labels = as.sheet(matrix(NA, nrow = nrow(data), ncol = 0))
        }
        data = cbind(row_labels, data)
        header = '<table class="display"><thead><tr><th> </th></thead></table>'
        empty_corner = NULL
    }
    args = list(...)
    args[["class"]] = if_null(args[["class"]], 'stripe hover cell-border row-border order-column compact')
    args[["filter"]] = if_null(args[["filter"]], "none")
    args[["options"]] = if_null(args[["options"]], 
                        list(paging = FALSE,
                             searching = FALSE, 
                             sorting = FALSE, 
                             ordering = FALSE,
                             bFilter = FALSE, 
                             bInfo = FALSE,
                             columnDefs = list(
                                 list(
                                     className = 'dt-head-left', # cell-border
                                     targets = 0:(ncol(data)-1)
                                 )
                             )
                        )
    )
    args[["rownames"]] = if_null(args[["rownames"]], FALSE)
    args[["container"]] = header
    args[["data"]] = data
    res = do.call(DT::datatable, args)
    # if(!is.null(format_bold_value)){
    #     columns = seq_len(ncol(empty_corner))-1
    #     if(length(columns)){
    #         res = DT::formatStyle(res, 
    #                           columns = columns, 
    #                           target = "row", 
    #                           fontWeight = DT::styleEqual("#Total", "bold")
    #                           )        
    #     }
    # }    
    if(NCOL(data)>0) {
        DT::formatStyle(res, seq_len(NCOL(data))[-seq_len(NCOL(empty_corner))], textAlign = 'right')
    } else {
        res
    }
    
}



matrix_header_to_html = function(corner, m_cols){
    # to pass CRAN check
    thead = NULL
    tr = NULL
    th = NULL
    # tags = NULL
    # HTML = NULL
    row_rle = list()
    if(NCOL(m_cols)>0){
        m_cols[is.na(m_cols)] = ""
        strange = colSums(m_cols != "") ==0
        m_cols[1, strange] = " "
        
        for(i in seq_len(nrow(m_cols))){
            # y = colSums((m_cols[1:i,-1L, drop = FALSE] != m_cols[1:i, -ncol(m_cols), drop = FALSE]) &
            #                 (m_cols[1:i, -1L, drop = FALSE] != "") )>0
            y = colSums((m_cols[1:i,-1L, drop = FALSE] != m_cols[1:i, -ncol(m_cols), drop = FALSE]))>0
            changes = c(which(y | is.na(y)), ncol(m_cols))
            row_rle[[i]] = structure(list(lengths = diff(c(0L, changes)), values = m_cols[i, changes]))
        }
        for (each_row in seq_along(row_rle)){
            curr_col = 1
            names(row_rle[[each_row]]) = c("colspan","values")
            row_rle[[each_row]][["rowspan"]] = rep(1, length(row_rle[[each_row]]$values))
            curr_row = row_rle[[each_row]] 
            for(each_item in seq_along(curr_row$values)){
                for(each in m_cols[-(1:each_row), curr_col]){
                    if(each == "") {
                        curr_row$rowspan[each_item] = curr_row$rowspan[each_item] + 1
                    } else {
                        break
                    }
                    
                }
                curr_col = curr_col + curr_row$colspan[each_item] 
            }
            # if(each_row>1){
            empty = curr_row$values %in% ""
            # if (each_row == 1) empty[1] = FALSE
            curr_row$values = curr_row$values[!empty]
            curr_row$colspan = curr_row$colspan[!empty]
            curr_row$rowspan = curr_row$rowspan[!empty]
            # }
            row_rle[[each_row]] = curr_row
        }
        row_rle[[1]]$values = c(corner[1,1],row_rle[[1]]$values)
        row_rle[[1]]$colspan = c(ncol(corner),row_rle[[1]]$colspan)
        row_rle[[1]]$rowspan = c(nrow(corner),row_rle[[1]]$rowspan)
    } else {
        row_rle[[1]] = list(values = corner[1,1], 
                            colspan = ncol(corner),
                            rowspan = nrow(corner)
        )
    }
    htmltools::withTags(table(
        class = 'display',
        thead(
            lapply(row_rle, function(row){
                tr(lapply(seq_along(row$values),function(item){
                    th(
                        htmltools::tags$style(type = "text/css",
                                              htmltools::HTML("th { text-align: center; } ")
                        ),
                        htmltools::tags$style(type = "text/css",
                                              htmltools::HTML("th {border: 1px solid #DDD}")
                        ),
                        rowspan = row$rowspan[item], 
                        colspan = row$colspan[item], 
                        row$values[item]
                    )      
                }))
            })
        )
    ))
}


#' @export
#' @rdname as.datatable_widget
as.datatable_widget.with_caption = function(data, 
                                            ...,
                                      repeat_row_labels = FALSE, 
                                      show_row_numbers = FALSE,
                                      digits = get_expss_digits()){
    caption = get_caption(data)
    data = set_caption(data, NULL)
    as.datatable_widget(
        data,
        ...,
        repeat_row_labels = repeat_row_labels, 
        show_row_numbers = show_row_numbers,
        digits = digits,
        caption = caption
        
    )
}





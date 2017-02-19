#' Create an HTML table widget for usage with Shiny
#' 
#' This is method for for rendering results of \link{fre}/\link{cro} in Shiny.
#' For detailed description of function and its arguments see \link[DT]{datatable}.
#'
#' @param data a data object (result of \link{fre}/\link{cro} and etc)
#' @param repeat_row_labels logical Should we repeat duplicated row labels in
#'   the every row?
#' @param show_row_numbers logical 
#' @param digits integer If it is not NULL than all numeric columns will be
#'   rounded to speciefied number of digits.
#' @param ... further parameters for \link[DT]{datatable}
#'
#' @return Object of class \link[DT]{datatable}
#' @seealso \link[htmlTable]{htmlTable} for knitting
#' @export
#'
#' @examples
#' \dontrun{ 
#' data(mtcars)
#' mtcars = apply_labels(mtcars,
#'                       mpg = "Miles/(US) gallon|Mean",
#'                       cyl = "Number of cylinders",
#'                       disp = "Displacement (cu.in.)|Mean",
#'                       hp = "Gross horsepower|Mean",
#'                       drat = "Rear axle ratio",
#'                       wt = "Weight (lb/1000)",
#'                       qsec = "1/4 mile time|Mean",
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
#' mtcars_table = calculate(mtcars,
#'                          cro_mean(list(mpg, hp), am %nest% vs) %add_rows%
#'                              cro_cpct(am, am %nest% vs)  
#'                          
#' )
#' library(shiny)
#' shinyApp(
#'     ui = fluidPage(fluidRow(column(12, DT::dataTableOutput('tbl')))),
#'     server = function(input, output) {
#'         output$tbl = DT::renderDataTable(
#'             datatable(mtcars_table, digits = 1)
#'         )
#'     }
#' )
#' }
datatable = function(data, ...){
    UseMethod("datatable")
}  

#' @export
datatable.default = function(data, ...){
    DT::datatable(data, ...)
}

#' @export
#' @rdname datatable
datatable.simple_table = function(data, 
                                  repeat_row_labels = FALSE, 
                                  show_row_numbers = FALSE,
                                  digits = NULL,
                                  ...){
    if(!is.null(digits)){
        for (i in seq_len(NCOL(data))){
            if(is.numeric(data[[i]])){
                data[[i]] = round(data[[i]], digits)
            }
        }
    }
    first_lab = colnames(data)[1]
    row_labels = data[[1]]
    data[[1]] = NULL # remove first column. This method is needed to prevent column names damaging
    header = t(split_labels(colnames(data), split = "|", remove_repeated = FALSE))
    
    row_labels = dtfrm(split_labels(row_labels, split = "|", remove_repeated = !repeat_row_labels))
    if(show_row_numbers) {
        row_labels = dtfrm(seq_len(nrow(row_labels)), row_labels)    
    }
    colnames(row_labels) = rep("", ncol(row_labels))
    empty_corner = matrix("", nrow = nrow(header), ncol = ncol(row_labels))
    header = cbind(empty_corner, header)
    if(!is.na(first_lab && first_lab!="row_labels")){
        header[1,1] = first_lab    
    }
    args = list(...)
    class = args[["class"]]
    if(is.null(class)) {
       class = 'stripe hover cell-border row-border order-column compact'
    }  
    filter = args[["filter"]]
    if(is.null(filter)) {
        filter = "none"
    }  
    options = args[["options"]]
    if(is.null(options)) {
        options = list(paging = FALSE,
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
                       ))
    }  
    header = matrix_header_to_html(header)
    data = cbind(row_labels, data)
    res = DT::datatable(data, 
                  container = header,
                  class = class, #  
                  rownames = FALSE,
                  filter = filter,
                  options = options 
                  )
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
    DT::formatStyle(res, NCOL(empty_corner):NCOL(data), textAlign = 'right')

}


#' @export
#' @rdname datatable
datatable.etable = datatable.simple
    
    
    
matrix_header_to_html = function(m_cols){
    # to pass CRAN check
    thead = NULL
    tr = NULL
    th = NULL
    m_cols[is.na(m_cols)] = ""
    row_rle = list()
    for(i in seq_len(nrow(m_cols))){
        y = colSums((m_cols[1:i,-1L, drop = FALSE] != m_cols[1:i, -ncol(m_cols), drop = FALSE]) & 
                        (m_cols[1:i, -1L, drop = FALSE] != "") )>0
        changes = c(which(y | is.na(y)), ncol(m_cols))
        row_rle[[i]] = structure(list(lengths = diff(c(0L, changes)), values = m_cols[i, changes]), 
                  class = "rle")
    }
    for (each_row in seq_along(row_rle)){
        curr_col = 1
        class(row_rle[[each_row]]) = NULL
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
        empty = curr_row$values %in% ""
        if (each_row == 1) empty[1] = FALSE
        curr_row$values = curr_row$values[!empty]
        curr_row$colspan = curr_row$colspan[!empty]
        curr_row$rowspan = curr_row$rowspan[!empty]
        row_rle[[each_row]] = curr_row
    }
    
    withTags(table(
        class = 'display',
        thead(
            
            lapply(row_rle, function(row){
                tr(lapply(seq_along(row$values),function(item){
                    th(tags$style(type = "text/css",
                                  HTML("th { text-align: center; } ")
                    ),
                    tags$style(type = "text/css",
                               HTML("th {border: 1px solid #DDD}")
                    ),
                     rowspan = row$rowspan[item], colspan = row$colspan[item], row$values[item])      
                }))
            })
        )
    ))
}





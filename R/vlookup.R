#' Look up values in dictionary.
#' 
#' This function is inspired by VLOOKUP spreadsheet function. Looks for a
#' lookup_value in the lookup_column of the dict, and then returns values in the
#' same rows from result_columns.
#' 
#' @param lookup_value Vector of looked up values
#' @param dict Dictionary. Should be vector/matrix or data.frame
#' @param result_columns Resulting columns of dict. Should be numeric or 
#'   character vector. There is a special value 'row.names'. If result_columns =
#'   'row.names' and dict is matrix/data.frame then row names of dict will be 
#'   returned. If result_columns = 'row.names' and dict is vector then names of 
#'   vector will be returned. If result_columns is NULL (default) then result 
#'   will be entire rows.
#' @param lookup_column Column of dict in which lookup value will be searched.
#'   By default it is the first column of the dict. There is a special value
#'   'row.names'. If lookup_columns = 'row.names' and dict is matrix/data.frame
#'   then values will be searched in the row names of dict. If lookup_columns =
#'   'row.names' and dict is vector then values will be searched in names of the
#'   dict.
#'   
#' @return Vector (if result_columns length is 1) or matrix/data.frame.
#'   
#' @export
#' @examples
#' # with data.frame
#' dict = data.frame(num=1:26,small=letters,cap=LETTERS,stringsAsFactors = FALSE)
#' rownames(dict) = paste0('rows',1:26)
#' identical(vlookup(1:3,dict),dict[1:3,]) # should be TRUE

#' vlookup(c(45,1:3,58),dict,result_columns='cap')
#' vlookup(c('z','d','f'),dict,lookup_column = 'small')
#' vlookup(c('rows7','rows2','rows5'),dict,lookup_column = 'row.names')
#' 
#' # with vector
#' dict=1:26
#' names(dict) = letters
#' 
#' vlookup(c(2,4,6),dict,result_columns='row.names')
#' 
#' # Just for fun. Examples borrowed from Microsoft Excel.
#' # It is not the R way of doing things.
#' 
#' # Example 2
#' 
#' ex2 = read.table(header = TRUE, text = "
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
#' A2 = ex2[1,1]
#' A3 = ex2[2,1]
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
#' ex3 = read.table(header = TRUE, text = "
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
#' ifelse(is.na(vlookup(5,ex3,"Last_name")),
#'        "Employee not found", 
#'        vlookup(5,ex3,"Last_name"))  # Burke 
#' ifelse(is.na(vlookup(15,ex3,"Last_name")),
#'        "Employee not found",
#'        vlookup(15,ex3,"Last_name")) # Employee not found 
#' 
#' # For the employee with an ID of 4, concatenates the values of three cells into
#' # a complete sentence.
#' paste0(vlookup(4,ex3,"First_name"), " ",
#'        vlookup(4,ex3,"Last_name"), " is a ", 
#'        vlookup(4,ex3,"Title")) # Michael Patten is a Sales Rep.
vlookup = function(lookup_value,dict,result_columns=NULL,lookup_column=1) {
    
    # validate lookup_column
    stopif(length(lookup_column)!=1L,"lookup_column shoud be vector of length 1.")
    stopif(is.numeric(lookup_column) && max(lookup_column,na.rm = TRUE)>NCOL(dict),
           "lookup_column is greater than number of columns in the dict.")
    stopif(is.numeric(lookup_column) && any(lookup_column <= 0),
           "lookup_column should be positive.")
    stopif(is.character(lookup_column) && lookup_column!="row.names" &&
               (is.matrix(dict) || is.data.frame(dict)) && !all(lookup_column %in% colnames(dict)),
           "lookup_column doesn't exists in column names of the dict.")
    
    
    # validate result_column
    stopif(!is.null(result_columns) && any(is.na(result_columns)), "NA's in result_columns")
    
    stopif(is.numeric(result_columns) && max(result_columns,na.rm = TRUE)>NCOL(dict),
           "result_columns is greater than number of columns in the dict.")
    stopif(is.character(result_columns) && result_columns!="row.names" &&
               (is.matrix(dict) || is.data.frame(dict)) && !all(result_columns %in% colnames(dict)),
           "some names in result_columns doesn't exists in column names of the dict.")
    
    # calculate index
    if (is.numeric(lookup_column) || is.character(lookup_column)){
        if (is.matrix(dict) || is.data.frame(dict)) {
            if (lookup_column=="row.names"){
                ind = match(lookup_value,row.names(dict),incomparables = NA) 	
            } else {
                ind = match(lookup_value,dict[,lookup_column],incomparables = NA) 
            }
        } else {
            if (lookup_column=="row.names"){
                ind = match(lookup_value,names(dict),incomparables = NA) 	
            } else {
                ind = match(lookup_value,dict,incomparables = NA)
            }
        }
    } else stop("lookup_column shoud be character or numeric.")
    ### caclulate result
    if (is.null(result_columns)){
        if (is.matrix(dict) || is.data.frame(dict)) {
            dict[ind,] # exit function
        } else {
            dict[ind]  # exit function
        }
    } else {
        if (length(result_columns) == 1 && result_columns == "row.names"){
            if (is.matrix(dict) || is.data.frame(dict)) {
                rownames(dict)[ind] # exit function
            } else {
                names(dict)[ind]  # exit function
            }
        } else {
            if (is.matrix(dict) || is.data.frame(dict)) {
                dict[ind,result_columns] # exit function
            } else {
                dict[ind]  # exit function
            }	
            
        }
    }
}

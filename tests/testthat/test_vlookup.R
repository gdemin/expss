context("vlookup data.frame")

dict = data.frame(num=1:26,small=letters,cap=LETTERS,stringsAsFactors = FALSE)
dict_mat = cbind(num=1:26,small=letters,cap=LETTERS)
rownames(dict) = paste0('rows',1:26)
rownames(dict_mat) = paste0('rows',1:26)
reset_rownames = function(x){
    rownames(x) = seq_len(NROW(x))
    x
}
expect_identical(vlookup_df(1:3,dict),reset_rownames(dict[1:3,])) 
expect_identical(vlookup(1:3,dict), dict[1:3,2]) 



expect_identical(vlookup(c(45,1:3,58,NA),dict,result_column='cap'),
                 c(NA,"A", "B", "C", NA,NA))
expect_identical(vlookup(c(45,1:3,58,NA),dict_mat,result_column='cap'),
                 c(NA,"A", "B", "C", NA,NA))
expect_identical(vlookup_df(c('z','d','f','d'),dict,lookup_column = 'small'),
                 reset_rownames(dict[c(26,4,6,4),]))
expect_identical(vlookup_df(c('rows1','rows5','rows2','rows2'),
                            dict,result_column = c("small","cap"),lookup_column = 'row.names'),
                 reset_rownames(dict[c(1,5,2,2),c("small","cap")]))

expect_identical(vlookup_df(c('rows1','rows5','rows2','rows2'),
                            dict,result_column = c("small","cap", "names"),
                            lookup_column = 'row.names'),
                 reset_rownames(
                 dtfrm(dict[c(1,5,2,2),c("small","cap")], row_names = rownames(dict)[c(1,5,2,2)])))

expect_identical(vlookup_df(c('rows1','rows5','rows2','rows2'), 
                            dict, result_column = c("row.names", "small","cap"),
                            lookup_column = 'row.names'),
                 reset_rownames(
                     dtfrm(
                         row_names = rownames(dict)[c(1,5,2,2)], 
                         dict[c(1,5,2,2),c("small","cap")])))

expect_identical(vlookup_df(c('z','d','f','d'),
                            dict_mat,
                            lookup_column = 'small'),
                 reset_rownames(as.dtfrm(dict_mat)[c(26,4,6,4),, drop = FALSE]))
expect_identical(vlookup_df(c('rows1','rows5','rows2','rows2'),dict_mat,result_column = c("small","cap"),lookup_column = 'row.names'),
                 reset_rownames(as.dtfrm(dict)[c(1,5,2,2),c("small","cap")]))

expect_error(vlookup(iris, mtcars))
expect_error(vlookup(c('rows1','rows5','rows2','rows2'),dict,result_column = c("small","cap"),lookup_column = 'row.names'))

context("vlookup tbl_df")
if(suppressWarnings(require(dplyr, quietly = TRUE))){

    
    dict = tbl_df(data.frame(num=1:26, small=letters, cap=LETTERS, stringsAsFactors = FALSE))
    # rownames(dict) = paste0('rows',1:26)
    expect_identical(vlookup_df(1:3, dict), dict[1:3,])
    
    expect_identical(vlookup(c(45, 1:3, 58, NA), dict, result_column='cap'), c(NA, "A", "B", "C", NA, NA))
    expect_identical(vlookup_df(c('z', 'd', 'f', 'd'), dict, lookup_column = 'small'), dict[c(26, 4, 6, 4),])
    # expect_identical(vlookup_df(c('rows1', 'rows5', 'rows2', 'rows2'), dict, result_column = c("small", "cap"), lookup_column = 'row.names'),
    #                  dict[c(1, 5, 2, 2), c("small", "cap")])
    
} else {
	cat("dplyr not found\n")
}
context("vlookup vector")
# with vector
dict=1:26
names(dict) = letters

expect_identical(vlookup(c(6, 4, 2), dict, result_column='row.names'),c("f","d","b"))
expect_identical(vlookup(c(6, 4, 2), dict, result_column='rownames'),c("f","d","b"))
expect_identical(vlookup(c(6, 4, 2), dict, result_column='names'),c("f","d","b"))

expect_identical(vlookup(c("f","d","b"), dict, result_column = 1, lookup_column='row.names'), c(6L, 4L, 2L))
expect_identical(vlookup(c("f","d","b"), dict, result_column = 1, lookup_column='rownames'), c(6L, 4L, 2L))
expect_identical(vlookup(c("f","d","b"), dict, result_column = 1, lookup_column='names'), c(6L, 4L, 2L))

expect_identical(vlookup(c("f","d","b"), dict, result_column = NULL, lookup_column='row.names'), c(6L, 4L, 2L))
expect_identical(vlookup(c("f","d","b"), dict, result_column = NULL, lookup_column='rownames'), c(6L, 4L, 2L))
expect_identical(vlookup(c("f","d","b"), dict, result_column = NULL, lookup_column='names'), c(6L, 4L, 2L))

expect_identical(vlookup_df(c(6, 4, 2), dict, result_column='row.names'),dtfrm(row_names = c("f","d","b")))
expect_identical(vlookup_df(c(6, 4, 2), dict, result_column='rownames'),dtfrm(row_names = c("f","d","b")))
expect_identical(vlookup_df(c(6, 4, 2), dict, result_column='names'),dtfrm(row_names = c("f","d","b")))

context("vlookup excel examples ex2")

# Just for fun. Examples borrowed from Microsoft Excel.
# It is not the R way of doing things.

# Example 2

ex2 = read.table(header = TRUE, text = "
    Item_ID Item Cost Markup 
    ST-340 Stroller 145.67  0.30  
    BI-567 Bib 3.56  0.40  
    DI-328 Diapers  21.45  0.35  
    WI-989 Wipes  5.12  0.40  
    AS-469 Aspirator 2.56  0.45 
", stringsAsFactors = FALSE)

# Calculates the retail price of diapers by adding the markup percentage to the cost. 
expect_identical(vlookup("DI-328", ex2, 3) * (1 + vlookup("DI-328", ex2, 4)) ,28.9575)

# Calculates the sale price of wipes by subtracting a specified discount from
# the retail price.
expect_identical((vlookup("WI-989", ex2, "Cost") * (1 + vlookup("WI-989", ex2, "Markup"))) * (1 - 0.2), 5.7344)

A2 = ex2[1,1]
A3 = ex2[2,1]

# If the cost of an item is greater than or equal to $20.00, displays the string
# "Markup is nn%"; otherwise, displays the string "Cost is under $20.00".
expect_identical(ifelse(vlookup(A2, ex2, "Cost") >= 20, 
       paste0("Markup is " , 100 * vlookup(A2, ex2, "Markup"),"%"), 
       "Cost is under $20.00"), 'Markup is 30%')


# If the cost of an item is greater than or equal to $20.00, displays the string
# Markup is nn%"; otherwise, displays the string "Cost is $n.nn".
expect_identical(ifelse(vlookup(A3, ex2, "Cost") >= 20, 
       paste0("Markup is: " , 100 * vlookup(A3, ex2, "Markup") , "%"), 
       paste0("Cost is $", vlookup(A3, ex2, "Cost"))), 'Cost is $3.56')


# Example 3
context("vlookup excel examples ex3")

ex3 = read.table(header = TRUE, text = "
    ID  Last_name  First_name  Title Birth_date  
    1 Davis Sara 'Sales Rep.'  12/8/1968 
    2 Fontana Olivier 'V.P. of Sales' 2/19/1952 
    3 Leal Karina 'Sales Rep.' 8/30/1963 
    4 Patten Michael 'Sales Rep.' 9/19/1958 
    5 Burke Brian 'Sales Mgr.' 3/4/1955 
    6 Sousa Luis 'Sales Rep.'  7/2/1963  
", stringsAsFactors = FALSE)

# If there is an employee with an ID of 5, displays the employee's last name;
# otherwise, displays the message "Employee not found".
expect_identical(ifelse(is.na(vlookup(5,ex3,"Last_name")),
       "Employee not found", 
       vlookup(5,ex3,"Last_name")), 'Burke') 
expect_identical(ifelse(is.na(vlookup(15,ex3,"Last_name")),
       "Employee not found",
       vlookup(15,ex3,"Last_name")), 'Employee not found')

# For the employee with an ID of 4, concatenates the values of three cells into
# a complete sentence.
expect_identical(paste0(vlookup(4,ex3,"First_name"), " ",
       vlookup(4,ex3,"Last_name"), " is a ", 
       vlookup(4,ex3,"Title")), 'Michael Patten is a Sales Rep.')

 

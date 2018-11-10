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
                 sheet(dict[c(1,5,2,2),c("small","cap")], row_names = rownames(dict)[c(1,5,2,2)])))

expect_identical(vlookup_df(c('rows1','rows5','rows2','rows2'), 
                            dict, result_column = c("row.names", "small","cap"),
                            lookup_column = 'row.names'),
                 reset_rownames(
                     sheet(
                         row_names = rownames(dict)[c(1,5,2,2)], 
                         dict[c(1,5,2,2),c("small","cap")])))

expect_identical(vlookup_df(c('z','d','f','d'),
                            dict_mat,
                            lookup_column = 'small'),
                 reset_rownames(as.sheet(dict_mat)[c(26,4,6,4),, drop = FALSE]))
expect_identical(vlookup_df(c('rows1','rows5','rows2','rows2'),dict_mat,result_column = c("small","cap"),lookup_column = 'row.names'),
                 reset_rownames(as.sheet(dict)[c(1,5,2,2),c("small","cap")]))

expect_error(vlookup(iris, mtcars))
expect_error(vlookup(c('rows1','rows5','rows2','rows2'),dict,result_column = c("small","cap"),lookup_column = 'row.names'))


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

expect_identical(vlookup_df(c(6, 4, 2), dict, result_column='row.names'),sheet(row_names = c("f","d","b")))
expect_identical(vlookup_df(c(6, 4, 2), dict, result_column='rownames'),sheet(row_names = c("f","d","b")))
expect_identical(vlookup_df(c(6, 4, 2), dict, result_column='names'),sheet(row_names = c("f","d","b")))

expect_identical(vlookup(c(1, NA, 2), 
                         dict = sheet(a = c(2, NA), b= c(3, 2))
                         ),
                 c(NA, 2, 3)
                 )

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

context("add_columns")
 
sh1 = sheet(a = 1:5, b = 5:1)
sh2 = sheet(a = 4:2, d = 4:2)

res = text_to_columns_csv("
                a,b,d
                1,5,NA
                2,4,2
                3,3,3
                4,2,4
                5,1,NA
                ")

expect_identical(add_columns(sh1, sh2), res)
expect_error(add_columns(sh1, sh2, by = "ee"))
expect_error(add_columns(sh1, sh2, by = c("d","ee")))
expect_error(add_columns(sh1, sh2, by = c("a","a")))
expect_identical(add_columns(sh1, sh2, by = "a"), res)
expect_identical(add_columns(sh1, sh2, by = 1), res)

sh2 = sheet(e = 4:2, d = 4:2)
expect_identical(add_columns(sh1, sh2, by = 1), res)
expect_identical(add_columns(sh1, sh2, by = c(a = "e")), res)
expect_error(add_columns(sh1, sh2))

sh2 = sheet(a = c(4:2,2), d = c(4:2, 42L))
expect_error(add_columns(sh1, sh2, by =1))
expect_identical(add_columns(sh1, sh2, by = "a", ignore_duplicates = TRUE), res)

sh2 = sheet(a = 4:2, d = 4:2)

res = text_to_columns_csv("
                    a,b,d
                    1,5,NA
                    2,4,4
                    3,3,3
                    4,2,2
                    5,1,NA
                    ")

expect_identical(add_columns(sh1, sh2, by = c("b" = "a")), res)

res = text_to_columns_csv("
                a,b,b_1
                    1,5,NA
                    2,4,2
                    3,3,3
                    4,2,4
                    5,1,NA
                    ")

sh2 = sheet(a = 4:2, b = 4:2)

expect_warning(add_columns(sh1, sh2, by = "a"))
expect_identical(suppressWarnings(add_columns(sh1, sh2, by = "a")), res)

sh2 = sheet(a = 4:2, b = 2:4, d = c(NA, NA, 42L))

res = text_to_columns_csv("
                a,b,d
                    1,5,NA
                    2,4,42
                    3,3,NA
                    4,2,NA
                    5,1,NA
                    ")

expect_identical(add_columns(sh1, sh2), res)
expect_identical(add_columns(sh1, sh2, by = c("a", "b")), res)
expect_identical(add_columns(sh1, sh2, by = c("b", "a")), res)
sh2 = sheet(a = 4:2, ee = 2:4, d = c(NA, NA, 42L))
expect_identical(add_columns(sh1, sh2, by = c("b" = "ee", "a")), res)

sh2 = sheet(a = 4:2, b = 2:4, d = c(NA, NA, 42L))
res = text_to_columns_csv("
                a,b,d
                    1,5,NA
                    2,4,NA
                    3,3,NA
                    4,2,42
                    5,1,NA
                    ")

expect_identical(add_columns(sh1, sh2, by = c("b" = "a", "a" = "b")), res)

default_dataset(sh1)
.add_columns(sh2, by = c("b" = "a", "a" = "b"))
expect_identical(sh1, res)


############################
context("add_columns data.table")
# data.table is modified by reference
sh1 = data.table(a = 1:5, b = 5:1)
sh2 = data.table(a = 4:2, d = 4:2)
res = data.table(text_to_columns_csv("
                a,b,d
                1,5,NA
                2,4,2
                3,3,3
                4,2,4
                5,1,NA
                "))

# sh1 and sh2 both are data.tables
expect_identical(add_columns(sh1, sh2), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = "a"), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = 1), res)

sh1 = data.table(a = 1:5, b = 5:1)
sh2 = data.table(e = 4:2, d = 4:2)
expect_identical(add_columns(sh1, sh2, by = 1), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = c(a = "e")), res)

# only data is data.table
sh2 = sheet(a = 4:2, d = 4:2)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = "a"), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = 1), res)

sh1 = data.table(a = 1:5, b = 5:1)
sh2 = sheet(e = 4:2, d = 4:2)
expect_identical(add_columns(sh1, sh2, by = 1), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = c(a = "e")), res)


# only dict is data.table
sh1 = sheet(a = 1:5, b = 5:1)
sh2 = data.table(a = 4:2, d = 4:2)
res = text_to_columns_csv("
                a,b,d
               1,5,NA
               2,4,2
               3,3,3
               4,2,4
               5,1,NA
               ")
expect_identical(add_columns(sh1, sh2), res)
expect_identical(add_columns(sh1, sh2, by = "a"), res)
expect_identical(add_columns(sh1, sh2, by = 1), res)

sh2 =  data.table(e = 4:2, d = 4:2)
expect_identical(add_columns(sh1, sh2, by = 1), res)
expect_identical(add_columns(sh1, sh2, by = c(a = "e")), res)

context("html datatable")

data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon|Mean",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)|Mean",
                      hp = "Gross horsepower|Mean",
                      drat = "Rear axle ratio",
                      wt = "Weight (lb/1000)",
                      qsec = "1/4 mile time|Mean",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)

options(expss.digits = NA)
set.seed(1)
mtcars_table = cro_cpct(mtcars$vs %nest% mtcars$am, list(mtcars$vs %nest% mtcars$am, "#Total")) 

expect_equal_to_reference(datatable(mtcars_table) %n_d% c("dependencies"),
                          "rds/html_datatable1.rds")
expect_equal_to_reference(datatable(mtcars_table[FALSE, ]) %n_d% c("dependencies"), 
                          "rds/html_datatable2.rds")
expect_equal_to_reference(datatable(mtcars_table[, 1])  %n_d% c("dependencies"), 
                          "rds/html_datatable2single1.rds")

expect_equal_to_reference(datatable(mtcars_table[, 1], repeat_row_labels = TRUE)  %n_d% c("dependencies"), 
                          "rds/html_datatable2single2.rds")

expect_equal_to_reference(datatable(mtcars_table[, 1], show_row_numbers = TRUE)  %n_d% c("dependencies"), 
                          "rds/html_datatable2single3.rds")

expect_equal_to_reference(datatable(mtcars_table[, FALSE, drop = FALSE])  %n_d% c("dependencies"), 
                          "rds/html_datatable2empty1.rds")

expect_equal_to_reference(datatable(mtcars_table[, FALSE, drop = FALSE], repeat_row_labels = TRUE)  %n_d% 
                              c("dependencies"), 
                          "rds/html_datatable2empty2.rds")

expect_equal_to_reference(datatable(mtcars_table[, FALSE, drop = FALSE], show_row_numbers = TRUE)  %n_d%
                              c("dependencies"), 
                          "rds/html_datatable2empty3.rds")

expect_equal_to_reference(datatable(mtcars_table, digits = 0) %n_d% c("dependencies"),
                          "rds/html_datatable3.rds")

expect_equal_to_reference(datatable(mtcars_table, repeat_row_labels = TRUE) %n_d% c("dependencies"),
                          "rds/html_datatable4.rds")

expect_equal_to_reference(datatable(mtcars_table, show_row_numbers = TRUE) %n_d% c("dependencies"),
                          "rds/html_datatable5.rds")

expect_equal_to_reference(datatable(mtcars_table, filter = "top") %n_d% c("dependencies"),
                          "rds/html_datatable6.rds")

expect_equal_to_reference(datatable(mtcars_table, options = list(ordering = TRUE)) %n_d% c("dependencies"),
                          "rds/html_datatable7.rds")

expect_equal_to_reference(datatable(mtcars_table, class = "") %n_d% c("dependencies"),
                          "rds/html_datatable8.rds")

expect_equal_to_reference(datatable(mtcars) %n_d% c("dependencies"),
                          "rds/html_datatable9.rds")



mtcars_table = calculate(mtcars,
                         cro_mean(list(mpg, hp), am %nest% vs) )
expect_equal_to_reference(datatable(mtcars_table) %n_d% c("dependencies"),
                          "rds/html_datatable10.rds")

mtcars_table = cro_cpct(mtcars$vs %nest% mtcars$am, list(mtcars$vs, "#Total"))
expect_equal_to_reference(datatable(mtcars_table) %n_d% c("dependencies"),
                          "rds/html_datatable11.rds")

mtcars_table = cro_cpct(mtcars$vs, list(mtcars$vs %nest% mtcars$am, "#Total"), prepend_var_lab = FALSE)
expect_equal_to_reference(datatable(mtcars_table) %n_d% c("dependencies"),
                          "rds/html_datatable12.rds")

expect_equal_to_reference(datatable(mtcars_table[,1]) %n_d% c("dependencies"),
                          "rds/html_datatable12single.rds")

expect_equal_to_reference(datatable(mtcars_table[,1], show_row_numbers = TRUE) %n_d% c("dependencies"),
                          "rds/html_datatable12single2.rds")


expect_equal_to_reference(datatable(mtcars_table[,FALSE, drop = FALSE]) %n_d% c("dependencies"),
                          "rds/html_datatable12empty.rds")

expect_equal_to_reference(datatable(mtcars_table[,FALSE, drop = FALSE], show_row_numbers = TRUE) %n_d% 
                              c("dependencies"),
                          "rds/html_datatable12empty2.rds")


new_am = mtcars$am
mtcars_table = cro_cpct(mtcars$vs %nest% mtcars$am, list(mtcars$vs %nest% mtcars$am, "#Total")) %merge%
    cro_cpct(mtcars$vs %nest% mtcars$am, list(new_am, "#Total"))

expect_equal_to_reference(datatable(mtcars_table) %n_d% c("dependencies"),
                          "rds/html_datatable13.rds")

var_lab(new_am) = "|"
val_lab(new_am) = setNames(0:1, c("", " "))
mtcars_table = cro_cpct(mtcars$vs %nest% mtcars$am, list(mtcars$vs %nest% mtcars$am, "#Total")) %merge%
    cro_cpct(mtcars$vs %nest% mtcars$am, list(new_am, "#Total"))
colnames(mtcars_table)[7] = ""

expect_equal_to_reference(datatable(mtcars_table) %n_d% c("dependencies"),
                          "rds/html_datatable14.rds")


options(expss.digits = 0)
expect_equal(datatable(mtcars_table),
                          datatable(mtcars_table, digits = 0))

options(expss.digits = 3)
expect_equal(datatable(mtcars_table),
                          datatable(mtcars_table, digits = 3))

expect_true(!isTRUE(all.equal(datatable(mtcars_table),
                          datatable(mtcars_table, digits = 4))))

options(expss.digits = NULL)
expect_equal(datatable(mtcars_table),
                          datatable(mtcars_table, digits = 1))

# 
# library(testthat)
# library(expss)
# 
# 
# library(shiny)
# shinyApp(
#     ui = fluidPage(fluidRow(column(12, DT::dataTableOutput('tbl')))),
#     server = function(input, output) {
#         output$tbl = DT::renderDataTable(
#             datatable(mtcars_table)
#         )
#     }
# )
# 
# `%n_d%` = function(e1, e2){
#     shinyApp(
#         ui = fluidPage(fluidRow(column(12, DT::dataTableOutput('tbl')))),
#         server = function(input, output) {
#             output$tbl = DT::renderDataTable(
#                 e1
#             )
#         }
#     )
# }
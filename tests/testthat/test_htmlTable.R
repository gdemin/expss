context("htmlTable")

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
mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total")) 

expect_equal_to_reference(htmlTable(mtcars_table),
                          "rds/htmlTable1.rds")
expect_equal_to_reference(suppressWarnings(htmlTable(mtcars_table[FALSE, ])) , 
                          "rds/htmlTable2.rds")
expect_equal_to_reference(htmlTable(mtcars_table[, 1])  , 
                          "rds/htmlTable2single1.rds")


expect_equal_to_reference(htmlTable(mtcars_table[, FALSE, drop = FALSE])  , 
                          "rds/htmlTable2empty1.rds")


expect_equal_to_reference(htmlTable(mtcars_table, digits = 0) ,
                          "rds/htmlTable3.rds")

mtcars_table = calculate(mtcars,
                         cro_mean(list(mpg, hp), list(am %nest% vs)) )
expect_equal_to_reference(htmlTable(mtcars_table) ,
                          "rds/htmlTable10.rds")

mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs, "#Total"))
expect_equal_to_reference(htmlTable(mtcars_table) ,
                          "rds/htmlTable11.rds")

mtcars_table = cro_cpct(list(unvr(mtcars$vs)), list(mtcars$vs %nest% mtcars$am, "#Total"))
expect_equal_to_reference(htmlTable(mtcars_table) ,
                          "rds/htmlTable12.rds")

expect_equal_to_reference(htmlTable(mtcars_table[,1]) ,
                          "rds/htmlTable12single.rds")


new_am = mtcars$am
mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total")) %merge%
    cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total"))

expect_equal_to_reference(htmlTable(mtcars_table) ,
                          "rds/htmlTable13.rds")

expect_equal_to_reference(htmlTable(mtcars_table, digits = 1) ,
                          "rds/htmlTable13_1.rds")

var_lab(new_am) = "|"
val_lab(new_am) = setNames(0:1, c("", " "))
mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total")) %merge%
    cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total"))
colnames(mtcars_table)[7] = ""

expect_equal_to_reference(htmlTable(mtcars_table) ,
                          "rds/htmlTable14.rds")

expect_equal_to_reference(htmlTable(mtcars_table, digits = 1) ,
                          "rds/htmlTable14_1.rds")

options(expss.digits = 0)
expect_equal(htmlTable(mtcars_table),
             htmlTable(mtcars_table, digits = 0))

options(expss.digits = 3)
expect_equal(htmlTable(mtcars_table),
             htmlTable(mtcars_table, digits = 3))

expect_true(!isTRUE(all.equal(htmlTable(mtcars_table),
                              htmlTable(mtcars_table, digits = 4))))

options(expss.digits = NULL)
expect_equal(htmlTable(mtcars_table),
             htmlTable(mtcars_table, digits = 1))

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

htmlTable = function(x, ...) htmlTable:::print.htmlTable(expss:::htmlTable.etable(x, ...),
                                                         useViewer = TRUE)



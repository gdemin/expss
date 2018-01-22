context("html as.datatable_widget")

# 
# library(testthat)
# library(expss)
# 
# 
# library(shiny)
# 
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

data(mtcars)

# expss::as.datatable_widget(cro(mtcars$am, mtcars$vs)[, 1:2])  %n_d% c("dependencies")
# cro(mtcars$am, mtcars$vs)[, 1:2]  %n_d% c("dependencies")
# DT::renderDataTable(cro(mtcars$am, mtcars$vs)[, 1:2])

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
if(packageVersion("DT") == "0.2"){
    options(expss.digits = NA)
    set.seed(1)
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total"),
                            total_label = "#Total") 
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable1.rds")
    expect_equal_to_reference(as.datatable_widget(mtcars_table[FALSE, ]) %n_d% c("dependencies"), 
                              "rds/html_datatable2.rds")
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, 1])  %n_d% c("dependencies"), 
                              "rds/html_datatable2single1.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, 1], repeat_row_labels = TRUE)  %n_d% c("dependencies"), 
                              "rds/html_datatable2single2.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, 1], show_row_numbers = TRUE)  %n_d% c("dependencies"), 
                              "rds/html_datatable2single3.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, FALSE, drop = FALSE])  %n_d% c("dependencies"), 
                              "rds/html_datatable2empty1.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, FALSE, drop = FALSE], repeat_row_labels = TRUE)  %n_d% 
                                  c("dependencies"), 
                              "rds/html_datatable2empty2.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, FALSE, drop = FALSE], show_row_numbers = TRUE)  %n_d%
                                  c("dependencies"), 
                              "rds/html_datatable2empty3.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, digits = 0) %n_d% c("dependencies"),
                              "rds/html_datatable3.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, repeat_row_labels = TRUE) %n_d% c("dependencies"),
                              "rds/html_datatable4.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, show_row_numbers = TRUE) %n_d% c("dependencies"),
                              "rds/html_datatable5.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, filter = "top") %n_d% c("dependencies"),
                              "rds/html_datatable6.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, options = list(ordering = TRUE)) %n_d% c("dependencies"),
                              "rds/html_datatable7.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, class = "") %n_d% c("dependencies"),
                              "rds/html_datatable8.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars) %n_d% c("dependencies"),
                              "rds/html_datatable9.rds")
    
    
    
    mtcars_table = calculate(mtcars,
                             cro_mean(list(mpg, hp), list(am %nest% vs)) )
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable10.rds")
    
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs, "#Total"),
                            total_label = "#Total") 
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable11.rds")
    
    mtcars_table = cro_cpct(list(unvr(mtcars$vs)), list(mtcars$vs %nest% mtcars$am, "#Total"),
                            total_label = "#Total") 
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable12.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[,1]) %n_d% c("dependencies"),
                              "rds/html_datatable12single.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[,1], show_row_numbers = TRUE) %n_d% c("dependencies"),
                              "rds/html_datatable12single2.rds")
    
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[,FALSE, drop = FALSE]) %n_d% c("dependencies"),
                              "rds/html_datatable12empty.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[,FALSE, drop = FALSE], show_row_numbers = TRUE) %n_d% 
                                  c("dependencies"),
                              "rds/html_datatable12empty2.rds")
    
    
    new_am = mtcars$am
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total"),
                            total_label = "#Total")  %merge%
        cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total"),
                 total_label = "#Total") 
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable13.rds")
    
    var_lab(new_am) = "|"
    val_lab(new_am) = setNames(0:1, c("", " "))
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total"),
                            total_label = "#Total")  %merge%
        cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total"),
                 total_label = "#Total") 
    colnames(mtcars_table)[7] = ""
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable14.rds")
    
    
    options(expss.digits = 0)
    expect_equal(as.datatable_widget(mtcars_table),
                 as.datatable_widget(mtcars_table, digits = 0))
    
    options(expss.digits = 3)
    expect_equal(as.datatable_widget(mtcars_table),
                 as.datatable_widget(mtcars_table, digits = 3))
    
    expect_true(!isTRUE(all.equal(as.datatable_widget(mtcars_table),
                                  as.datatable_widget(mtcars_table, digits = 4))))
    
    options(expss.digits = NULL)
    expect_equal(as.datatable_widget(mtcars_table),
                 as.datatable_widget(mtcars_table, digits = 1))
    
    
    mtcars_table$row_labels[nrow(mtcars_table)] = "<b>#Total</b>"
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable14_escape_html.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, escape = FALSE) %n_d% c("dependencies"),
                              "rds/html_datatable14_no_escape_html.rds")
    
} else {
    options(expss.digits = NA)
    set.seed(1)
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total"),
                            total_label = "#Total") 
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable1_new.rds")
    expect_equal_to_reference(as.datatable_widget(mtcars_table[FALSE, ]) %n_d% c("dependencies"), 
                              "rds/html_datatable2_new.rds")
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, 1])  %n_d% c("dependencies"), 
                              "rds/html_datatable2single1_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, 1], repeat_row_labels = TRUE)  %n_d% c("dependencies"), 
                              "rds/html_datatable2single2_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, 1], show_row_numbers = TRUE)  %n_d% c("dependencies"), 
                              "rds/html_datatable2single3_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, FALSE, drop = FALSE])  %n_d% c("dependencies"), 
                              "rds/html_datatable2empty1_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, FALSE, drop = FALSE], repeat_row_labels = TRUE)  %n_d% 
                                  c("dependencies"), 
                              "rds/html_datatable2empty2_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[, FALSE, drop = FALSE], show_row_numbers = TRUE)  %n_d%
                                  c("dependencies"), 
                              "rds/html_datatable2empty3_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, digits = 0) %n_d% c("dependencies"),
                              "rds/html_datatable3_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, repeat_row_labels = TRUE) %n_d% c("dependencies"),
                              "rds/html_datatable4_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, show_row_numbers = TRUE) %n_d% c("dependencies"),
                              "rds/html_datatable5_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, filter = "top") %n_d% c("dependencies"),
                              "rds/html_datatable6_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, options = list(ordering = TRUE)) %n_d% c("dependencies"),
                              "rds/html_datatable7_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, class = "") %n_d% c("dependencies"),
                              "rds/html_datatable8_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars) %n_d% c("dependencies"),
                              "rds/html_datatable9_new.rds")
    
    
    
    mtcars_table = calculate(mtcars,
                             cro_mean(list(mpg, hp), list(am %nest% vs)) )
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable10_new.rds")
    
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs, "#Total"),
                            total_label = "#Total") 
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable11_new.rds")
    
    mtcars_table = cro_cpct(list(unvr(mtcars$vs)), list(mtcars$vs %nest% mtcars$am, "#Total"),
                            total_label = "#Total") 
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable12_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[,1]) %n_d% c("dependencies"),
                              "rds/html_datatable12single_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[,1], show_row_numbers = TRUE) %n_d% c("dependencies"),
                              "rds/html_datatable12single2_new.rds")
    
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[,FALSE, drop = FALSE]) %n_d% c("dependencies"),
                              "rds/html_datatable12empty_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table[,FALSE, drop = FALSE], show_row_numbers = TRUE) %n_d% 
                                  c("dependencies"),
                              "rds/html_datatable12empty2_new.rds")
    
    
    new_am = mtcars$am
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total"),
                            total_label = "#Total")  %merge%
        cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total"),
                 total_label = "#Total") 
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable13_new.rds")
    
    var_lab(new_am) = "|"
    val_lab(new_am) = setNames(0:1, c("", " "))
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total"),
                            total_label = "#Total")  %merge%
        cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total"),
                 total_label = "#Total") 
    colnames(mtcars_table)[7] = ""
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable14_new.rds")
    
    
    options(expss.digits = 0)
    expect_equal(as.datatable_widget(mtcars_table),
                 as.datatable_widget(mtcars_table, digits = 0))
    
    options(expss.digits = 3)
    expect_equal(as.datatable_widget(mtcars_table),
                 as.datatable_widget(mtcars_table, digits = 3))
    
    expect_true(!isTRUE(all.equal(as.datatable_widget(mtcars_table),
                                  as.datatable_widget(mtcars_table, digits = 4))))
    
    options(expss.digits = NULL)
    expect_equal(as.datatable_widget(mtcars_table),
                 as.datatable_widget(mtcars_table, digits = 1))
    
    
    mtcars_table$row_labels[nrow(mtcars_table)] = "<b>#Total</b>"
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table) %n_d% c("dependencies"),
                              "rds/html_datatable14_escape_html_new.rds")
    
    expect_equal_to_reference(as.datatable_widget(mtcars_table, escape = FALSE) %n_d% c("dependencies"),
                              "rds/html_datatable14_no_escape_html_new.rds")    
}

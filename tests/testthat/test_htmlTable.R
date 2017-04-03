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
mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), 
                        list(mtcars$vs %nest% mtcars$am, "#Total")) 

expect_equal_to_reference(htmlTable(mtcars_table),
                          "rds/htmlTable1.rds")
expect_equal_to_reference(suppressWarnings(htmlTable(mtcars_table[FALSE, ])) , 
                          "rds/htmlTable2.rds")
expect_equal_to_reference(htmlTable(mtcars_table[, 1])  ,  ### wrong
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

expect_equal_to_reference(htmlTable(mtcars_table[,1]) ,    #####
                          "rds/htmlTable12single.rds")

colnames(mtcars_table)[1] = "My table"

expect_equal_to_reference(htmlTable(mtcars_table[,1]) ,    #####
                          "rds/htmlTable12single2.rds")

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

data("product_test")
res = product_test %>%
    tab_cols(c1) %>%
    tab_cells(unvr(mrset(a1_1 %to% a1_6))) %>%
    tab_stat_cpct(label = var_lab(a1_1)) %>%
    tab_cells(unvr(mrset(b1_1 %to% b1_6))) %>%
    tab_stat_cpct(label = var_lab(b1_1)) %>%
    tab_pivot(stat_position = "inside_columns")

## first row of header with duplicates
expect_equal_to_reference(htmlTable(res), "rds/htmlTable15.rds")

res = product_test %>%
    compute({
        total = 1
        var_lab(total) = "Total"
        val_lab(total) = setNames(1, " ")
        }) %>% 
    tab_cols(total) %>%
    tab_cells(unvr(mrset(a1_1 %to% a1_6))) %>%
    tab_stat_cpct() %>%
    tab_pivot()

# single column header
expect_equal_to_reference(htmlTable(res), "rds/htmlTable16.rds")

# single row header
res = mtcars %>% calc(cro(am, list(unvr(vs))))
expect_equal_to_reference(htmlTable(res), "rds/htmlTable17.rds")

# single row header
res = mtcars %>% calc(cro(list(unvr(am)), list(unvr(vs))))
expect_equal_to_reference(htmlTable(res), "rds/htmlTable18.rds")

# temp = function(x) htmlTable:::print.htmlTable(x, useViewer = TRUE)

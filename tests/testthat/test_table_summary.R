context("table_summary")


data(mtcars)
mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"
    var_lab(vs) = "V/S"
    val_lab(vs) = c("Straight" = 0, "V" = 1)
    var_lab(am) = "Transmission (0 = automatic, 1 = manual)"
    val_lab(am) = c(" automatic" = 0, " manual" =  1)
    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
})

# row_labels = c("row_vars", "row_vars_values", "summary_vars", "fun_names", "stat_names"),
# col_labels = c("col_vars", "col_vars_values")

# table_summary(mtcars %except% c("cyl", "am"), col_vars = mtcars$am, fun = mean)
# table_means(mtcars %except% c("cyl", "am"), col_vars = mtcars$am)
# table_medians(mtcars %except% c("cyl", "am"), col_vars = mtcars$am)
# table_sums(mtcars %except% c("cyl", "am"), col_vars = mtcars$am)
# table_sums(mtcars %except% c("cyl", "am"), col_vars = mtcars$am, weight = 2)
expect_error(table_summary(mtcars %except% c("cyl", "am"), col_vars = mtcars$am, fun = mean,
              row_labels = c("stat_names", "col_vars_values", "row_vars", "row_vars_values", "summary_vars", "col_vars"),
                col_labels = NULL
              ))
expect_error(table_summary(mtcars %except% c("cyl", "am"), col_vars = mtcars$am, fun = mean,
              col_labels = c("stat_names", "col_vars_values", "row_vars", "row_vars_values", "summary_vars",  "col_vars"),
              row_labels = NULL
))
expect_error(table_summary(mtcars %except% c("cyl", "am"), col_vars = mtcars$am, fun = mean,
              row_labels = c("stat_names", "col_vars_values", "row_vars", "row_vars_values", "summary_vars",  "col_vars"),
              col_labels = c("adasd", "rvtrvtt")
))
expect_error(table_summary(mtcars %except% c("cyl", "am"), col_vars = mtcars$am, fun = mean,
              col_labels = c("stat_names", "col_vars_values", "row_vars", "row_vars_values", "summary_vars",  "col_vars"),
              row_labels = c("adasd", "rvtrvtt")
))

#######
expect_error(
    table_summary(mtcars %except% c("cyl", "am"), col_vars = mtcars$am, fun = mean, weight = 2)
    )



expect_equal_to_reference(
    table_summary(mtcars %except% c("cyl", "am"), col_vars = mtcars$am, fun = w_mean, weight = 2),
                          "rds/table_summary0.rds")

expect_equal_to_reference(
    table_summary(mtcars %except% c("cyl", "am"), col_vars = mtcars$am, fun = "w_mean", weight = 2),
    "rds/table_summary0.rds")



expect_equal_to_reference(
    table_summary(mtcars %except% c("cyl", "am"), col_vars = list("Total", mtcars$am), fun = mean),
    "rds/table_summary1.rds"
    )


add_val_lab(mtcars$am) = c(HardToSay = 3)
expect_equal_to_reference(
    table_summary(mtcars %except% c("cyl", "am"), col_vars = list("Total", mtcars$am), fun = mean)
    ,"rds/table_summary2.rds"
)

data(mtcars)
mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"
    var_lab(vs) = "V/S"
    val_lab(vs) = c("Straight" = 0, "V" = 1)
    var_lab(am) = "Transmission (0 = automatic, 1 = manual)"
    val_lab(am) = c(" automatic" = 0, " manual" =  1)
    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
})

add_val_lab(mtcars$am) = c(HardToSay = 3)
expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_labels = c("row_vars", "row_vars_values", "summary_vars", "stat_names"),
              col_labels = c("col_vars", "col_vars_values"), fun = mean)
,"rds/table_summary3.rds"
)

expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_labels = c("row_vars", "row_vars_values", "summary_vars"),
              col_labels = c("col_vars", "col_vars_values", "stat_names"), fun = mean)
,"rds/table_summary4.rds"
)

expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_labels = c("row_vars", "row_vars_values", "stat_names"),
              col_labels = c("col_vars", "col_vars_values", "summary_vars"), fun = mean)
,"rds/table_summary5.rds"
)

expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_labels = c("row_vars", "row_vars_values", "stat_names"),
              col_labels = c("col_vars", "summary_vars", "col_vars_values"), fun = mean)
,"rds/table_summary6.rds"
)


########## rowlabels

expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_vars = mtcars$vs,
              row_labels = c("row_vars", "row_vars_values", "summary_vars", "stat_names"),
              col_labels = c("col_vars", "col_vars_values"), fun = mean)
,"rds/table_summary7.rds"
)


expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_vars = mtcars$vs,
              row_labels = c("row_vars", "row_vars_values", "summary_vars"),
              col_labels = c("col_vars", "col_vars_values", "stat_names"), fun = mean)
,"rds/table_summary8.rds"
)

expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_vars = mtcars$vs,
              row_labels = c("row_vars", "row_vars_values", "stat_names"),
              col_labels = c("col_vars", "col_vars_values", "summary_vars"), fun = mean)
,"rds/table_summary9.rds"
)

expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_vars = mtcars$vs,
              row_labels = c("row_vars", "row_vars_values",  "col_vars_values", "summary_vars", "stat_names"),
              col_labels = c("col_vars"), fun = mean)
,"rds/table_summary10.rds"
)
##############################
add_val_lab(mtcars$vs) = c("Don't know" = 88)

expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_vars = mtcars$vs,
              row_labels = c("row_vars", "row_vars_values", "summary_vars", "stat_names"),
              col_labels = c("col_vars", "col_vars_values"), fun = mean)
,"rds/table_summary11.rds"
)

expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_vars = mtcars$vs,
              row_labels = c("row_vars", "row_vars_values", "summary_vars"),
              col_labels = c("col_vars", "col_vars_values", "stat_names"), fun = mean)
,"rds/table_summary12.rds"
)


expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_vars = mtcars$vs,
              row_labels = c("row_vars", "row_vars_values", "summary_vars", "stat_names"),
              col_labels = c("col_vars", "col_vars_values", "summary_vars"), fun = mean)
,"rds/table_summary13.rds"
)


expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_vars = mtcars$vs,
              row_labels = c("row_vars", "row_vars_values", "stat_names"),
              col_labels = c("col_vars", "col_vars_values", "summary_vars"), fun = mean)
,"rds/table_summary14.rds"
)


expect_equal_to_reference(
table_summary(mtcars %except% c("vs", "am"), col_vars = list("Total", mtcars$am),
              row_vars = mtcars$vs,
              row_labels = c("row_vars", "row_vars_values",  "col_vars_values", "summary_vars", "stat_names"),
              col_labels = c("col_vars"), fun = mean)
,"rds/table_summary15.rds"
)

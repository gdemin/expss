context("output methods print labelled")

x = c(letters, LETTERS)
x = as.labelled(x)

expect_identical(print(x), x)
options(width = 1000)
expect_output_file(print(x), "rds/print_labelled1.txt")
expect_output_file(str(x), "rds/str_labelled1.txt")
var_lab(x) = "Letters"
expect_output_file(print(x), "rds/print_labelled2.txt")
expect_output_file(str(x), "rds/str_labelled2.txt")
expect_output_file(print(x, max = 100), "rds/print_labelled3.txt")
expect_output_file(print(x, max = 100, max_labels = 100), "rds/print_labelled4.txt")
expect_output_file(print(unvl(x)), "rds/print_labelled5.txt")
# x_mat = matrix(x, ncol = 2)
# var_lab(x_mat) = var_lab(x)
# val_lab(x_mat) = val_lab(x)
# expect_output_file(print(x_mat), "rds/print_labelled6.txt")
# expect_output_file(str(x_mat), "rds/str_labelled6.txt")

x = letters[1:10]
x = as.labelled(x)
var_lab(x) = "Letters"
expect_output_file(str(x), "rds/str_labelled7.txt")

#####################################
context("output etable fre methods")

data(mtcars)
# add labels to dataset
mtcars = apply_labels(mtcars, 
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      carb = "Carbureuter",
                      hp = "Gross horsepower",
                      vs = "Engine",
                      vs = num_lab(" 
                                   0 V-engine
                                   1 Straight engine
                                   "),
                      
                      am = "Transmission",
                      am = num_lab(" 
                                   0 Automatic
                                   1 Manual
                                   ")
                      )

tbl = mtcars %>% 
    tab_cols(total(), am %nest% vs) %>%
    tab_cells(cyl) %>% 
    tab_stat_fun(Mean = w_mean, "Std. dev" = w_sd, "Valid N" = w_n) %>% 
    tab_stat_cpct() %>% 
    tab_cells(carb) %>% 
    tab_stat_fun(Mean = w_mean, "Std. dev" = w_sd, "Valid N" = w_n) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() 

options(expss.digits = NA)
options(width = 1000)
expect_output_file(print(tbl[, 1:3]), 
                   "rds/print_etable_unrounded.txt")
expect_output_file(print(tbl[, 1:4], remove_repeated = FALSE),
                   "rds/print_etable_unrounded2.txt")



options(expss.digits = 2)
expect_output_file(print(tbl[, 1:3]), 
                   "rds/print_etable_digits2.txt")
expect_output_file(print(tbl[, 1:4], remove_repeated = FALSE),
                   "rds/print_etable_digits2_2.txt")
expect_output_file(print(tbl[, 1:3], digits = NA), 
                   "rds/print_etable_unrounded.txt")

options(expss.digits = NULL)

expect_output_file(print(tbl[, 1:3]), 
                   "rds/print_etable.txt")
expect_output_file(print(tbl[, 1:4], remove_repeated = FALSE),
                   "rds/print_etable_2.txt")
expect_output_file(print(tbl[, FALSE]), 
                   "rds/print_etable_zero_columns.txt")

expect_output_file(print(tbl[FALSE, ]), 
                   "rds/print_etable_zero_rows.txt")

expect_output_file(print(tbl[FALSE, FALSE]), 
                   "rds/print_etable_zero.txt")

expect_output_file(print(tbl[, 1]), 
                   "rds/print_etable_single_column.txt")


##############################

expss_output_commented()

expect_output_file(print(tbl),
                   "rds/print_etable_commented_1.txt")

expect_output_file(print(tbl[, 1:3]), 
                   "rds/print_etable_commented_2.txt")
expect_output_file(print(tbl[, 1:4], remove_repeated = FALSE),
                   "rds/print_etable_commented_3.txt")
expect_output_file(print(tbl[, FALSE]), 
                   "rds/print_etable_zero_columns_commented.txt")

expect_output_file(print(tbl[FALSE, ]), 
                   "rds/print_etable_zero_rows_commented.txt")

expect_output_file(print(tbl[FALSE, FALSE]), 
                   "rds/print_etable_zero_commented.txt")

expss_output_default()

################################
colnames(tbl)[1] = "my custom label"
expect_output_file(print(tbl[, 1:3]), 
                   "rds/print_etable_custom_label.txt")

#################################

options(expss.digits = 4)
expect_output_file(print(cro_mean(iris[,-5], list(iris$Species, total()))), 
                   "rds/cro_mean_out.txt")
options(expss.digits = NULL)
options(expss.output = "rnotebook")
expect_output_file(print(tbl), 
                   "rds/print_etable_rnotebook.txt")

options(expss.output = "viewer")
aa = capture_output(
    expect_identical(print(tbl), NULL)
)

options(expss.output = "raw")
expect_output_file(print(tbl[, 1:2]), 
                   "rds/print_etable_raw.txt")

options(expss.output = NULL)

res = expss:::knit_print.etable(tbl)
expect_equal_to_reference(res, "rds/knit_print.rds")

options(expss.output = "rnotebook")
colnames(tbl) = enc2utf8(colnames(tbl))
tbl[[1]] = enc2utf8(tbl[[1]])
expect_output_file(print(tbl), 
                   "rds/print_etable_rnotebook.txt")

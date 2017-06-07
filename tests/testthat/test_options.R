context("options")
data(mtcars)
data(iris)
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

expss_output_default()
expss_digits(NA)
options(width = 1000)
expect_output_file(print(tbl[, 1:3]), 
                   "rds/print_etable_unrounded.txt")
expect_output_file(print(tbl[, 1:4], remove_repeated = FALSE),
                   "rds/print_etable_unrounded2.txt")



expss_digits(2)
expect_output_file(print(tbl[, 1:3]), 
                   "rds/print_etable_digits2.txt")
expect_output_file(print(tbl[, 1:4], remove_repeated = FALSE),
                   "rds/print_etable_digits2_2.txt")
expect_output_file(print(tbl[, 1:3], digits = NA), 
                   "rds/print_etable_unrounded.txt")

expss_digits()

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

colnames(tbl)[1] = "my custom label"
expect_output_file(print(tbl[, 1:3]), 
                   "rds/print_etable_custom_label.txt")

expss_digits(4)
expect_output_file(print(cro_mean(iris[,-5], list(iris$Species, total()))), 
                   "rds/cro_mean_out.txt")
expss_digits()
expss_output_rnotebook()
expect_output_file(print(tbl), 
                   "rds/print_etable_rnotebook.txt")

expss_output_viewer()
aa = capture_output(
    expect_identical(print(tbl), NULL)
)

expss_output_raw()
expect_output_file(print(tbl[, 1:2]), 
                   "rds/print_etable_raw.txt")


expss_output_default()
res = expss:::knit_print.etable(tbl)
expect_equal_to_reference(res, "rds/knit_print.rds")

expss_output_rnotebook()
colnames(tbl) = enc2utf8(colnames(tbl))
tbl[[1]] = enc2utf8(tbl[[1]])
expect_output_file(print(tbl), 
                   "rds/print_etable_rnotebook.txt")

expss_output_default()

a = 1:0
a_str = as.character(a)
a_log = c(TRUE, FALSE)
var_lab(a) = "Lab"
var_lab(a_str) = "Lab"
var_lab(a_log) = "Lab"
val_lab(a) = c("Lab" = 1)
val_lab(a_str) = c("Lab" = 1)
val_lab(a_log) = c("Lab" = 1)

a_integer = a
storage.mode(a_integer) = "integer"
class(a_integer) = c("labelled", "integer")

a_numeric = a + 0.5 - 0.5
class(a_numeric) = c("labelled", "numeric")
expect_identical(as.numeric(a_str), a_numeric)
expect_identical(as.integer(a_str), a_integer)

expect_identical(as.logical(a), unvl(a_log))
expect_identical(as.integer(a_log), a_integer)

# options(expss.enable_value_labels_support = 0)
expss_disable_value_labels_support()
expect_identical(as.character(a), unvl(a_str))
expss_enable_value_labels_support()
expect_identical(as.character(a), c("Lab", "0"))
expect_identical(as.character(a, prepend_varlab = TRUE), c("Lab|Lab", "Lab|0"))


aa = 1:3
val_lab(aa) = c(c = 1, b = 2, a = 3)

expect_identical(factor(aa), factor(1:3, levels = 1:3, labels = c("c", "b", "a")))

a = c(1, 1, 0, NA)
var_lab(a) = "This is a"
val_lab(a) = c("a" = 1, b = 0)

expect_identical(unique(a), a[-1])

# options(expss.enable_value_labels_support = 0)
expss_disable_value_labels_support()
expect_identical(unique(a), c(1, 0, NA))

expss_enable_value_labels_support()
expect_identical(unique(a), a[-1])
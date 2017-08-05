context("custom tables significance tests")

data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (lb/1000)",
                      qsec = "1/4 mile time",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)


res = mtcars %>% tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>%
    tab_last_cpct_significance(keep = "none") %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct1.rds")


res = mtcars %>% 
    tab_significance_options(keep = "none") %>% 
    tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>%
    tab_last_cpct_significance() %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct1.rds")

res = mtcars %>% 
    tab_significance_options(keep = "none") %>% 
    tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>% 
    tab_last_cpct_significance(inequality_sign = TRUE) %>%
    tab_pivot()


expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct1_greater_sign.rds")


res = mtcars %>% 
    tab_significance_options(subtable_marks = "both") %>% 
    tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>% 
    tab_last_cpct_significance() %>%
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct2.rds")

res = mtcars %>% 
    tab_significance_options(subtable_marks = "both") %>% 
    tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>% 
    tab_last_cpct_significance(inequality_sign = FALSE) %>%
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct3.rds")

res = mtcars %>% 
    tab_significance_options(subtable_marks = "greater") %>% 
    tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>% 
    tab_last_cpct_significance(subtable_marks = "both") %>%
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct4.rds")

res = mtcars %>% 
    tab_significance_options(subtable_marks = "both") %>% 
    tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>% 
    tab_last_cpct_significance() %>%
    tab_significance_options() %>% 
    tab_stat_cpct() %>% 
    tab_last_cpct_significance() %>%
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct5.rds")

res = mtcars %>% tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct(label = "%") %>% 
    tab_last_cpct_significance() %>%
    tab_pivot(stat_position = "inside_columns")

expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct6.rds")


res = mtcars %>% tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>%
    tab_last_add_sig_labels() %>% 
    tab_last_cpct_significance(keep = "none", sig_labels = NULL, mode = "append") %>%
    tab_pivot(stat_position = "inside_columns")


expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct7.rds")

res = mtcars %>% tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>%
    tab_last_add_sig_labels() %>% 
    tab_last_round() %>% 
    tab_last_cpct_significance(keep = "none", sig_labels = NULL, mode = "append") %>%
    tab_pivot(stat_position = "inside_rows")


expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct8.rds")

res = mtcars %>% 
    tab_total_row_position("above") %>% 
    tab_total_label("BASE") %>% 
    tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>% 
    tab_last_cpct_significance() %>%
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct9.rds")

res = mtcars %>% 
    tab_cells(cyl, gear) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>% 
    tab_last_cpct_significance(keep = "percent") %>%
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ct_signif_cpct10.rds")
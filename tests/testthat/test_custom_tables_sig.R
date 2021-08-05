if(isTRUE(getOption("covr"))){ 

    
    context("custom tables significance cpct")
    
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
    
    
    res = mtcars %>% 
        tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>%
        tab_last_sig_cpct(keep = "none") %>% 
        tab_pivot()
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct1.rds",  update = FALSE)
    
    
    res = mtcars %>% 
        tab_significance_options(keep = "none") %>% 
        tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>%
        tab_last_sig_cpct() %>% 
        tab_pivot()
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct1.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_significance_options(keep = "none") %>% 
        tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>% 
        tab_last_sig_cpct(inequality_sign = TRUE) %>%
        tab_pivot()
    
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct1_greater_sign.rds",  update = FALSE)
    
    
    res = mtcars %>% 
        tab_significance_options(subtable_marks = "both") %>% 
        tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>% 
        tab_last_sig_cpct() %>%
        tab_pivot()
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct2.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_significance_options(subtable_marks = "both") %>% 
        tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>% 
        tab_last_sig_cpct(inequality_sign = FALSE) %>%
        tab_pivot()
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct3.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_significance_options(subtable_marks = "greater") %>% 
        tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>% 
        tab_last_sig_cpct(subtable_marks = "both") %>%
        tab_pivot()
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct4.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_significance_options(subtable_marks = "both") %>% 
        tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>% 
        tab_last_sig_cpct() %>%
        tab_significance_options() %>% 
        tab_stat_cpct() %>% 
        tab_last_sig_cpct() %>%
        tab_pivot()
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct5.rds",  update = FALSE)
    
    res = mtcars %>% tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct(label = "%") %>% 
        tab_last_sig_cpct() %>%
        tab_pivot(stat_position = "inside_columns")
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct6.rds",  update = FALSE)
    
    
    res = mtcars %>% tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>%
        tab_last_add_sig_labels() %>% 
        tab_last_sig_cpct(keep = "none", sig_labels = NULL, mode = "append") %>%
        tab_pivot(stat_position = "inside_columns")
    
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct7.rds",  update = FALSE)
    
    res = mtcars %>% tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>%
        tab_last_add_sig_labels() %>% 
        tab_last_round() %>% 
        tab_last_sig_cpct(keep = "none", sig_labels = NULL, mode = "append") %>%
        tab_pivot(stat_position = "inside_rows")
    
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct8.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_total_row_position("above") %>% 
        tab_total_label("BASE") %>% 
        tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>% 
        tab_last_sig_cpct() %>%
        tab_pivot()
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct9.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_cells(cyl, gear) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>% 
        tab_last_sig_cpct(keep = "percent") %>%
        tab_pivot()
    
    expect_equal_to_reference(
        res,
        "rds/ct_signif_cpct10.rds",  update = FALSE)
    
    
    context("custom tables significance means")
    
    mtcars_table = cro_mean_sd_n(list(mtcars$mpg, mtcars$hp),
                                 list(total(), mtcars$vs, mtcars$am))
    
    
    res = mtcars %>% 
        tab_cells(mpg, hp) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_mean_sd_n() %>% 
        tab_last_sig_means(compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable"),
                           bonferroni = TRUE,
                           sig_level = 0.05) %>% 
        tab_pivot()
    
    expect_equal_to_reference(res, 
                              "rds/signif_means20bonferroni.rds",  update = FALSE
    )
    
    res = mtcars %>% 
        tab_significance_options(compare_type = c("first_column", 
                                                  "previous_column",
                                                  "subtable"),
                                 bonferroni = TRUE,
                                 sig_level = 0.05) %>% 
        tab_cells(mpg, hp) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_mean_sd_n() %>% 
        tab_last_sig_means() %>% 
        tab_pivot()
    
    expect_equal_to_reference(res, 
                              "rds/signif_means20bonferroni.rds",  update = FALSE
    )
    
    res = mtcars %>% 
        let(
            weight = 2
        ) %>% 
        tab_significance_options(compare_type = c("first_column", 
                                                  "previous_column",
                                                  "subtable")
        ) %>% 
        tab_cells(mpg, hp) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_weight(weight) %>% 
        tab_stat_mean_sd_n(weighted_valid_n = TRUE) %>% 
        tab_last_sig_means(subtable_marks = "both") %>% 
        tab_pivot()
    
    expect_equal_to_reference(res, 
                              "rds/ct_signif_means1.rds",  update = FALSE
    )
    
    res = mtcars %>% 
        tab_significance_options(compare_type = c("first_column", 
                                                  "previous_column",
                                                  "subtable"),
                                 bonferroni = TRUE,
                                 sig_level = 0.05) %>% 
        tab_cells(mpg, hp) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_mean_sd_n(labels = c("mean", "std", "N=")) %>% 
        tab_last_sig_means() %>% 
        tab_cells(cyl) %>% 
        tab_stat_cpct() %>%
        tab_last_sig_cpct() %>% 
        tab_stat_cases() %>% 
        tab_last_sig_cases(keep = "none") %>% 
        tab_last_add_sig_labels() %>% 
        tab_cells(gear) %>% 
        tab_stat_cpct() %>%
        tab_last_sig_cpct() %>% 
        tab_stat_cases() %>% 
        tab_last_sig_cases(keep = "none") %>% 
        tab_last_add_sig_labels() %>% 
        tab_pivot(stat_position = "inside_rows")
    
    expect_equal_to_reference(res, 
                              "rds/ct_signif_many1.rds",  update = FALSE
    )
    
    tab_many_sig = . %>% tab_stat_cpct() %>%
        tab_last_sig_cpct() %>% 
        tab_stat_cases() %>% 
        tab_last_sig_cases(keep = "none") %>% 
        tab_last_add_sig_labels() %>% 
        tab_last_vstack()
    
    res = mtcars %>% 
        tab_significance_options(compare_type = c("first_column", 
                                                  "previous_column",
                                                  "subtable"),
                                 bonferroni = TRUE,
                                 sig_level = 0.05) %>% 
        tab_cells(mpg, hp) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_mean_sd_n(labels = c("mean", "std", "N=")) %>% 
        tab_last_sig_means() %>% 
        tab_cells(cyl) %>% 
        tab_many_sig() %>% 
        tab_cells(gear) %>% 
        tab_many_sig() %>%
        tab_pivot(stat_position = "inside_rows")
    
    expect_equal_to_reference(res, 
                              "rds/ct_signif_many1.rds",  update = FALSE
    )
    
    tab_sig_means_left = . %>% 
        tab_stat_mean_sd_n() %>% 
        tab_last_add_sig_labels() %>%
        tab_last_sig_means(keep = "none", sig_labels = NULL, mode = "append") %>% 
        tab_last_hstack("inside_columns")
    
    tab_sig_cpct_left = . %>% 
        tab_stat_cpct() %>% 
        tab_last_add_sig_labels() %>%
        tab_last_sig_cpct(keep = "none", sig_labels = NULL, mode = "append") %>% 
        tab_last_hstack("inside_columns")
    
    res = mtcars %>% 
        tab_significance_options(compare_type = c("first_column", 
                                                  "previous_column",
                                                  "subtable"),
                                 subtable_marks = "both",
                                 sig_level = 0.05) %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(mpg, hp) %>% 
        tab_sig_means_left() %>% 
        tab_cells(cyl, gear) %>% 
        tab_sig_cpct_left() %>% 
        tab_pivot(stat_position = "inside_rows") %>% 
        drop_empty_columns()
    
    expect_equal_to_reference(res,
                              "rds/ct_signif_many2.rds",  update = FALSE
    )
    
    res = mtcars %>% 
        tab_significance_options(keep = "none", sig_labels = NULL, 
                                 subtable_marks = "both",  mode = "append") %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(mpg, hp) %>% 
        tab_stat_mean_sd_n() %>% 
        tab_last_sig_means() %>% 
        tab_last_hstack("inside_columns") %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_last_sig_cpct() %>% 
        tab_last_hstack("inside_columns") %>% 
        tab_pivot(stat_position = "inside_rows") %>% 
        drop_empty_columns()
    
    
    expect_equal_to_reference(res, 
                              "rds/ct_signif_many3.rds",  update = FALSE
    )
    
}
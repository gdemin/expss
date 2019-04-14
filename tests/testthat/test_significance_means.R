test_that("significance_means", { 
    skip_on_cran()
    
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
    
    
    context("significance_means")
    
    mtcars_table = cro_mean_sd_n(list(mtcars$mpg, mtcars$hp),
                                 list(total(), mtcars$vs, mtcars$am))
    
    
    
    expect_equal_to_reference(
        significance_means(mtcars_table),
        "rds/signif_means1.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, subtable_marks = "less"),
        "rds/signif_means1_less.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, subtable_marks = "both"),
        "rds/signif_means1_both.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, sig_level = 1e-3),
        "rds/signif_means1b.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, keep = "none"),
        "rds/signif_means2.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_means(mtcars_table,
                           keep = "none",
                           sig_labels = paste0("(",letters, ")")),
        "rds/signif_means3.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_means(mtcars_table,
                           keep = "none",
                           sig_labels = NULL),
        "rds/signif_means4.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, keep = "none",
                           compare_type = c("subtable", "first_column"),
                           sig_level = 0.4
        ),
        
        "rds/signif_means5.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_means(mtcars_table, keep = "none",
                           compare_type = c("subtable", "adjusted_first_column"),
                           sig_level = 0.4),
        "rds/signif_means6.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_means(mtcars_table, keep = "none",
                           compare_type = c("subtable", "first_column"),
                           sig_level = 0.4,
                           var_equal = TRUE
        ),
        "rds/signif_means7.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, keep = "none",
                           compare_type = c("previous_column"),
                           sig_level = 0.2
        ),
        "rds/signif_means8.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, keep = "none",
                           compare_type = c("previous_column"),
                           sig_level = 0.2,
                           var_equal = TRUE),
        "rds/signif_means9.rds",  update = FALSE)
    
    #####################
    expect_equal_to_reference(
        significance_means(mtcars_table, keep = "bases"),
        "rds/signif_means10.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, keep = "sd"),
        "rds/signif_means10sd.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, keep = c("sd", "bases")),
        "rds/signif_means10sd_bases.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table),
        "rds/signif_means1.rds",  update = FALSE)
    
    
    expect_equal_to_reference(
        significance_means(mtcars_table, delta_means = 10),
        "rds/signif_means11_delta.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, keep = "means"),
        "rds/signif_means12.rds",  update = FALSE)
    
    
    
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           compare_type = c("subtable", "first_column")),
        "rds/signif_means13.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           compare_type = c("subtable", "adjusted_first_column")),
        "rds/signif_means14.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_means(mtcars_table,
                           compare_type = c("subtable", "adjusted_first_column"),
                           var_equal = TRUE
        ),
        "rds/signif_means15.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           compare_type = c("previous_column")),
        "rds/signif_means16.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           delta_means = 10,
                           compare_type = c("previous_column")),
        "rds/signif_means16_delta.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           delta_means = 10,
                           var_equal = TRUE,
                           compare_type = c("previous_column")),
        "rds/signif_means16_delta2.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           compare_type = c("previous_column"), 
                           var_equal = TRUE),
        "rds/signif_means17.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           compare_type = c("previous_column"), 
                           var_equal = TRUE,
                           min_base = 14),
        "rds/signif_means18.rds",  update = FALSE)
    
    
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           sig_level = 0.9),
        "rds/signif_means19.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable"),
                           bonferroni = FALSE,
                           sig_level = 0.05),
        "rds/signif_means20.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable"),
                           bonferroni = TRUE,
                           sig_level = 0.05),
        "rds/signif_means20bonferroni.rds",  update = FALSE)
    
    
    mtcars_table2 = cro_mean_sd_n(list(mtcars$mpg, mtcars$hp),
                                  list(total(), mtcars$cyl))
    
    expect_equal_to_reference(
        significance_means(mtcars_table2, 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable"),
                           bonferroni = FALSE,
                           sig_level = 0.0005),
        "rds/signif_means20_2.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table2, 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable"),
                           subtable_marks = "greater",
                           sig_level = 0.05),
        "rds/signif_means20_2_greater.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table2, 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable"),
                           inequality_sign = TRUE,
                           subtable_marks = "greater",
                           sig_level = 0.05),
        "rds/signif_means20_2_greater_sign.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table2, 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable"),
                           subtable_marks = "less",
                           sig_level = 0.05),
        "rds/signif_means20_2_less.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table2, 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable"),
                           inequality_sign = TRUE,
                           subtable_marks = "less",
                           sig_level = 0.05),
        "rds/signif_means20_2_less_sign.rds",  update = FALSE)
    
    
    expect_equal_to_reference(
        significance_means(mtcars_table2, 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable"),
                           subtable_marks = "both",
                           sig_level = 0.05),
        "rds/signif_means20_2_both.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table2, 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable"),
                           inequality_sign = FALSE,
                           subtable_marks = "both",
                           sig_level = 0.05),
        "rds/signif_means20_2_both_no_sign.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table2, 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable"),
                           bonferroni = TRUE,
                           sig_level = 0.0005),
        "rds/signif_means20bonferroni_2.rds",  update = FALSE)
    
    
    ############################################
    
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           compare_type = c("subtable", "first_column"), 
                           digits = 2),
        "rds/signif_means21.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table, 
                           compare_type = c("subtable", "first_column"), 
                           delta_means = 10,
                           digits = 2),
        "rds/signif_means21_delta.rds",  update = FALSE)
    
    expect_error(
        significance_means(mtcars_table[1:5,], 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable")))
    
    
    
    
    mtcars_table3 = cro_mean_sd_n(list(mtcars$mpg, mtcars$hp),
                                  list(total(), mtcars$vs, mtcars$am), weighted_valid_n = TRUE,
                                  weight = 1/100)
    
    expect_equal_to_reference(
        significance_means(mtcars_table3, 
                           compare_type = c("first_column", 
                                            "previous_column",
                                            "subtable")),
        "rds/signif_means23.rds",  update = FALSE)
    
    
    
    
    expect_equal_to_reference(
        significance_means(mtcars_table, keep = "none", sig_labels = "a"),
        "rds/signif_means25.rds",  update = FALSE)
    
    
    expect_equal_to_reference(
        significance_means(mtcars_table,
                           compare_type = c("previous_column", "subtable"),
                           sig_labels = "a"),
        "rds/signif_means26.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table,
                           compare_type = c("previous_column", "subtable"),
                           sig_labels = NULL),
        "rds/signif_means26a.rds",  update = FALSE)
    
    
    
    mtcars_table2 = mtcars_table
    
    colnames(mtcars_table2) = rep("", NCOL(mtcars_table3))
    
    expect_equal_to_reference(
        significance_means(mtcars_table2,
                           compare_type = c("previous_column", "subtable")),
        "rds/signif_means29.rds",  update = FALSE)
    
    expect_error(
        significance_means(mtcars_table2,
                           compare_type = c("first_column", "adjusted_first_column"))
    )
    
    
    
    expect_equal_to_reference(
        significance_means(mtcars_table[ ,1],
                           compare_type = c("first_column", "subtable")),
        "rds/signif_means30.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table[ ,1:2],
                           compare_type = c("first_column", "subtable")),
        "rds/signif_means31.rds",  update = FALSE)
    
    
    expect_equal_to_reference(
        significance_means(mtcars_table[ ,1:3],
                           compare_type = c("first_column", "subtable")),
        "rds/signif_means32.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table3[ ,c(1:2,4,5)],
                           compare_type = c("first_column", "subtable")),
        "rds/signif_means33.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table[ ,1],
                           keep = "none"
        ),
        "rds/signif_means34.rds",  update = FALSE)
    
    
    expect_equal_to_reference(
        significance_means(mtcars_table[ ,1],
                           keep = "means"
        ),
        "rds/signif_means34.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_means(mtcars_table[ ,1],
                           keep = c("means", "sd")
        ),
        "rds/signif_means35.rds",  update = FALSE)
    
    
    expect_equal_to_reference(
        significance_means(mtcars_table[ ,1],
                           keep ="bases"
        ),
        "rds/signif_means36.rds",  update = FALSE)
    
})
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
    "rds/signif_means1.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, sig_level = 1e-3),
    "rds/signif_means1b.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = FALSE),
    "rds/signif_means2.rds")
expect_equal_to_reference(
    significance_means(mtcars_table,
                       keep_means = FALSE,
                      sig_labels = paste0("(",letters, ")")),
    "rds/signif_means3.rds")
expect_equal_to_reference(
    significance_means(mtcars_table,
                       keep_means = FALSE,
                      sig_labels = NULL),
    "rds/signif_means4.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = FALSE,
                      compare_type = c("subtable", "first_column"),
                      sig_level = 0.4
                      ),
    
    "rds/signif_means5.rds")
expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = FALSE,
                      compare_type = c("subtable", "adjusted_first_column"),
                      sig_level = 0.4),
    "rds/signif_means6.rds")
expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = FALSE,
                      compare_type = c("subtable", "first_column"),
                      sig_level = 0.4,
                      var_equal = TRUE
    ),
    "rds/signif_means7.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = FALSE
                      , compare_type = c("previous_column"),
                      sig_level = 0.2
                      ),
    "rds/signif_means8.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = FALSE,
                      compare_type = c("previous_column"),
                      sig_level = 0.2,
                      var_equal = TRUE),
    "rds/signif_means9.rds")

#####################
expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = FALSE, keep_bases = TRUE),
    "rds/signif_means10.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = FALSE, keep_sd = TRUE),
    "rds/signif_means10sd.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = FALSE, keep_sd = TRUE, keep_bases = TRUE),
    "rds/signif_means10sd_bases.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = TRUE),
    "rds/signif_means1.rds")


expect_equal_to_reference(
    significance_means(mtcars_table, delta_means = 10),
    "rds/signif_means11_delta.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = TRUE, keep_sd = FALSE, keep_bases = FALSE),
    "rds/signif_means12.rds")



expect_equal_to_reference(
    significance_means(mtcars_table, 
                      compare_type = c("subtable", "first_column"), 
                      keep_means = TRUE),
    "rds/signif_means13.rds")
expect_equal_to_reference(
    significance_means(mtcars_table, 
                      compare_type = c("subtable", "adjusted_first_column"), 
                      
                      keep_means = TRUE),
    "rds/signif_means14.rds")
expect_equal_to_reference(
    significance_means(mtcars_table,
                      compare_type = c("subtable", "adjusted_first_column"),
                      var_equal = TRUE, 
                      keep_means = TRUE
    ),
    "rds/signif_means15.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, 
                      compare_type = c("previous_column"), 
                      keep_means = TRUE),
    "rds/signif_means16.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, 
                      delta_means = 10,
                       compare_type = c("previous_column"), 
                      keep_means = TRUE),
    "rds/signif_means16_delta.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, 
                       delta_means = 10,
                      var_equal = TRUE,
                      compare_type = c("previous_column"), 
                      keep_means = TRUE),
    "rds/signif_means16_delta2.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, 
                      compare_type = c("previous_column"), 
                      var_equal = TRUE, 
                      keep_means = TRUE),
    "rds/signif_means17.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, 
                      compare_type = c("previous_column"), 
                      var_equal = TRUE, 
                      keep_means = TRUE,
                      min_base = 14),
    "rds/signif_means18.rds")


expect_equal_to_reference(
    significance_means(mtcars_table, 
                                       sig_level = 0.9),
    "rds/signif_means19.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, 
                      compare_type = c("first_column", 
                                       "previous_column",
                                       "subtable"),
                      bonferroni = FALSE,
                      keep_means = TRUE,
                      sig_level = 0.05),
    "rds/signif_means20.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, 
                       compare_type = c("first_column", 
                                        "previous_column",
                                        "subtable"),
                       bonferroni = TRUE,
                       keep_means = TRUE,
                       sig_level = 0.05),
    "rds/signif_means20bonferroni.rds")


############################################

expect_equal_to_reference(
    significance_means(mtcars_table, 
                      compare_type = c("subtable", "first_column"), 
                      keep_means = TRUE,
                      digits = 2),
    "rds/signif_means21.rds")

expect_equal_to_reference(
    significance_means(mtcars_table, 
                      compare_type = c("subtable", "first_column"), 
                      keep_means = TRUE,
                      delta_means = 10,
                      digits = 2),
    "rds/signif_means21_delta.rds")

expect_error(
    significance_means(mtcars_table[1:5,], 
                      compare_type = c("first_column", 
                                       "previous_column",
                                       "subtable"), 
                      keep_means = TRUE))




mtcars_table3 = cro_mean_sd_n(list(mtcars$mpg, mtcars$hp),
                              list(total(), mtcars$vs, mtcars$am), weighted_valid_n = TRUE,
                         weight = 1/100)

expect_equal_to_reference(
    significance_means(mtcars_table3, 
                      compare_type = c("first_column", 
                                       "previous_column",
                                       "subtable"), 
                      keep_means = TRUE),
    "rds/signif_means23.rds")




expect_equal_to_reference(
    significance_means(mtcars_table, keep_means = FALSE, sig_labels = "a"),
    "rds/signif_means25.rds")


expect_equal_to_reference(
    significance_means(mtcars_table,
                      compare_type = c("previous_column", "subtable"),
                      keep_means = TRUE, 
                      sig_labels = "a"),
    "rds/signif_means26.rds")

expect_equal_to_reference(
    significance_means(mtcars_table,
                      compare_type = c("previous_column", "subtable"),
                      keep_means = TRUE, 
                      sig_labels = NULL),
    "rds/signif_means26a.rds")



mtcars_table2 = mtcars_table

colnames(mtcars_table2) = rep("", NCOL(mtcars_table3))

expect_equal_to_reference(
    significance_means(mtcars_table2,
                      compare_type = c("previous_column", "subtable"),
                      keep_means = TRUE),
    "rds/signif_means29.rds")

expect_error(
    significance_means(mtcars_table2,
                      compare_type = c("first_column", "adjusted_first_column"),
                      keep_means = TRUE)
)



expect_equal_to_reference(
    significance_means(mtcars_table[ ,1],
                      compare_type = c("first_column", "subtable"),
                      keep_means = TRUE),
    "rds/signif_means30.rds")

expect_equal_to_reference(
    significance_means(mtcars_table[ ,1:2],
                      compare_type = c("first_column", "subtable"),
                      keep_means = TRUE),
    "rds/signif_means31.rds")


expect_equal_to_reference(
    significance_means(mtcars_table[ ,1:3],
                      compare_type = c("first_column", "subtable"),
                      keep_means = TRUE),
    "rds/signif_means32.rds")

expect_equal_to_reference(
    significance_means(mtcars_table3[ ,c(1:2,4,5)],
                      compare_type = c("first_column", "subtable"),
                      keep_means = TRUE),
    "rds/signif_means33.rds")
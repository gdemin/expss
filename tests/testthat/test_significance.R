context("significance tests")

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

tbl = t(table(mtcars$am, mtcars$vs))

expect_equal(compare_proportions(12/18, 7/14, 18, 14),
             prop.test(tbl, correct = FALSE)$p.value)

tbl[2,1] = 0

expect_equal(compare_proportions(12/18, 0/7, 18, 7),
             suppressWarnings(prop.test(tbl, correct = FALSE))$p.value)

t_test_pval = t.test(mpg ~ am, data = mtcars)$p.value 

res = with(mtcars, 
     compare_means(
         mean(mpg[am==0]), mean(mpg[am==1]), 
         sd(mpg[am==0]),  sd(mpg[am==1]),
         length(mpg[am==0]), length(mpg[am==1])
     ))

expect_equal(res, t_test_pval)


res = with(mtcars, 
           compare_means(
               c(mean(mpg[am==0]), mean(mpg[am==0])), c(mean(mpg[am==1]), mean(mpg[am==1])), 
               c(sd(mpg[am==0]), sd(mpg[am==0])),  c(sd(mpg[am==1]), sd(mpg[am==1])),
               c(length(mpg[am==0]), length(mpg[am==0])), c(length(mpg[am==1]), length(mpg[am==1]))
           ))

expect_equal(res, c(t_test_pval, t_test_pval))

t_test_pval = t.test(mpg ~ am, data = mtcars, var.equal = TRUE)$p.value 

res = with(mtcars, 
           compare_means(
               c(mean(mpg[am==0]), mean(mpg[am==0])), c(mean(mpg[am==1]), mean(mpg[am==1])), 
               c(sd(mpg[am==0]), sd(mpg[am==0])),  c(sd(mpg[am==1]), sd(mpg[am==1])),
               c(length(mpg[am==0]), length(mpg[am==0])), c(length(mpg[am==1]), length(mpg[am==1])),
               var_equal = TRUE
           ))

expect_equal(res, c(t_test_pval, t_test_pval))

context("significance_cpct")

mtcars_table = cro_cpct(list(mtcars$cyl, mtcars$gear),
                        list(total(), mtcars$vs, mtcars$am))



expect_equal_to_reference(
    significance_cpct(mtcars_table, keep_percent = FALSE),
    "rds/signif_cpct1.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table, keep_percent = FALSE, na_as_zero = TRUE),
    "rds/signif_cpct2.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table,
                      keep_percent = FALSE,
                      sig_labels = paste0("(",letters, ")")),
    "rds/signif_cpct3.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table,
                      keep_percent = FALSE,
                      sig_labels = NULL),
    "rds/signif_cpct4.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table, keep_percent = FALSE,
                      compare_type = c("subtable", "first_column")),
    "rds/signif_cpct5.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table, keep_percent = FALSE,
                      compare_type = c("subtable", "adjusted_first_column")),
    "rds/signif_cpct6.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table, keep_percent = FALSE,
                      compare_type = c("subtable", "adjusted_first_column"),
                      na_as_zero = TRUE
                      ),
    "rds/signif_cpct7.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, keep_percent = FALSE
                      , compare_type = c("previous_column")),
    "rds/signif_cpct8.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, keep_percent = FALSE,
                      compare_type = c("previous_column"),
                      na_as_zero = TRUE),
    "rds/signif_cpct9.rds")

#####################
expect_equal_to_reference(
    significance_cpct(mtcars_table, keep_percent = FALSE, keep_bases = TRUE),
    "rds/signif_cpct10.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, keep_percent = TRUE),
    "rds/signif_cpct11.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table),
    "rds/signif_cpct11.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, delta_cpct = 50),
    "rds/signif_cpct11_delta.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, keep_percent = TRUE, keep_bases = FALSE),
    "rds/signif_cpct12.rds")



expect_equal_to_reference(
    significance_cpct(mtcars_table, 
                      compare_type = c("subtable", "first_column"), 
                      keep_percent = TRUE),
    "rds/signif_cpct13.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table, 
                      compare_type = c("subtable", "adjusted_first_column"), 
                      keep_percent = TRUE),
    "rds/signif_cpct14.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table,
                      compare_type = c("subtable", "adjusted_first_column"),
                      na_as_zero = TRUE, 
                      keep_percent = TRUE
    ),
    "rds/signif_cpct15.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, 
                      compare_type = c("previous_column"), 
                      keep_percent = TRUE),
    "rds/signif_cpct16.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, 
                      delta_cpct = 65,
                      compare_type = c("previous_column"), 
                      keep_percent = TRUE),
    "rds/signif_cpct16_delta.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, 
                      delta_cpct = 65,
                      na_as_zero = TRUE,
                      compare_type = c("previous_column"), 
                      keep_percent = TRUE),
    "rds/signif_cpct16_delta2.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, 
                      compare_type = c("previous_column"), 
                      na_as_zero = TRUE, 
                      keep_percent = TRUE),
    "rds/signif_cpct17.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, 
                      compare_type = c("previous_column"), 
                      na_as_zero = TRUE, 
                      keep_percent = TRUE,
                      min_base = 14),
    "rds/signif_cpct18.rds")


expect_equal_to_reference(
    significance_cpct(mtcars_table, 
                      na_as_zero = TRUE, 
                      keep_percent = TRUE,
                      sig_level = 0.9),
    "rds/signif_cpct19.rds")

#####################
mtcars_table2 = cro_cpct(list(mtcars$vs, mtcars$am),
                        list(total(), mtcars$cyl))

expect_equal_to_reference(
    significance_cpct(mtcars_table2, 
                      compare_type = c("first_column", 
                                       "previous_column",
                                       "subtable"),
                      bonferroni = FALSE,
                      na_as_zero = FALSE, 
                      keep_percent = TRUE,
                      sig_level = 0.1),
    "rds/signif_cpct20.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table2, 
                      compare_type = c("first_column", 
                                       "previous_column",
                                       "subtable"),
                      bonferroni = TRUE,
                      na_as_zero = FALSE, 
                      keep_percent = TRUE,
                      sig_level = 0.1),
    "rds/signif_cpct20boneferrony.rds")

#####################

expect_equal_to_reference(
    significance_cpct(mtcars_table, 
                      compare_type = c("subtable", "first_column"), 
                      keep_percent = TRUE,
                      digits = 2),
    "rds/signif_cpct21.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, 
                      compare_type = c("subtable", "first_column"), 
                      keep_percent = TRUE,
                      delta_cpct = 30,
                      digits = 2),
    "rds/signif_cpct21_delta.rds")

expect_error(
    significance_cpct(mtcars_table, 
                      compare_type = c("first_column", 
                                       "previous_column",
                                       "subtable"), 
                      keep_percent = TRUE,
                      total_marker = "@"))

mtcars_table2 = mtcars_table
mtcars_table2[[1]] = gsub("#", "@", mtcars_table2[[1]])
expect_equal_to_reference(
    significance_cpct(mtcars_table2, 
                      compare_type = c("first_column", 
                                       "previous_column",
                                       "subtable"), 
                      keep_percent = TRUE,
                      total_marker = "@"),
    "rds/signif_cpct22.rds")

expect_error(
significance_cpct(mtcars_table2, 
                  compare_type = c("first_column", 
                                   "previous_column",
                                   "subtable"), 
                  keep_percent = TRUE,
                  total_marker = "@",
                  total_row = 2))

mtcars_table3 = cro_cpct(list(mtcars$cyl, mtcars$gear),
                        list(total(), mtcars$vs, mtcars$am),
                        weight = 1/100,
                        total_row_position = "above",
                        total_statistic = c("w_cases", "u_cases")
                        )

expect_equal_to_reference(
significance_cpct(mtcars_table3, 
                  compare_type = c("first_column", 
                                   "previous_column",
                                   "subtable"), 
                  keep_percent = TRUE),
"rds/signif_cpct23.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table3, 
                      compare_type = c("first_column", 
                                       "previous_column",
                                       "subtable"), 
                      total_row = 2,
                      keep_percent = TRUE),
    "rds/signif_cpct24.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table3, 
                      compare_type = c("first_column", 
                                       "previous_column",
                                       "subtable"), 
                      total_row = "#Total cases",
                      keep_percent = TRUE),
    "rds/signif_cpct24.rds")

mtcars_table3 = cro_cpct(list(mtcars$cyl, mtcars$gear),
                         list(total(), mtcars$vs, mtcars$am),
                         total_row_position = "above"
)

expect_equal_to_reference(
    significance_cpct(mtcars_table3, keep_percent = FALSE),
    "rds/signif_cpct1a.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table3, keep_percent = FALSE, sig_labels = "a"),
    "rds/signif_cpct25.rds")

mtcars_table3 = cro_cpct(list(mtcars$cyl, mtcars$gear),
                         unvr(list(total(), mtcars$vs, mtcars$am)),
                         total_row_position = "above"
)

expect_equal_to_reference(
    significance_cpct(mtcars_table3,
                      compare_type = c("previous_column", "subtable"),
                      keep_percent = TRUE, 
                      sig_labels = "a"),
    "rds/signif_cpct26.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table3,
                      compare_type = c("previous_column", "subtable"),
                      keep_percent = TRUE, 
                      sig_labels = NULL),
    "rds/signif_cpct26a.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table3,
                      compare_type = c("previous_column", "subtable"),
                      keep_percent = TRUE),
    "rds/signif_cpct27.rds")

mtcars_table3 = cro_cpct(list(mtcars$cyl),
                         unvr(list(total(), mtcars$vs, mtcars$am)),
                         total_row_position = "above"
)

expect_equal_to_reference(
    significance_cpct(mtcars_table3,
                      compare_type = c("previous_column", "subtable"),
                      keep_percent = TRUE),
    "rds/signif_cpct28.rds")

colnames(mtcars_table3) = rep("", NCOL(mtcars_table3))

expect_equal_to_reference(
    significance_cpct(mtcars_table3,
                      compare_type = c("previous_column", "subtable"),
                      keep_percent = TRUE),
    "rds/signif_cpct29.rds")

expect_error(
    significance_cpct(mtcars_table3,
                      compare_type = c("first_column", "adjusted_first_column"),
                      keep_percent = TRUE)
)

mtcars_table3 = cro_cpct(list(mtcars$cyl),
                         (list(total(), mtcars$vs, mtcars$am)),
                         total_row_position = "above"
)

expect_equal_to_reference(
    significance_cpct(mtcars_table3[ ,1],
                      compare_type = c("first_column", "subtable"),
                      keep_percent = TRUE),
    "rds/signif_cpct30.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table3[ ,1:2],
                      compare_type = c("first_column", "subtable"),
                      keep_percent = TRUE),
    "rds/signif_cpct31.rds")


expect_equal_to_reference(
    significance_cpct(mtcars_table3[ ,1:3],
                      compare_type = c("first_column", "subtable"),
                      keep_percent = TRUE),
    "rds/signif_cpct32.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table3[ ,c(1:2,4,5)],
                      compare_type = c("first_column", "subtable"),
                      keep_percent = TRUE),
    "rds/signif_cpct33.rds")
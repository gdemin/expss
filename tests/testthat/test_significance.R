context("significance")

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

expect_equal(expss:::compare_proportions(12/18, 7/14, 18, 14),
             prop.test(tbl, correct = FALSE)$p.value)

tbl[2,1] = 0

expect_equal(expss:::compare_proportions(12/18, 0/7, 18, 7),
             suppressWarnings(prop.test(tbl, correct = FALSE))$p.value)

t_test_pval = t.test(mpg ~ am, data = mtcars)$p.value 

res = with(mtcars, 
     expss:::compare_means(
         mean(mpg[am==0]), mean(mpg[am==1]), 
         sd(mpg[am==0]),  sd(mpg[am==1]),
         length(mpg[am==0]), length(mpg[am==1])
     ))

expect_equal(res, t_test_pval)


res = with(mtcars, 
           expss:::compare_means(
               c(mean(mpg[am==0]), mean(mpg[am==0])), c(mean(mpg[am==1]), mean(mpg[am==1])), 
               c(sd(mpg[am==0]), sd(mpg[am==0])),  c(sd(mpg[am==1]), sd(mpg[am==1])),
               c(length(mpg[am==0]), length(mpg[am==0])), c(length(mpg[am==1]), length(mpg[am==1]))
           ))

expect_equal(res, c(t_test_pval, t_test_pval))

t_test_pval = t.test(mpg ~ am, data = mtcars, var.equal = TRUE)$p.value 

res = with(mtcars, 
           expss:::compare_means(
               c(mean(mpg[am==0]), mean(mpg[am==0])), c(mean(mpg[am==1]), mean(mpg[am==1])), 
               c(sd(mpg[am==0]), sd(mpg[am==0])),  c(sd(mpg[am==1]), sd(mpg[am==1])),
               c(length(mpg[am==0]), length(mpg[am==0])), c(length(mpg[am==1]), length(mpg[am==1])),
               var_equal = TRUE
           ))

expect_equal(res, c(t_test_pval, t_test_pval))


mtcars_table = cro_cpct(list(mtcars$cyl, mtcars$gear),
                        list(total(), mtcars$vs, mtcars$am))



expect_equal_to_reference(
    significance_cpct(mtcars_table),
    "rds/signif_cpct1.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table, na_as_zero = TRUE),
    "rds/signif_cpct2.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table, sig_labels = paste0("(",letters, ")")),
    "rds/signif_cpct3.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table, sig_labels = NULL),
    "rds/signif_cpct4.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table, compare_type = c("subtable", "first_column")),
    "rds/signif_cpct5.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table, compare_type = c("subtable", "first_column_adjusted")),
    "rds/signif_cpct6.rds")
expect_equal_to_reference(
    significance_cpct(mtcars_table,
                      compare_type = c("subtable", "first_column_adjusted"),
                      na_as_zero = TRUE
                      ),
    "rds/signif_cpct7.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, compare_type = c("previous_column")),
    "rds/signif_cpct8.rds")

expect_equal_to_reference(
    significance_cpct(mtcars_table, compare_type = c("previous_column"), na_as_zero = TRUE),
    "rds/signif_cpct9.rds")
# significance_cpct(mtcars_table, compare_type = c("subtable", "first_column"), sig_labels = paste0("(",letters, ")"))
# significance_cpct(mtcars_table, compare_type = c("subtable", "first_column_adjusted"))



# table(mtcars$am, mtcars$vs) %>% chisq.test(correct = FALSE) %>% extract2("p.value")
# table(mtcars$am, mtcars$vs) %>% prop.test(correct = FALSE) %>% extract2("p.value")



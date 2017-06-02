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

expect_equal(expss:::prop_pvalue(12/18, 7/14, 18, 14),
             prop.test(tbl, correct = FALSE)$p.value)

tbl[2,1] = 0

expect_equal(expss:::prop_pvalue(12/18, 0/7, 18, 7),
             suppressWarnings(prop.test(tbl, correct = FALSE))$p.value)

t_test_pval = t.test(mpg ~ am, data = mtcars)$p.value 

res = with(mtcars, 
     expss:::means_pvalue(
         mean(mpg[am==0]), mean(mpg[am==1]), 
         sd(mpg[am==0]),  sd(mpg[am==1]),
         length(mpg[am==0]), length(mpg[am==1])
     ))

expect_equal(res, t_test_pval)


res = with(mtcars, 
           expss:::means_pvalue(
               c(mean(mpg[am==0]), mean(mpg[am==0])), c(mean(mpg[am==1]), mean(mpg[am==1])), 
               c(sd(mpg[am==0]), sd(mpg[am==0])),  c(sd(mpg[am==1]), sd(mpg[am==1])),
               c(length(mpg[am==0]), length(mpg[am==0])), c(length(mpg[am==1]), length(mpg[am==1]))
           ))

expect_equal(res, c(t_test_pval, t_test_pval))

t_test_pval = t.test(mpg ~ am, data = mtcars, var.equal = TRUE)$p.value 

res = with(mtcars, 
           expss:::means_pvalue(
               c(mean(mpg[am==0]), mean(mpg[am==0])), c(mean(mpg[am==1]), mean(mpg[am==1])), 
               c(sd(mpg[am==0]), sd(mpg[am==0])),  c(sd(mpg[am==1]), sd(mpg[am==1])),
               c(length(mpg[am==0]), length(mpg[am==0])), c(length(mpg[am==1]), length(mpg[am==1])),
               var_equal = TRUE
           ))

expect_equal(res, c(t_test_pval, t_test_pval))


mtcars_table = cro_cpct(list(mtcars$cyl, mtcars$gear),
                        list(total(), mtcars$vs, mtcars$am))



# 
# 
# significance_cpct(mtcars_table)
# significance_cpct(mtcars_table, sig_labels = paste0("(",letters, ")"))
# significance_cpct(mtcars_table, compare_type = c("subtable", "first_column"))
# significance_cpct(mtcars_table, compare_type = c("subtable", "first_column"), sig_labels = paste0("(",letters, ")"))
# significance_cpct(mtcars_table, compare_type = c("subtable", "first_column_adjusted"))
# significance_cpct(mtcars_table, compare_type = c("first_column"), sig_level = .2)


table(mtcars$am, mtcars$vs) %>% chisq.test(correct = FALSE) %>% extract2("p.value")
table(mtcars$am, mtcars$vs) %>% prop.test(correct = FALSE) %>% extract2("p.value")



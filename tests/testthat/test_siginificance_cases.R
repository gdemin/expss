context("significance_cases")

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

mtcars_table = cro_cases(list(mtcars$cyl, mtcars$gear),
                        list(total(), mtcars$vs, mtcars$am))

# table(mtcars$cyl, mtcars$vs) %>% chisq.test()
expect_equal_to_reference(
    significance_cases(mtcars_table),
    "rds/significance_cases1.rds"
)

# table(mtcars$gear) %>% chisq.test()
expect_equal_to_reference(
    significance_cases(mtcars_table, sig_level = 0.1),
    "rds/significance_cases2.rds"
)

expect_equal_to_reference(
    significance_cases(mtcars_table, sig_level = 0.1, min_base = 20),
    "rds/significance_cases3.rds"
)

expect_equal_to_reference(
    significance_cases(mtcars_table, sig_level = 0.1, keep_cases = FALSE),
    "rds/significance_cases4.rds"
)

expect_equal_to_reference(
    significance_cases(mtcars_table, sig_level = 0.1, 
                       keep_cases = TRUE, keep_bases = FALSE),
    "rds/significance_cases5.rds"
)

expect_equal_to_reference(
    significance_cases(mtcars_table, sig_level = 0.1, 
                       keep_cases = FALSE, keep_bases = TRUE),
    "rds/significance_cases5.rds"
)

expect_equal_to_reference(
    significance_cases(mtcars_table, sig_level = 0.1, digits = 3),
    "rds/significance_cases6.rds"
)
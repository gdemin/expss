context("prepend_names/prepend_values")

data(mtcars)

res = mtcars %>% 
    prepend_names %>% 
    calculate(
        cro_cpct(list(cyl, gear), list(total(), vs, am))
    )

expect_equal_to_reference(res, "rds/prepend1.rds")

res = mtcars %>% 
    prepend_values %>% 
    calculate(
        cro_cpct(list(cyl, gear), list(total(), vs, am))
    )

expect_equal_to_reference(res, "rds/prepend2.rds")

res = mtcars %>% 
    prepend_all %>% 
    calculate(
        cro_cpct(list(cyl, gear), list(total(), vs, am))
    )

expect_equal_to_reference(res, "rds/prepend1.rds")

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
    prepend_names %>% 
    calculate(
        cro_cpct(list(cyl, gear), list(total(), vs, am))
    )

expect_equal_to_reference(res, "rds/prepend3.rds")

res = mtcars %>% 
    prepend_values %>% 
    calculate(
        cro_cpct(list(cyl, gear), list(total(), vs, am))
    )

expect_equal_to_reference(res, "rds/prepend4.rds")

res = mtcars %>% 
    prepend_all %>% 
    calculate(
        cro_cpct(list(cyl, gear), list(total(), vs, am))
    )

expect_equal_to_reference(res, "rds/prepend4.rds")
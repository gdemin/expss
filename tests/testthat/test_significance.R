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

mtcars_table = cro_cpct(list(mtcars$cyl, mtcars$gear), 
                        list(total(), mtcars$vs, mtcars$am)) 


significance_cpct(mtcars_table)
significance_cpct(mtcars_table, sig_labels = paste0("(",letters, ")"))
significance_cpct(mtcars_table, compare_type = c("subtable", "first_column"))
significance_cpct(mtcars_table, compare_type = c("subtable", "first_column"), sig_labels = paste0("(",letters, ")"))
significance_cpct(mtcars_table, compare_type = c("subtable", "first_column_adjusted"))
significance_cpct(mtcars_table, compare_type = c("first_column"), sig_level = .2)

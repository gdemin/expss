context("apply_labels")

data(mtcars)
correct_mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"
    var_lab(vs) = "Engine"
    val_lab(vs) = c("V-engine" = 0, 
                    "Straight engine" = 1) 
    var_lab(am) = "Transmission"
    val_lab(am) = c(automatic = 0, 
                    manual=1)
    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
})


test_mtcars = apply_labels(mtcars, 
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
                               am = c(automatic = 0, 
                                               manual=1),
                               gear = "Number of forward gears",
                               carb = "Number of carburetors"
                               )

expect_identical(correct_mtcars, test_mtcars)

expect_warning(apply_labels(mtcars, 
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
                            am = c(automatic = 0, 
                                   manual=1),
                            gear = "Number of forward gears",
                            carb = "Number of carburetors",
                            doentexists = "one",
                            doentexists2 = "two"
))

expect_error(apply_labels(mtcars, 
                            mpg = "Miles/(US) gallon",
                            cyl = "Number of cylinders",
                            disp = "Displacement (cu.in.)",
                            hp = c("Gross horsepower", 1),
                            drat = "Rear axle ratio",
                            wt = "Weight (lb/1000)",
                            qsec = "1/4 mile time",
                            vs = "Engine",
                            vs = c("V-engine" = 0, 
                                   "Straight engine" = 1),
                            am = "Transmission",
                            am = c(automatic = 0, 
                                   manual=1),
                            gear = "Number of forward gears",
                            carb = "Number of carburetors"
))


data(mtcars)
correct_mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"
    var_lab(vs) = "Engine"
    var_lab(am) = "Transmission"
    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
})


test_mtcars = apply_var_labs(mtcars, 
                           mpg = "Miles/(US) gallon",
                           cyl = "Number of cylinders",
                           disp = "Displacement (cu.in.)",
                           hp = "Gross horsepower",
                           drat = "Rear axle ratio",
                           wt = "Weight (lb/1000)",
                           qsec = "1/4 mile time",
                           vs = "Engine",
                           am = "Transmission",
                           gear = "Number of forward gears",
                           carb = "Number of carburetors"
)

expect_identical(correct_mtcars, test_mtcars)

expect_warning(apply_var_labs(mtcars, 
                            mpg = "Miles/(US) gallon",
                            cyl = "Number of cylinders",
                            disp = "Displacement (cu.in.)",
                            hp = "Gross horsepower",
                            drat = "Rear axle ratio",
                            wt = "Weight (lb/1000)",
                            qsec = "1/4 mile time",
                            vs = "Engine",
                            am = "Transmission",
                            gear = "Number of forward gears",
                            carb = "Number of carburetors",
                            doentexists = "one",
                            doentexists2 = "two"
))

expect_warning(apply_var_labs(mtcars, 
                              mpg = "Miles/(US) gallon",
                              cyl = "Number of cylinders",
                              disp = "Displacement (cu.in.)",
                              hp = "Gross horsepower",
                              drat = "Rear axle ratio",
                              wt = "Weight (lb/1000)",
                              qsec = "1/4 mile time",
                              vs = "Engine",
                              am = "Transmission",
                              gear = "Number of forward gears",
                              gear = "Number of forward gears",
                              carb = "Number of carburetors"
))

expect_error(apply_var_labs(mtcars, 
                          mpg = "Miles/(US) gallon",
                          cyl = "Number of cylinders",
                          disp = "Displacement (cu.in.)",
                          hp = c("Gross horsepower", 1),
                          drat = "Rear axle ratio",
                          wt = "Weight (lb/1000)",
                          qsec = "1/4 mile time",
                          vs = c("V-engine" = 0, 
                                 "Straight engine" = 1),
                          am = "Transmission",
                          gear = "Number of forward gears",
                          carb = "Number of carburetors"
))


data(mtcars)
correct_mtcars = modify(mtcars,{
    val_lab(vs) = c("V-engine" = 0, 
                    "Straight engine" = 1) 
    val_lab(am) = c(automatic = 0, 
                    manual=1)
})

test_mtcars = apply_val_labs(mtcars, 
                           vs = c("V-engine" = 0, 
                                  "Straight engine" = 1),
                           am = c(automatic = 0, 
                                  manual=1)
)

expect_identical(correct_mtcars, test_mtcars)

expect_warning(apply_val_labs(mtcars, 
                            vs = c("V-engine" = 0, 
                                   "Straight engine" = 1),
                            am = c(automatic = 0, 
                                   manual=1),
                            doentexists = "one",
                            doentexists2 = "two"
))

expect_warning(apply_val_labs(mtcars, 
                              vs = c("V-engine" = 0, 
                                     "Straight engine" = 1),
                              am = c(automatic = 0, 
                                     manual=1),
                              am = c(automatic = 0, 
                                     manual=1)
))

expect_error(apply_val_labs(mtcars, 
                          vs = "Engine"
))


context("apply_labels default_dataset")

data(mtcars)
default_mtcars = mtcars
default_dataset(default_mtcars)

correct_mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"
    var_lab(vs) = "Engine"
    val_lab(vs) = c("V-engine" = 0, 
                    "Straight engine" = 1) 
    var_lab(am) = "Transmission"
    val_lab(am) = c(automatic = 0, 
                    manual=1)
    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
})


.apply_labels( 
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
              am = c(automatic = 0, 
                     manual=1),
              gear = "Number of forward gears",
              carb = "Number of carburetors"
)

expect_identical(correct_mtcars, default_mtcars)


data(mtcars)
default_mtcars = mtcars
default_dataset(default_mtcars)

correct_mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"
    var_lab(vs) = "Engine"
    var_lab(am) = "Transmission"
    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
})


.apply_var_labs( 
              mpg = "Miles/(US) gallon",
              cyl = "Number of cylinders",
              disp = "Displacement (cu.in.)",
              hp = "Gross horsepower",
              drat = "Rear axle ratio",
              wt = "Weight (lb/1000)",
              qsec = "1/4 mile time",
              vs = "Engine",
              am = "Transmission",
              gear = "Number of forward gears",
              carb = "Number of carburetors"
)

expect_identical(correct_mtcars, default_mtcars)


data(mtcars)
default_mtcars = mtcars
default_dataset(default_mtcars)

correct_mtcars = modify(mtcars,{
    val_lab(vs) = c("V-engine" = 0, 
                    "Straight engine" = 1) 
    val_lab(am) = c(automatic = 0, 
                    manual=1)
})


.apply_val_labs(
              vs = c("V-engine" = 0, 
                     "Straight engine" = 1),
              am = c(automatic = 0, 
                     manual=1)
)

expect_identical(correct_mtcars, default_mtcars)

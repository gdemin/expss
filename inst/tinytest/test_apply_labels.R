context("apply_labels")

data(mtcars)
correct_mtcars = mtcars

var_lab(correct_mtcars$mpg) = "Miles/(US) gallon"
var_lab(correct_mtcars$cyl) = "Number of cylinders"
var_lab(correct_mtcars$disp) = "Displacement (cu.in.)"
var_lab(correct_mtcars$hp) = "Gross horsepower"
var_lab(correct_mtcars$drat) = "Rear axle ratio"
var_lab(correct_mtcars$wt) = "Weight (lb/1000)"
var_lab(correct_mtcars$qsec) = "1/4 mile time"
var_lab(correct_mtcars$vs) = "Engine"
val_lab(correct_mtcars$vs) = c("V-engine" = 0, 
                "Straight engine" = 1) 
var_lab(correct_mtcars$am) = "Transmission"
val_lab(correct_mtcars$am) = c(automatic = 0, 
                manual=1)
var_lab(correct_mtcars$gear) = "Number of forward gears"
var_lab(correct_mtcars$carb) = "Number of carburetors"



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
correct_mtcars = mtcars

var_lab(correct_mtcars$mpg) = "Miles/(US) gallon"
var_lab(correct_mtcars$cyl) = "Number of cylinders"
var_lab(correct_mtcars$disp) = "Displacement (cu.in.)"
var_lab(correct_mtcars$hp) = "Gross horsepower"
var_lab(correct_mtcars$drat) = "Rear axle ratio"
var_lab(correct_mtcars$wt) = "Weight (lb/1000)"
var_lab(correct_mtcars$qsec) = "1/4 mile time"
var_lab(correct_mtcars$vs) = "Engine"
val_lab(correct_mtcars$vs) = c("V-engine" = 0, 
                               "Straight engine" = 1) 
var_lab(correct_mtcars$am) = "Transmission"
val_lab(correct_mtcars$am) = c(automatic = 0, 
                               manual=1)
var_lab(correct_mtcars$gear) = "Number of forward gears"
var_lab(correct_mtcars$carb) = "Number of carburetors"



data(mtcars)
correct_mtcars = mtcars

val_lab(correct_mtcars$vs) = c("V-engine" = 0, 
                    "Straight engine" = 1) 
val_lab(correct_mtcars$am) = c(automatic = 0, 
                    manual=1)


test_mtcars = apply_labels(mtcars, 
                           vs = c("V-engine" = 0, 
                                  "Straight engine" = 1),
                           am = c(automatic = 0, 
                                  manual=1)
)

expect_identical(correct_mtcars, test_mtcars)

expect_warning(apply_labels(mtcars, 
                            vs = c("V-engine" = 0, 
                                   "Straight engine" = 1),
                            am = c(automatic = 0, 
                                   manual=1),
                            doentexists = "one",
                            doentexists2 = "two"
))


data("mtcars")

dt_mt = as.data.table(mtcars)

new_mt  = apply_labels(dt_mt,
                       vs = "Engine",
                       vs = c("V-engine" = 0, 
                              "Straight engine" = 1)
                       )


expect_identical(dt_mt, new_mt)

expect_identical(var_lab(dt_mt$vs), "Engine")
expect_identical(val_lab(dt_mt$vs), c("V-engine" = 0, 
                                      "Straight engine" = 1))

expect_silent(dt_mt[,new:=1])
expect_silent(new_mt[,new:=1])

context("apply_labels list argument")

data(mtcars)
correct_mtcars = mtcars

var_lab(correct_mtcars$mpg) = "Miles/(US) gallon"
var_lab(correct_mtcars$cyl) = "Number of cylinders"
var_lab(correct_mtcars$disp) = "Displacement (cu.in.)"
var_lab(correct_mtcars$hp) = "Gross horsepower"
var_lab(correct_mtcars$drat) = "Rear axle ratio"
var_lab(correct_mtcars$wt) = "Weight (lb/1000)"
var_lab(correct_mtcars$qsec) = "1/4 mile time"
var_lab(correct_mtcars$vs) = "Engine"
val_lab(correct_mtcars$vs) = c("V-engine" = 0, 
                               "Straight engine" = 1) 
var_lab(correct_mtcars$am) = "Transmission"
val_lab(correct_mtcars$am) = c(automatic = 0, 
                               manual=1)
var_lab(correct_mtcars$gear) = "Number of forward gears"
var_lab(correct_mtcars$carb) = "Number of carburetors"

list_arg =  list(
    drat = "Rear axle ratio",
    wt = "Weight (lb/1000)",
    qsec = "1/4 mile time",
    vs = "Engine",
    vs = c("V-engine" = 0, 
           "Straight engine" = 1)
)

test_mtcars = apply_labels(mtcars, 
                           mpg = "Miles/(US) gallon",
                           cyl = "Number of cylinders",
                           disp = "Displacement (cu.in.)",
                           hp = "Gross horsepower",
                           list_arg,
                           am = "Transmission",
                           am = c(automatic = 0, 
                                  manual=1),
                           gear = "Number of forward gears",
                           carb = "Number of carburetors"
)

expect_identical(correct_mtcars, test_mtcars)


test_mtcars = apply_labels(mtcars, 
                           list_arg,
                           mpg = "Miles/(US) gallon",
                           cyl = "Number of cylinders",
                           disp = "Displacement (cu.in.)",
                           hp = "Gross horsepower",
                           am = "Transmission",
                           am = c(automatic = 0, 
                                  manual=1),
                           gear = "Number of forward gears",
                           carb = "Number of carburetors"
)

expect_identical(correct_mtcars, test_mtcars)

list_arg2 = list(
    mpg = "Miles/(US) gallon",
    cyl = "Number of cylinders",
    disp = "Displacement (cu.in.)",
    hp = "Gross horsepower",
    am = "Transmission",
    am = c(automatic = 0, 
           manual=1),
    gear = "Number of forward gears",
    carb = "Number of carburetors"
)

test_mtcars = apply_labels(mtcars, 
                           list_arg,
                           list_arg2
)

expect_identical(correct_mtcars, test_mtcars)

test_mtcars = apply_labels(mtcars, 
                           c(list_arg,
                           list_arg2)
)

expect_identical(correct_mtcars, test_mtcars)

test_mtcars = apply_labels(mtcars, 
                           list(list_arg,
                             list_arg2)
)

expect_identical(correct_mtcars, test_mtcars)

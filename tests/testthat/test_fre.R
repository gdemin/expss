context("elementary_freq")
data(mtcars)
mtcars = modify(mtcars,{
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



expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs)), "rds/fre1.rds")


expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, am)), "rds/fre2.rds")


mtcars$am[1:2] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, am)), "rds/fre3.rds")

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(am)), "rds/fre4.rds")


mtcars$vs[4:5] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, am)), "rds/fre5.rds")

context("elementary_freq weighted")
data(mtcars)
mtcars = modify(mtcars,{
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


mtcars$weight = 2

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, weight = weight)), "rds/fre6.rds")

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, am, weight = weight)), "rds/fre7.rds")


mtcars$am[1:2] = NA

expect_equal_to_reference( with(mtcars, expss:::elementary_freq(vs, am, weight = weight)), "rds/fre8.rds")

expect_equal_to_reference (with(mtcars, expss:::elementary_freq(am, weight = weight)), "rds/fre9.rds")


mtcars$vs[4:5] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, am, weight = weight)),"rds/fre10.rds")


context("elementary_freq multiple")



data(mtcars)
mtcars = modify(mtcars,{
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

mtcars$vs1 = ifelse(1:32<=16, mtcars$vs, NA)
mtcars$vs2 = ifelse(1:32>16, mtcars$vs, NA)

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2)), "rds/fre11.rds")


expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am)), "rds/fre12.rds")


mtcars$am[1:2] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am)), "rds/fre13.rds")




mtcars$vs1[4:5] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am)), "rds/fre14.rds")

context("elementary_freq multiple weighted")
data(mtcars)
mtcars = modify(mtcars,{
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

mtcars$vs1 = ifelse(1:32>16, mtcars$vs, NA)
mtcars$vs2 = ifelse(1:32<=16, mtcars$vs, NA)

mtcars$weight = 2

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, weight = weight)), "rds/fre15.rds")


expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am, weight = weight)), "rds/fre16.rds")

mtcars$am[1:2] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am, weight = weight)), "rds/fre17.rds")


mtcars$vs1[4:5] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am, weight = weight)), "rds/fre18.rds")


context("elementary_freq factor")
expect_equal_to_reference(expss:::elementary_freq(iris$Species), "rds/fre19.rds")
expect_equal_to_reference(expss:::elementary_freq(iris$Species, iris$Species), "rds/fre20.rds")



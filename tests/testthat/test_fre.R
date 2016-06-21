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



expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs)), "rds/elem_fre1.rds")


expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, am)), "rds/elem_fre2.rds")


mtcars$am[1:2] = NA

expect_equal_to_reference(fre(mtcars$am), "rds/fre2.5.rds")

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, am)), "rds/elem_fre3.rds")

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(am)), "rds/elem_fre4.rds")


mtcars$vs[4:5] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, am)), "rds/elem_fre5.rds")

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

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, weight = weight)), "rds/elem_fre6.rds")

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, am, weight = weight)), "rds/elem_fre7.rds")


mtcars$am[1:2] = NA

expect_equal_to_reference( with(mtcars, expss:::elementary_freq(vs, am, weight = weight)), "rds/elem_fre8.rds")

expect_equal_to_reference (with(mtcars, expss:::elementary_freq(am, weight = weight)), "rds/elem_fre9.rds")


mtcars$vs[4:5] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs, am, weight = weight)),"rds/elem_fre10.rds")


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

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2)), "rds/elem_fre11.rds")


expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am)), "rds/elem_fre12.rds")


mtcars$am[1:2] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am)), "rds/elem_fre13.rds")




mtcars$vs1[4:5] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am)), "rds/elem_fre14.rds")

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

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, weight = weight)), "rds/elem_fre15.rds")


expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am, weight = weight)), "rds/elem_fre16.rds")

mtcars$am[1:2] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am, weight = weight)), "rds/elem_fre17.rds")


mtcars$vs1[4:5] = NA

expect_equal_to_reference(with(mtcars, expss:::elementary_freq(vs1 %to% vs2, am, weight = weight)), "rds/elem_fre18.rds")


context("elementary_freq factor")
expect_equal_to_reference(expss:::elementary_freq(iris$Species), "rds/elem_fre19.rds")
expect_equal_to_reference(expss:::elementary_freq(iris$Species, iris$Species), "rds/elem_fre20.rds")



context("fre and cro examples")


data(mtcars)
mtcars = modify(mtcars,{
    var_lab(vs) = "Engine"
    val_lab(vs) = c("V-engine" = 0, 
                    "Straight engine" = 1) 
    var_lab(am) = "Transmission"
    val_lab(am) = c(automatic = 0, 
                    manual=1)
})

expect_equal_to_reference(fre(mtcars$vs), "rds/fre_ex1.rds")
expect_equal_to_reference(with(mtcars, cro(am, vs)), "rds/fre_ex2.rds")
expect_equal_to_reference(with(mtcars, cro_pct(am, vs)), "rds/fre_ex3.rds")

# multiple-choise variable
# brands - multiple response question
# Which brands do you use during last three months? 
set.seed(123)
brands = data.frame(t(replicate(20,sample(c(1:5,NA),4,replace = FALSE))))
# score - evaluation of tested product
score = sample(-1:1,20,replace = TRUE)
var_lab(brands) = "Used brands"
val_lab(brands) = make_labels("
                              1 Brand A
                              2 Brand B
                              3 Brand C
                              4 Brand D
                              5 Brand E
                              ")

var_lab(score) = "Evaluation of tested brand"
val_lab(score) = make_labels("
                             -1 Dislike it
                             0 So-so
                             1 Like it    
                             ")

expect_equal_to_reference(fre(brands), "rds/fre_ex4.rds")
expect_equal_to_reference(cro(brands, score), "rds/fre_ex5.rds")
expect_equal_to_reference(cro_pct(brands, score), "rds/fre_ex6.rds")

context("fre and cro some special cases")

expect_equal_to_reference(fre(numeric(0)), "rds/fre1.rds")

a = rep(NA, 5)

expect_equal_to_reference(fre(a), "rds/fre2.rds")

expect_equal_to_reference(cro(a, a), "rds/cro1.rds")
expect_equal_to_reference(cro_pct(a, a), "rds/cro2.rds")

a = c(1,1,1, NA, NA)
b = c(NA, NA, NA, 1, 1)
expect_equal_to_reference(cro(a, b), "rds/cro3.rds")
expect_equal_to_reference(cro_pct(a, b), "rds/cro4.rds")
a = c(1,1,1, 1, 1)
b = c(1, 1, 1, 1, 1)
weight = rep(NA, 5)
expect_equal_to_reference(cro(a, b, weight), "rds/cro5.rds")
expect_equal_to_reference(cro_pct(a, b, weight), "rds/cro5.rds")

context("fre and cro from real life")

data = readRDS("rds/data.rds")

data$reg[1:10] = NA
q8 = sprintf("q8r_%s", c(1:12, 99))
data[1:10, q8] = NA
var_lab(data$q8r_1) = "Используемые услуги"
expect_equal_to_reference(fre(data$reg), "rds/fre_real1.rds")
expect_equal_to_reference(fre(data$s1), "rds/fre_real2.rds")
expect_equal_to_reference(with(data, fre(q8r_1 %to% q8r_99)), "rds/fre_real3.rds")


expect_equal_to_reference(cro(data$reg, data$s1), "rds/cro_real1.rds")
expect_equal_to_reference(cro_pct(data$reg, data$s1), "rds/cro_real2.rds")
expect_equal_to_reference(with(data, cro(q8r_1 %to% q8r_99, reg)), "rds/cro_real3.rds")
expect_equal_to_reference(with(data, cro_pct(q8r_1 %to% q8r_99, reg)), "rds/cro_real4.rds")
expect_equal_to_reference(with(data, cro(q8r_1 %to% q8r_99, s1)), "rds/cro_real5.rds")
expect_equal_to_reference(with(data, cro_pct(q8r_1 %to% q8r_99, s1)), "rds/cro_real6.rds")

#### with weight
expect_equal_to_reference(fre(data$reg, weight = data$weight1), "rds/fre_real1w.rds")
expect_equal_to_reference(fre(data$s1, weight = data$weight1), "rds/fre_real2w.rds")
expect_equal_to_reference(with(data, fre(q8r_1 %to% q8r_99, weight = weight1)), "rds/fre_real3w.rds")


expect_equal_to_reference(cro(data$reg, data$s1, weight = data$weight1), "rds/cro_real1w.rds")
expect_equal_to_reference(cro_pct(data$reg, data$s1, weight = data$weight1), "rds/cro_real2w.rds")
expect_equal_to_reference(with(data, cro(q8r_1 %to% q8r_99, reg, weight = weight1)), "rds/cro_real3w.rds")
expect_equal_to_reference(with(data, cro_pct(q8r_1 %to% q8r_99, reg, weight = weight1)), "rds/cro_real4w.rds")
expect_equal_to_reference(with(data, cro(q8r_1 %to% q8r_99, s1, weight = weight1)), "rds/cro_real5w.rds")
expect_equal_to_reference(with(data, cro_pct(q8r_1 %to% q8r_99, s1, weight = weight1)), "rds/cro_real6w.rds")








context("w_lm")
data(mtcars)


mod = mtcars %>% 
    w_lm(mpg ~ disp + hp + am)

expect_equal(coef(mod), coef(lm(mpg ~ disp + hp + am, data = mtcars)))

expect_equal(coef(summary(mod)), coef(summary(lm(mpg ~ disp + hp + am, data = mtcars))))

mod_weighted = mtcars %>% 
    w_lm(mpg ~ disp + hp + am, weight = wt)

# results are tests against SPSS linear regression
# WEIGHT BY wt.
# REGRESSION
# /MISSING LISTWISE
# /STATISTICS COEFF OUTS R ANOVA
# /CRITERIA=PIN(.05) POUT(.10)
# /NOORIGIN 
# /DEPENDENT mpg
# /METHOD=ENTER disp hp am.
expect_equal(
    unname(round(coef(mod_weighted), 6)), c(27.430259, -0.015215, -0.036862, 3.354353)
)

summary_weighted = summary(mod_weighted)
pvalue = unname(summary_weighted$coefficients[,"Pr(>|t|)"])
expect_equal(
    round(pvalue, 14), 
    round(c(3.7449E-57, 0.00033488253600, .00000013879170, 0.00001057396390), 14)
)

expect_equal(
    round(summary_weighted$r.squared, 6), 0.795608
)
expect_equal(
    round(summary_weighted$adj.r.squared, 6),
    0.789412
)

###

mod_weighted = mtcars %>% 
    w_lm(mpg ~ disp + hp + am, weight = wt, weight_is_frequency = FALSE)

mod_lm = mtcars %>% 
    lm(mpg ~ disp + hp + am, weight = wt, data = .)


expect_equal(
    coef(mod_weighted), coef(mod_lm)
)

summary_weighted = summary(mod_weighted)
summary_lm = summary(mod_lm)

expect_equal(
    summary_weighted$coefficients[,"Pr(>|t|)"], 
    summary_lm$coefficients[,"Pr(>|t|)"]
)

expect_equal(
    summary_weighted$r.squared, summary_lm$r.squared
)
expect_equal(
    summary_weighted$adj.r.squared, summary_lm$adj.r.squared
)

expect_equal(
    summary_weighted$df, summary_lm$df
)

mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (1000 lbs)",
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



mod = mtcars %>% 
    w_lm(mpg ~ disp + hp + am)

expect_equal(rownames(coef(summary(mod))), 
             c("(Intercept)", "Displacement (cu.in.)", "Gross horsepower", "Transmission"))


expect_equal(rownames(coef(summary(unlab(mod)))), 
             c("(Intercept)", "disp", "hp", "am"))



expect_equal(rownames(coef(summary(drop_var_labs(mod)))), 
             c("(Intercept)", "disp", "hp", "am"))

expect_equal(rownames(coef(summary(prepend_names(mod)))), 
             c("(Intercept)", "disp Displacement (cu.in.)", "hp Gross horsepower", "am Transmission"))


data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "",
                      am = ""
)

mod = mtcars %>% 
    w_lm(mpg ~ disp + hp + am)

expect_equal(rownames(coef(summary(mod))), 
             c("(Intercept)", "Displacement (cu.in.)", "hp", "am"))



expect_equal(rownames(coef(summary(unlab(mod)))), 
             c("(Intercept)", "disp", "hp", "am"))



expect_equal(rownames(coef(summary(drop_var_labs(mod)))), 
             c("(Intercept)", "disp", "hp", "am"))

expect_equal(rownames(coef(summary(prepend_names(mod)))), 
             c("(Intercept)", "disp Displacement (cu.in.)", "hp", "am"))

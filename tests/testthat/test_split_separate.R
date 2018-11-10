context("split_separate")

data(airquality)
aq_list = airquality %>% 
    split_separate(Month) %>% 
    compute({
        Ozone_zscore = scale(Ozone)
    }) 

expect_equal_to_reference(aq_list, "rds/split_separate1.rds",  update = FALSE)

aq_list = airquality %>% 
    split_by(Month) %>% 
    compute({
        Ozone_zscore = scale(Ozone)
    }) 

expect_equal_to_reference(aq_list, "rds/split_separate1.rds",  update = FALSE)

aq2 = aq_list %>% split_off()
expect_equal_to_reference(aq2, "rds/split_separate2.rds",  update = FALSE)

data(mtcars)
# add labels to dataset
mtcars = apply_labels(mtcars, 
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      carb = NULL,
                      qsec = "1/4 mile time",
                      hp = "Gross horsepower",
                      vs = "Engine",
                      vs = num_lab(" 
                                   0 V-engine
                                   1 Straight engine
                                   "),
                      
                      am = "Transmission",
                      am = num_lab(" 
                                   0 Automatic
                                   1 Manual
                                   99 Not specified
                                   ")
                      )

mtcars_list = mtcars %>% split_separate(am, vs, drop = FALSE)
expect_equal_to_reference(mtcars_list, "rds/split_separate4.rds",  update = FALSE)

mtcars_list = mtcars %>% split_separate(am, vs)
expect_equal_to_reference(mtcars_list, "rds/split_separate3.rds",  update = FALSE)


regr = mtcars_list %>% 
    use_labels({
        res = lm(mpg ~ hp + disp + wt)
        sheet(Estimate = coef(res), confint(res))
    }) %>% 
    split_off(groups = TRUE, rownames = TRUE)
expect_equal_to_reference(regr, "rds/split_separate5.rds",  update = FALSE)

regr = mtcars_list %>% 
    use_labels({
        res = lm(mpg ~ hp + disp + wt)
        sheet(Estimate = coef(res), confint(res))
    }) %>% 
    split_off(groups = "transmission-engine", rownames = "variables")
expect_equal_to_reference(mtcars_list, "rds/split_separate6.rds",  update = FALSE)



regr = mtcars_list %>% 
    unname() %>% 
    use_labels({
        res = lm(mpg ~ hp + disp + wt)
        sheet(Estimate = coef(res), confint(res))
    }) %>% 
    split_off(groups = "transmission-engine", rownames = "variables")
expect_equal_to_reference(mtcars_list, "rds/split_separate7.rds",  update = FALSE)

regr = mtcars_list %>% 
    use_labels({
        res = lm(mpg ~ hp + disp + wt)
        cbind(Estimate = coef(res), confint(res))
    }) %>% 
    split_off(groups = "transmission-engine", rownames = "variables")
expect_equal_to_reference(mtcars_list, "rds/split_separate7.rds",  update = FALSE)

regr = mtcars_list %>% 
    use_labels({
        res = lm(mpg ~ hp + disp + wt)
        list(Estimate = coef(res), confint(res))
    }) %>% 
    split_off(groups = "transmission-engine", rownames = "variables")
expect_equal_to_reference(mtcars_list, "rds/split_separate7.rds",  update = FALSE)

regr = mtcars_list %>% 
    use_labels({
        res = lm(mpg ~ hp + disp + wt)
        coef(res)
    }) %>% 
    split_off(groups = "transmission-engine", rownames = "variables")
expect_equal_to_reference(mtcars_list, "rds/split_separate8.rds",  update = FALSE)

regr = mtcars_list %>% 
    use_labels({
        cor(cbind(mpg, hp, disp, wt, qsec))
    }) %>% 
    split_off(groups = "transmission-engine", rownames = "variables")
expect_equal_to_reference(mtcars_list, "rds/split_separate9.rds",  update = FALSE)
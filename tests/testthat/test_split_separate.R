context("split_separate")

data(airquality)
airquality2 = airquality %>% 
    split_separate(Month) %>% 
    compute({
        Ozone_zscore = scale(Ozone)
    }) %>% 
    split_off()

mtcars %>% 
    split_separate(am, vs) %>% 
    calculate({
        res = lm(mpg ~ hp + disp + wt)
        dtfrm(coef(res), confint(res))
    }) %>% 
    split_off()

mtcars %>% 
    compute({
        add_val_lab(am) = c("undefined" = 99)
    }) %>% 
    split_separate(am, vs, drop = FALSE) %>% 
    use_labels({
        as.dtfrm(table(cyl, carb))
    }) %>% 
    split_off()
## ---- message=FALSE, warning=FALSE--------------------------------------------
library(expss)
data(mtcars)
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


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(vs) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_caption("Simple column percent")


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_caption("Split by columns and rows")


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_sort_desc() %>% 
    tab_caption("Multiple banners, table is sorted by total")


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), vs %nest% am) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_caption("Nested banners")


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(carb) %>% 
    tab_cols(total(), list(cyl, vs) %nest% am) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_caption("Multiple nested banners")


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(mpg, disp, hp, wt, qsec) %>%
    tab_cols(total(), am) %>% 
    tab_stat_fun(Mean = w_mean, "Std. dev." = w_sd, "Valid N" = w_n) %>%
    tab_pivot() %>% 
    tab_caption("Multiple variable and multiple summary statistics")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(mpg, disp, hp, wt, qsec) %>%
    tab_cols(total(), am) %>% 
    tab_stat_fun(Mean = w_mean, "Valid N" = w_n, method = list) %>%
    tab_pivot() %>% 
    tab_caption("Multiple variable and multiple summary statistics - statistic lables in columns")


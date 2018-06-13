## ---- message=FALSE, warning=FALSE---------------------------------------
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


## ---- fig.height=6, fig.width=7------------------------------------------
with(mtcars, table(am, vs))

boxplot(mpg ~ am, data = mtcars)

## ------------------------------------------------------------------------
# table with dimension names
use_labels(mtcars, table(am, vs)) 

# linear regression
use_labels(mtcars, lm(mpg ~ wt + hp + qsec)) %>% summary

## ---- fig.height=6, fig.width=7------------------------------------------
library(ggplot2, warn.conflicts = FALSE)

use_labels(mtcars, {
    # '..data' is shortcut for all 'mtcars' data.frame inside expression 
    ggplot(..data) +
        geom_point(aes(y = mpg, x = wt, color = qsec)) +
        facet_grid(am ~ vs)
}) 


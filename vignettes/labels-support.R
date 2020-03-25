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


## -----------------------------------------------------------------------------
nps = c(-1, 0, 1, 1, 0, 1, 1, -1)
var_lab(nps) = "Net promoter score"
val_lab(nps) = num_lab("
            -1 Detractors
             0 Neutralists    
             1 Promoters    
")


## -----------------------------------------------------------------------------
var_lab(nps) # get variable label
val_lab(nps) # get value labels

# add new labels
add_val_lab(nps) = num_lab("
                           98 Other    
                           99 Hard to say
                           ")

# remove label by value
# %d% - diff, %n_d% - names diff 
val_lab(nps) = val_lab(nps) %d% 98
# or, remove value by name
val_lab(nps) = val_lab(nps) %n_d% "Other"

## -----------------------------------------------------------------------------
drop_val_labs(nps)
drop_var_labs(nps)
unlab(nps)
drop_unused_labels(nps)
prepend_values(nps)

## ---- fig.height=6, fig.width=7-----------------------------------------------
with(mtcars, table(am, vs))
with(mtcars, 
     barplot(
         table(am, vs), 
         beside = TRUE, 
         legend = TRUE)
     )

## -----------------------------------------------------------------------------
# table with dimension names
use_labels(mtcars, table(am, vs)) 

# linear regression
use_labels(mtcars, lm(mpg ~ wt + hp + qsec)) %>% summary

# boxplot with variable labels
use_labels(mtcars, boxplot(mpg ~ am))

## ---- fig.height=6, fig.width=7-----------------------------------------------
library(ggplot2, warn.conflicts = FALSE)

use_labels(mtcars, {
    # '..data' is shortcut for all 'mtcars' data.frame inside expression 
    ggplot(..data) +
        geom_point(aes(y = mpg, x = wt, color = qsec)) +
        facet_grid(factor(am) ~ factor(vs))
}) 

## -----------------------------------------------------------------------------
nps = c(-1, 0, 1, 1, 0, 1, 1, -1)
var_lab(nps) = "Net promoter score"
val_lab(nps) = num_lab("
            -1 Detractors
             0 Neutralists    
             1 Promoters
             99 Hard to say
")

## -----------------------------------------------------------------------------
expss_disable_value_labels_support()
table(nps) # there is no labels in the result
unique(nps)

## -----------------------------------------------------------------------------
expss_enable_value_labels_support()
# table with labels but there are no label "Hard to say"
table(nps)
unique(nps)

## -----------------------------------------------------------------------------
expss_enable_value_labels_support_extreme()
# now we see "Hard to say" with zero counts
table(nps) 
# weird 'unique'! There is a value 99 which is absent in 'nps'
unique(nps) 


## -----------------------------------------------------------------------------
expss_enable_value_labels_support()

## -----------------------------------------------------------------------------
str(mtcars)

## -----------------------------------------------------------------------------
mtcars_subset = mtcars[1:10, ]

## -----------------------------------------------------------------------------
str(mtcars_subset)

## ---- eval = FALSE------------------------------------------------------------
#  # we need to load packages strictly in this order to avoid conflicts
#  library(haven)
#  library(expss)
#  spss_data = haven::read_spss("spss_file.sav")
#  # add missing 'labelled' class
#  spss_data = add_labelled_class(spss_data)


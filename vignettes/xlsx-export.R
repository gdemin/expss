## -----------------------------------------------------------------------------
library(expss)
library(openxlsx)
data(mtcars)
mtcars = apply_labels(mtcars,
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
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)

mtcars_table = mtcars %>% 
    calc_cro_cpct(
        cell_vars = list(cyl, gear),
        col_vars = list(total(), am, vs)
    ) %>% 
    set_caption("Table 1")

mtcars_table

## -----------------------------------------------------------------------------
wb = createWorkbook()
sh = addWorksheet(wb, "Tables")

## -----------------------------------------------------------------------------
xl_write(mtcars_table, wb, sh)

## ---- eval=FALSE--------------------------------------------------------------
#  saveWorkbook(wb, "table1.xlsx", overwrite = TRUE)

## -----------------------------------------------------------------------------
banner = calc(mtcars, list(total(), am, vs))

## -----------------------------------------------------------------------------
list_of_tables = lapply(mtcars, function(variable) {
    if(length(unique(variable))<7){
        cro_cpct(variable, banner) %>% significance_cpct()
    } else {
        # if number of unique values greater than seven we calculate mean
        cro_mean_sd_n(variable, banner) %>% significance_means()
        
    }
    
})

## -----------------------------------------------------------------------------
wb = createWorkbook()
sh = addWorksheet(wb, "Tables")

## -----------------------------------------------------------------------------
xl_write(list_of_tables, wb, sh, 
         # remove '#' sign from totals 
         col_symbols_to_remove = "#",
         row_symbols_to_remove = "#",
         # format total column as bold
         other_col_labels_formats = list("#" = createStyle(textDecoration = "bold")),
         other_cols_formats = list("#" = createStyle(textDecoration = "bold")),
         )

## ---- eval = FALSE------------------------------------------------------------
#  saveWorkbook(wb, "report.xlsx", overwrite = TRUE)


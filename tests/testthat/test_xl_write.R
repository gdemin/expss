context("xl_write")
if(require(openxlsx, quietly = TRUE, warn.conflicts = FALSE)){
    data(mtcars)
    mtcars = apply_labels(mtcars,
                          mpg = "Miles/(US) gallon|Mean",
                          cyl = "Number of cylinders",
                          disp = "Displacement (cu.in.)|Mean",
                          hp = "Gross horsepower|Mean",
                          drat = "Rear axle ratio",
                          wt = "Weight (lb/1000)",
                          qsec = "1/4 mile time|Mean",
                          vs = "Engine",
                          vs = c("V-engine" = 0,
                                 "Straight engine" = 1),
                          am = "Transmission",
                          am = c("Automatic" = 0,
                                 "Manual"=1),
                          gear = "Number of forward gears",
                          carb = "Number of carburetors"
    )
    
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), 
                            list(mtcars$vs %nest% mtcars$am, "#Total"))   
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table, wb, sh)
    saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table, wb, sh, row = 3, col = 5, remove_repeated = "none")
    saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
}
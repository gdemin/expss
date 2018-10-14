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
    
    mtcars_table = cro_cpct(list(unvr(mtcars$vs)), 
                            list(unvr(mtcars$am))) 
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table, wb, sh, row = 1, col = 1)
    # wb$core = ""
    # expect_known_hash(wb, "d16a04e59b")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table[, FALSE], wb, sh, row = 1, col = 1)
    # wb$core = ""
    # expect_known_hash(wb, "ae5924be7a")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table[, 1], wb, sh, row = 1, col = 1)
    # wb$core = ""
    # expect_known_hash(wb, "1b2ec1a029")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table[FALSE,], wb, sh, row = 1, col = 1)
    # wb$core = ""
    # expect_known_hash(wb, "17d4333c22")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), 
                            list(mtcars$vs %nest% mtcars$am, "#Total"))   
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table[, 1], wb, sh, row = 5, col = 2)
    # wb$core = ""
    # expect_known_hash(wb, "484ac05a7f")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table[FALSE,], wb, sh, row = 5, col = 2)
    # wb$core = ""
    # expect_known_hash(wb, "bb022a4103")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table, wb, sh, row = 5, col = 2)
    # wb$core = ""
    # expect_known_hash(wb, "dd6cf865c7")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(make_subheadings(mtcars_table, 2), wb, sh, row = 5, col = 2)
    # wb$core = ""
    # expect_known_hash(wb, "1c8af7a194")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    # test different style for different table parts
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table, wb, sh, row = 3, col = 5, 
                   header_format = createStyle(fg = "#00ff00"),
                   top_left_corner_format = createStyle(fg = "#0000ff"),
                   main_format = createStyle(
                       halign = "right",
                       fgFill = "#FF0000",
                       numFmt = format(0, nsmall = get_expss_digits())
                   ),
                   total_format = createStyle(fontColour = "#00ffff"),
                   row_labels_format = createStyle(fg = "#00ffff"),
                   total_row_labels_format = createStyle(fg = "#ffff00")
                   )
    # wb$core = ""
    # expect_known_hash(wb, "5874817cef")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table, wb, sh, row = 3, col = 5, 
                   format_table = FALSE,
                   header_format = createStyle(fg = "#00ff00"),
                   top_left_corner_format = createStyle(fg = "#0000ff"),
                   main_format = createStyle(
                       halign = "right",
                       fgFill = "#FF0000",
                       numFmt = format(0, nsmall = get_expss_digits())
                   ),
                   total_format = createStyle(fontColour = "#00ffff"),
                   row_labels_format = createStyle(fg = "#00ffff"),
                   total_row_labels_format = createStyle(fg = "#ffff00")
    )
    # wb$core = ""
    # expect_known_hash(wb, "e23639926e")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table, wb, sh, row = 3, col = 5, remove_repeated = "none")
    # wb$core = ""
    # expect_known_hash(wb, "4ee23cd6b8")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    colnames(mtcars_table)[6] = paste0(" |", colnames(mtcars_table)[6])
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(mtcars_table, wb, sh, row = 3, col = 5, col_symbols_to_remove = "Engine|#", 
                   row_symbols_to_remove = "#")
    # wb$core = ""
    # expect_known_hash(wb, "6cfcca3057")
   
    
}


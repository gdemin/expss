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
    context("xl_write.list")
    
    res_list = list()
    mtcars_table = cro_cpct(list(unvr(mtcars$vs)), 
                            list(unvr(mtcars$am))) 
    
    res_list = c(res_list, list(mtcars_table))
    
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), 
                            list(mtcars$vs %nest% mtcars$am, "#Total")) 
    res_list = c(res_list, list(mtcars_table))
    mtcars_table = calculate(mtcars,
                             cro_mean(list(mpg, hp), list(am %nest% vs)) )
    res_list = c(res_list, list(mtcars_table))
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs, "#Total"))
    res_list = c(res_list, list(mtcars_table))
    mtcars_table = cro_cpct(list(unvr(mtcars$vs)), list(mtcars$vs %nest% mtcars$am, "#Total"))
    res_list = c(res_list, list(mtcars_table))
    colnames(mtcars_table)[1] = "My table"
    new_am = mtcars$am
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total")) %merge%
        cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total"))
    res_list = c(res_list, list(mtcars_table))
    var_lab(new_am) = "|"
    val_lab(new_am) = setNames(0:1, c("", " "))
    mtcars_table = cro_cpct(list(mtcars$vs %nest% mtcars$am), list(mtcars$vs %nest% mtcars$am, "#Total")) %merge%
        cro_cpct(list(mtcars$vs %nest% mtcars$am), list(new_am, "#Total"))
    colnames(mtcars_table)[7] = ""
    res_list = c(res_list, list(mtcars_table))
    data("product_test")
    mtcars_table = product_test %>%
        tab_cols(c1) %>%
        tab_cells(unvr(mrset(a1_1 %to% a1_6))) %>%
        tab_stat_cpct(label = var_lab(a1_1)) %>%
        tab_cells(unvr(mrset(b1_1 %to% b1_6))) %>%
        tab_stat_cpct(label = var_lab(b1_1)) %>%
        tab_pivot(stat_position = "inside_columns")
    res_list = c(res_list, list(mtcars_table))
    mtcars_table = product_test %>%
        compute({
            total = 1
            var_lab(total) = "Total"
            val_lab(total) = setNames(1, " ")
        }) %>% 
        tab_cols(total) %>%
        tab_cells(unvr(mrset(a1_1 %to% a1_6))) %>%
        tab_stat_cpct() %>%
        tab_pivot()
    res_list = c(res_list, list(mtcars_table))
    mtcars_table = fre(mtcars$cyl)
    res_list = c(res_list, list(mtcars_table))
    res_list = c(res_list, list(mtcars))
    res_list = c(res_list, list(as.etable(mtcars)), list(1:3))
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(res_list, wb, sh, row = 3, col = 5, remove_repeated = "columns")
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    wb = createWorkbook()
    sh = addWorksheet(wb, "Tables")
    res = xl_write(res_list, wb, sh, row = 1, col = 1, gap = 2, rownames = TRUE)
    # saveWorkbook(wb, "tables.xlsx", overwrite = TRUE)
    
    
}


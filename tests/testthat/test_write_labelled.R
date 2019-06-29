context("write_labelled_*")
if(isTRUE(getOption("covr")) && dir.exists("data_files")){
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
    
    
    write_labelled_csv(mtcars, "mtcars.csv")
    res = readLines("mtcars.csv")
    mt2 = read_labelled_csv("mtcars.csv")
    expect_known_value(res, "rds/write_labelled_csv1.rds", update = FALSE)
    unlink("mtcars.csv")
    write_labelled_tab(mtcars, "mtcars.tab", single_file = FALSE)
    dat = readLines("mtcars.tab")
    dic= readLines("mtcars.dic.tab")
    expect_known_value(dat, "rds/write_labelled_tab.rds", update = FALSE)
    expect_known_value(dic, "rds/write_labelled_dic_tab.rds", update = FALSE)
    mt3 = read_labelled_tab("mtcars.tab")
    expect_equal(mt2, mt3)
    
    unlink("mtcars.dic.tab")
    unlink("mtcars.tab")
    write_labelled_csv2(mtcars, "mtcars.csv", quote = TRUE)
    res = readLines("mtcars.csv")
    expect_known_value(res, "rds/write_labelled_csv2_quoted.rds", update = FALSE)
    mt4 = read_labelled_csv2("mtcars.csv")
    expect_equal(mt2, mt4)
    unlink("mtcars.csv")
    write_labelled_tab2(mtcars, "mtcars.tab", single_file = FALSE, quote = TRUE)
    dat = readLines("mtcars.tab")
    dic= readLines("mtcars.dic.tab")
    expect_known_value(dat, "rds/write_labelled_tab2.rds", update = FALSE)
    expect_known_value(dic, "rds/write_labelled_dic_tab2.rds", update = FALSE)
    mt4 = read_labelled_tab2("mtcars.tab")
    expect_equal(mt2, mt4)
    unlink("mtcars.dic.tab")
    unlink("mtcars.tab")
    write_labelled_fst(mtcars, "mtcars.fst")
    mt2 = read_labelled_fst("mtcars.fst")
    rownames(mtcars) = NULL
    expect_equal(mtcars, mt2)
    unlink("mtcars.dic.fst")
    unlink("mtcars.fst")
    write_labelled_xlsx(mtcars, "mtcars.xlsx")
    mt2 = read_labelled_xlsx("mtcars.xlsx")
    rownames(mtcars) = NULL
    expect_equal(mtcars, mt2)
    unlink("mtcars.xlsx")
    old_write_labelled_csv(mtcars, "mtcars.csv")
    data = readLines("mtcars.csv")
    dic = readLines("mtcars.csv.dic.R")
    expect_known_value(data, "rds/old_write_labelled_data.rds", update = FALSE)
    expect_known_value(dic, "rds/old_write_labelled_dic.rds", update = FALSE)
    res1 = read_labelled_csv("mtcars.csv")
    res2 = old_read_labelled_csv("mtcars.csv")
    expect_equal(res1, res2)
    unlink("mtcars.csv")
    unlink("mtcars.csv.dic.R")
    #########
    write_labelled_csv(unlab(mtcars), "mtcars.csv")
    mt2 = read_labelled_csv("mtcars.csv")
    expect_equal(mt2, unlab(mtcars))
    unlink("mtcars.csv")
    
    write_labelled_tab(unlab(mtcars), "mtcars.tab", single_file = FALSE)
    mt2 = read_labelled_tab("mtcars.tab")
    expect_equal(mt2, unlab(mtcars))
    unlink("mtcars.tab")
    unlink("mtcars.dic.tab")
    
    write_labelled_fst(unlab(mtcars), "mtcars.fst")
    mt2 = read_labelled_fst("mtcars.fst")
    expect_equal(mt2, unlab(mtcars))
    unlink("mtcars.fst")
    
    write_labelled_xlsx(unlab(mtcars), "mtcars.xlsx")
    mt2 = read_labelled_xlsx("mtcars.xlsx")
    rownames(mtcars) = NULL
    expect_equal(mt2, unlab(mtcars))
    unlink("mtcars.xlsx")
}


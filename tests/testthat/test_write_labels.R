context("write_labels")

if(isTRUE(options("covr")[[1]])){
# if(TRUE){
    aaa = suppressWarnings(read_spss("data/7556w2_4Client_prelaunch.sav"))
    bbb = suppressWarnings(read_spss("data/2014-2016final.sav"))
    nsk = suppressWarnings(read_spss("data/NSK_all.sav"))
    nsk2 = suppressWarnings(read_spss("file:///data/NSK_all.sav"))
    expect_equal_to_reference(nsk, "rds/nsk.rds")
    expect_equal_to_reference(nsk2, "rds/nsk.rds")
    aaa2 = cbind(aaa, empty = NA, fac = factor("a"), fractional = 1.2345)
    write_labelled_spss(aaa2, "data/prelaunch.csv")
    dat = readLines("data/prelaunch.csv")
    sps = readLines("data/prelaunch.csv.sps")
    etalon_dat = readLines("data/etalon_prelaunch.csv")
    etalon_sps = readLines("data/etalon_prelaunch.csv.sps")
    # expect_identical(dat, etalon_dat)
    # expect_identical(sps, etalon_sps)
    data(iris)
    ex_iris = iris[,-5]

    colnames(ex_iris) = c("a", "a", "a", "a")

    var_lab(ex_iris[[1]]) = "v1"
    var_lab(ex_iris[[2]]) = "v2"
    var_lab(ex_iris[[3]]) = "v3"
    var_lab(ex_iris[[4]]) = "v4"
    write_labelled_spss(ex_iris, "data/labelled_iris.csv")
    dat = readLines("data/labelled_iris.csv")
    sps = readLines("data/labelled_iris.csv.sps")
    etalon_dat = readLines("data/etalon_labelled_iris.csv")
    etalon_sps = readLines("data/etalon_labelled_iris.csv.sps")
    # expect_identical(dat, etalon_dat)
    # expect_identical(sps, etalon_sps)
    
    raw_data = readRDS("data/raw.RDS")
    data(iris)
    write_labelled_csv(iris, "data/iris.csv")
    iris2 = iris
    iris2$Species = as.character(iris2$Species)
    read_iris = read_labelled_csv("data/iris.csv")
    expect_equal(read_iris, iris2)
    unlink("data/iris.csv")
    unlink("data/iris.csv.dic.R")
    unlink("data/labelled_iris.csv")
    unlink("data/labelled_iris.csv.sps")
    
    write_labelled_csv(aaa, "data/aaa.csv")
    write_labelled_csv2(aaa2, "data/aaa_csv2.csv")
    write_labelled_tab(aaa2, "data/aaa_tab.csv")
    write_labelled_tab2(aaa2, "data/aaa_tab2.csv")
    
    aaa2_csv2 = read_labelled_csv2("data/aaa_csv2.csv")
    aaa2_tab = read_labelled_tab("data/aaa_tab.csv")
    aaa2_tab2 = read_labelled_tab2("data/aaa_tab2.csv")
    
    aaa_test = cbind(aaa, empty = NA, fac = "a", fractional = 1.2345)
    obj1 = aaa2_csv2
    obj2 = aaa_test
    res = sapply(colnames(obj1), function(col) all(obj1[[col]] == obj2[[col]], na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    res = sapply(colnames(obj1), function(col) all(trimws(names(val_lab(obj1[[col]]))) == trimws(names(val_lab(obj2[[col]]))), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    res = sapply(colnames(obj1), function(col) all(var_lab(obj1[[col]]) == var_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    obj1 = aaa2_tab
    obj2 = aaa_test
    res = sapply(colnames(obj1), function(col) all(obj1[[col]] == obj2[[col]], na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    res = sapply(colnames(obj1), function(col) all(trimws(names(val_lab(obj1[[col]]))) == trimws(names(val_lab(obj2[[col]]))), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    res = sapply(colnames(obj1), function(col) all(var_lab(obj1[[col]]) == var_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    obj1 = aaa2_tab2
    obj2 = aaa_test
    res = sapply(colnames(obj1), function(col) all(obj1[[col]] == obj2[[col]], na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    res = sapply(colnames(obj1), function(col) all(trimws(names(val_lab(obj1[[col]]))) == trimws(names(val_lab(obj2[[col]]))), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    res = sapply(colnames(obj1), function(col) all(var_lab(obj1[[col]]) == var_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
 
    write_labelled_csv(bbb, "data/bbb.csv")
    write_labelled_csv(raw_data, "data/raw.csv.gz")
    
    
    a2 = read_labelled_csv("data/aaa.csv")
    b2 = read_labelled_csv("data/bbb.csv")
    raw_gz = read_labelled_csv("data/raw.csv.gz")
    
    ### data
    obj1 = aaa
    obj2 = a2
    res = sapply(colnames(obj1), function(col) all(obj1[[col]] == obj2[[col]], na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    obj1 = bbb
    obj2 = b2
    res = sapply(colnames(obj1), function(col) all(obj1[[col]] == obj2[[col]], na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    
    obj1 = raw_data
    obj2 = raw_gz
    res = sapply(colnames(obj1), function(col) all(obj1[[col]] == obj2[[col]], na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    ### val labs
    obj1 = aaa
    obj2 = a2
    res = sapply(colnames(obj1), function(col) all(val_lab(obj1[[col]]) == val_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    obj1 = bbb
    obj2 = b2
    res = sapply(colnames(obj1), function(col) all(val_lab(obj1[[col]]) == val_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    obj1 = raw_data
    obj2 = raw_gz
    res = sapply(colnames(obj1), function(col) all(val_lab(obj1[[col]]) == val_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    
    #############
    
    obj1 = aaa
    obj2 = a2
    res = sapply(colnames(obj1), function(col) all(trimws(names(val_lab(obj1[[col]]))) == trimws(names(val_lab(obj2[[col]]))), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    obj1 = bbb
    obj2 = b2
    res = sapply(colnames(obj1), function(col) all(trimws(names(val_lab(obj1[[col]]))) == trimws(names(val_lab(obj2[[col]]))), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    obj1 = raw_data
    obj2 = raw_gz
    res = sapply(colnames(obj1), function(col) all(trimws(names(val_lab(obj1[[col]]))) == trimws(names(val_lab(obj2[[col]]))), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    ### var labs
    obj1 = aaa
    obj2 = a2
    res = sapply(colnames(obj1), function(col) all(var_lab(obj1[[col]]) == var_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    obj1 = bbb
    obj2 = b2
    res = sapply(colnames(obj1), function(col) all(var_lab(obj1[[col]]) == var_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    obj1 = raw_data
    obj2 = raw_gz
    res = sapply(colnames(obj1), function(col) all(var_lab(obj1[[col]]) == var_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
    expect_equal(length(res), 0 )
    
    unlink("data/aaa.csv.dic.R")
    expect_warning(read_labelled_csv("data/aaa.csv"))
    
    unlink("data/prelaunch.csv")
    unlink("data/prelaunch.csv.sps")
    unlink("data/aaa.csv")
    unlink("data/bbb.csv")
    unlink("data/bbb.csv.dic.R")
    unlink("data/raw.csv.gz")
    unlink("data/raw.csv.gz.dic.R")
    unlink("data/aaa_csv2.csv")
    unlink("data/aaa_tab.csv")
    unlink("data/aaa_tab2.csv")
    
}
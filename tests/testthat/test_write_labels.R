context("write_labels")

if(FALSE){
    aaa = suppressWarnings(read_spss("data/7556w2_4Client_prelaunch.sav"))
    bbb = suppressWarnings(read_spss("data/2014-2016final.sav"))
    raw_data = readRDS("data/raw.RDS")
    
    write_labelled_csv(aaa, "data/aaa.csv")
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
    
    unlink("data/aaa.csv")
    unlink("data/aaa.csv.dic.R")
    unlink("data/bbb.csv")
    unlink("data/bbb.csv.dic.R")
    unlink("data/raw.csv.gz")
    unlink("data/raw.csv.gz.dic.R")
    
}
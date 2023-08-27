if(isTRUE(getOption("covr"))){ 
    cat("write_labels", "
")
    if(dir.exists("data_files")){
        # if(TRUE){
        is_windows = any(grepl("windows", tolower(sessionInfo()$running)))
        aaa = suppressWarnings(read_spss("data_files/7556w2_4Client_prelaunch.sav"))
        bbb = suppressWarnings(read_spss("data_files/2014-2016final.sav"))
        nsk = suppressWarnings(read_spss("data_files/NSK_all.sav"))
        nsk2 = suppressWarnings(read_spss("file:///data_files/NSK_all.sav"))
        if(as.numeric(version$major) ==3 && as.numeric(version$minor)<4){
            expect_equal_to_reference(nsk, "rds/nsk.rds")
            expect_equal_to_reference(nsk2, "rds/nsk.rds")
        } else {
            expect_equal_to_reference(nsk, "rds/nsk_R3.4.rds")
            expect_equal_to_reference(nsk2, "rds/nsk_R3.4.rds")        
        }
        missings = suppressWarnings(read_spss("data_files/missings.sav", use_missings = TRUE))
        expect_identical(unlab(missings$q1), c(NA, NA, 3))
        expect_identical(unlab(missings$q2), c(4, NA, NA))
        missings = suppressWarnings(read_spss("data_files/missings.sav", use_missings = FALSE))
        expect_identical(unlab(missings$q1), c(1, 2, 3))
        expect_identical(unlab(missings$q2), c(4, 5, 6))
        aaa2 = cbind(aaa, empty = NA, fac = factor("a"), fractional = 1.2345)
        write_labelled_spss(aaa2, "data_files/prelaunch.csv")
        dat = readLines("data_files/prelaunch.csv")
        sps = readLines("data_files/prelaunch.csv.sps")
        etalon_dat = readLines("data_files/etalon_prelaunch.csv")
        etalon_sps = readLines("data_files/etalon_prelaunch.csv.sps")
        # expect_identical(dat, etalon_dat)
        # expect_identical(sps[-2], etalon_sps[-2])
        data(iris)
        ex_iris = iris[,-5]
        
        colnames(ex_iris) = c("a", "a", "a", "a")
        
        var_lab(ex_iris[[1]]) = "v1"
        var_lab(ex_iris[[2]]) = "v2"
        var_lab(ex_iris[[3]]) = "v3"
        var_lab(ex_iris[[4]]) = "v4"
        write_labelled_spss(ex_iris, "data_files/labelled_iris.csv")
        dat = readLines("data_files/labelled_iris.csv")
        sps = readLines("data_files/labelled_iris.csv.sps")
        etalon_dat = readLines("data_files/etalon_labelled_iris.csv")
        etalon_sps = readLines("data_files/etalon_labelled_iris.csv.sps")
        expect_identical(dat, etalon_dat)
        sps = sps[!grepl("OUTFILE", sps)]
        etalon_sps = etalon_sps[!grepl("OUTFILE", etalon_sps)]
        # expect_identical(sps[-2], etalon_sps[-2])
        
        raw_data = readRDS("data_files/raw.rds")
        data(iris)
        write_labelled_csv(iris, "data_files/iris.csv")
        iris2 = iris
        iris2$Species = as.character(iris2$Species)
        read_iris = read_labelled_csv("data_files/iris.csv")
        expect_equal(read_iris, iris2)
        unlink("data_files/iris.csv")
        unlink("data_files/iris.csv.dic.R")
        unlink("data_files/labelled_iris.csv")
        unlink("data_files/labelled_iris.csv.sps")
        
        write_labelled_csv(aaa, "data_files/aaa.csv")
        write_labelled_csv2(aaa2, "data_files/aaa_csv2.csv")
        write_labelled_tab(aaa2, "data_files/aaa_tab.csv")
        write_labelled_tab2(aaa2, "data_files/aaa_tab2.csv")
        
        aaa2_csv2 = read_labelled_csv2("data_files/aaa_csv2.csv")
        aaa2_tab = read_labelled_tab("data_files/aaa_tab.csv")
        aaa2_tab2 = read_labelled_tab2("data_files/aaa_tab2.csv")
        
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

            write_labelled_csv(bbb, "data_files/bbb.csv")
            b2 = read_labelled_csv("data_files/bbb.csv")
            obj1 = bbb
            obj2 = b2
            res = sapply(colnames(obj1), function(col) all(obj1[[col]] == obj2[[col]], na.rm = TRUE)) %d% TRUE
            expect_equal(length(res), 0 )
            obj1 = bbb
            obj2 = b2
            res = sapply(colnames(obj1), function(col) all(val_lab(obj1[[col]]) == val_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
            expect_equal(length(res), 0 )
            obj1 = bbb
            obj2 = b2
            res = sapply(colnames(obj1), function(col) all(trimws(names(val_lab(obj1[[col]]))) == trimws(names(val_lab(obj2[[col]]))), na.rm = TRUE)) %d% TRUE
            expect_equal(length(res), 0 )
            obj1 = bbb
            obj2 = b2
            res = sapply(colnames(obj1), function(col) all(var_lab(obj1[[col]]) == var_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
            expect_equal(length(res), 0 )
            unlink("data_files/bbb.csv")
            unlink("data_files/bbb.csv.dic.R")
            # write_labelled_csv(raw_data, "data_files/raw.csv.gz")

        
        a2 = read_labelled_csv("data_files/aaa.csv")
        
        # raw_gz = read_labelled_csv("data_files/raw.csv.gz")
        
        ### data
        obj1 = aaa
        obj2 = a2
        res = sapply(colnames(obj1), function(col) all(obj1[[col]] == obj2[[col]], na.rm = TRUE)) %d% TRUE
        expect_equal(length(res), 0 )
        
        # obj1 = raw_data
        # obj2 = raw_gz
        # res = sapply(colnames(obj1), function(col) all(obj1[[col]] == obj2[[col]], na.rm = TRUE)) %d% TRUE
        # expect_equal(length(res), 0 )
        
        ### val labs
        obj1 = aaa
        obj2 = a2
        res = sapply(colnames(obj1), function(col) all(val_lab(obj1[[col]]) == val_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
        expect_equal(length(res), 0 )
        
        
        # obj1 = raw_data
        # obj2 = raw_gz
        # res = sapply(colnames(obj1), function(col) all(val_lab(obj1[[col]]) == val_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
        # expect_equal(length(res), 0 )
        
        
        #############
        
        obj1 = aaa
        obj2 = a2
        res = sapply(colnames(obj1), function(col) all(trimws(names(val_lab(obj1[[col]]))) == trimws(names(val_lab(obj2[[col]]))), na.rm = TRUE)) %d% TRUE
        expect_equal(length(res), 0 )
        
        
        # obj1 = raw_data
        # obj2 = raw_gz
        # res = sapply(colnames(obj1), function(col) all(trimws(names(val_lab(obj1[[col]]))) == trimws(names(val_lab(obj2[[col]]))), na.rm = TRUE)) %d% TRUE
        # expect_equal(length(res), 0 )
        
        ### var labs
        obj1 = aaa
        obj2 = a2
        res = sapply(colnames(obj1), function(col) all(var_lab(obj1[[col]]) == var_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
        expect_equal(length(res), 0 )
        
        
        # obj1 = raw_data
        # obj2 = raw_gz
        # res = sapply(colnames(obj1), function(col) all(var_lab(obj1[[col]]) == var_lab(obj2[[col]]), na.rm = TRUE)) %d% TRUE
        # expect_equal(length(res), 0 )
        
        ####### wrong slash
        w = read_spss("data_files/wrong_slash.sav")
        
        write_labelled_csv(w, "data_files/wrong_slash.csv")
        w2 = read_labelled_csv("data_files/wrong_slash.csv")
        class(w2$q1) = c("labelled", "numeric")
        expect_true(all.equal(w, w2))
        
        a = sheet(q = 'aaa"aaa"aaa')
        write_labelled_csv(a, "data_files/qmethod.csv")
        b = read_labelled_csv("data_files/qmethod.csv")
        expect_identical(a, b)
        
        b = read_labelled_csv("data_files/qmethod.csv", undouble_quotes = FALSE)
        a_res = fread("data_files/qmethod.csv", header = TRUE, data.table = FALSE)
        expect_identical(a_res, b)
        
        a = sheet(q = 'aaa\naaa')
        a_res = sheet(q = 'aaa aaa')
        write_labelled_csv(a, "data_files/newline.csv")
        b = read_labelled_csv("data_files/newline.csv")
        expect_identical(a_res, b)
        
        write_labelled_csv(a, "data_files/newline.csv", remove_new_lines = FALSE)
        b = read_labelled_csv("data_files/newline.csv")
        expect_identical(a, b)
        
        a = sheet(.err = 1.5, err = "a")
        a_res = sheet(err = 1.5, err_1 = "a")
        write_labelled_spss(a, "data_files/var_with_dot.csv")
        b = fread("data_files/var_with_dot.csv", data.table = FALSE)
        expect_identical(a_res, b)
        
        unlink("data_files/var_with_dot.csv")
        unlink("data_files/var_with_dot.csv.sps")
        
        unlink("data_files/newline.csv")
        unlink("data_files/newline.csv.dic.R")
        unlink("data_files/qmethod.csv")
        unlink("data_files/qmethod.csv.dic.R")
        unlink("data_files/wrong_slash.csv")
        unlink("data_files/wrong_slash.csv.dic.R")
        unlink("data_files/prelaunch.csv")
        unlink("data_files/prelaunch.csv.sps")
        unlink("data_files/aaa.csv")
        
        # unlink("data_files/raw.csv.gz")
        # unlink("data_files/raw.csv.gz.dic.R")
        unlink("data_files/aaa_csv2.csv")
        unlink("data_files/aaa_tab.csv")
        unlink("data_files/aaa_tab2.csv")
        cat("write_labelled_xlsx", "
")
        aaa = suppressWarnings(read_spss("data_files/7556w2_4Client_prelaunch.sav"))
        bbb = suppressWarnings(read_spss("data_files/2014-2016final.sav"))
        nsk = suppressWarnings(read_spss("data_files/NSK_all.sav"))
        write_labelled_xlsx(aaa, "xlsx.xlsx")
        res = read_labelled_xlsx("xlsx.xlsx")
        expect_equal(drop_c(aaa), drop_c(res))
        write_labelled_xlsx(bbb, "xlsx.xlsx", remove_repeated = TRUE, use_references = FALSE)
        res = read_labelled_xlsx("xlsx.xlsx")
        expect_equal(drop_c(bbb), drop_c(res))
        
        write_labelled_xlsx(nsk, "xlsx.xlsx")
        res = read_labelled_xlsx("xlsx.xlsx")
        expect_equal(drop_c(nsk), drop_c(res))
        expect_error(read_labelled_xlsx("xlsx.xlsx", data_sheet = "wow"))
        res = read_labelled_xlsx("xlsx.xlsx", dict_sheet =  42)
        expect_equal(drop_c(unlab(nsk)), drop_c(res))
        write_labelled_xlsx(unlab(nsk), "xlsx.xlsx")
        res = read_labelled_xlsx("xlsx.xlsx")
        expect_equal(drop_c(unlab(nsk)), drop_c(res))
        unlink("xlsx.xlsx")
        
    }
}
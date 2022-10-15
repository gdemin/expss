if(isTRUE(getOption("covr"))){ 

    
    context("cro_fun extended")
    suppressWarnings(RNGversion("3.5.0"))
    
    
    data(mtcars)
    mtcars = apply_labels(mtcars,
                          mpg = "Miles/(US) gallon",
                          cyl = "Number of cylinders",
                          disp = "Displacement (cu.in.)",
                          hp = "Gross horsepower",
                          drat = "Rear axle ratio",
                          wt = "Weight (lb/1000)",
                          qsec = "1/4 mile time",
                          vs = "V/S",
                          vs = c("Straight" = 0, "V" = 1),
                          am = "Transmission (0 = automatic, 1 = manual)",
                          am = c(" automatic" = 0, " manual" =  1),
                          gear = "Number of forward gears",
                          carb = "Number of carburetors"
    )
    
    
    # row_labels = c("row_vars", "row_vars_values", "summary_vars", "fun_names", "stat_names"),
    # col_labels = c("col_vars", "col_vars_values")
    
    
    #######
    expect_error(
        cro_fun(mtcars %>% columns(-"cyl", -"am"), col_vars = mtcars$am, fun = mean, weight = 2)
    )
    
    
    
    expect_equal_to_reference(
        cro_fun(mtcars %>% columns(-"cyl", -"am"),
                col_vars = mtcars$am, fun = combine_functions(w_mean), weight = 2),
        "rds/table_summary0.rds",  update = FALSE)
    
    expect_equal_to_reference(
        cross_fun(mtcars, ..[!perl("cyl|am")], col_vars = am, 
                     fun = combine_functions(w_mean), 
                     weight = 2
        ),
        "rds/table_summary0.rds",  update = FALSE)
    
    
    
    expect_equal_to_reference(
        cro_fun(mtcars %>% columns(-"cyl", -"am"), col_vars = list("Total", mtcars$am), 
                fun = combine_functions(mean)),
        "rds/table_summary1.rds",  update = FALSE
    )
    
    
    add_val_lab(mtcars$am) = c(HardToSay = 3)
    expect_equal_to_reference(
        cro_fun(mtcars %>% columns(-"cyl", -"am"), col_vars = list("Total", mtcars$am), 
                fun = combine_functions(mean))
        ,"rds/table_summary2.rds",  update = FALSE
    )
    
    data(mtcars)
    mtcars = apply_labels(mtcars,
                          mpg = "Miles/(US) gallon",
                          cyl = "Number of cylinders",
                          disp = "Displacement (cu.in.)",
                          hp = "Gross horsepower",
                          drat = "Rear axle ratio",
                          wt = "Weight (lb/1000)",
                          qsec = "1/4 mile time",
                          vs = "V/S",
                          vs = c("V" = 1,
                                 "Straight" = 0),
                          am = "Transmission (0 = automatic, 1 = manual)",
                          am = c(" automatic" = 0,
                                 " manual"=1),
                          gear = "Number of forward gears",
                          carb = "Number of carburetors"
    )
    
    add_val_lab(mtcars$am) = c(HardToSay = 3)
    expect_equal_to_reference(
        cro_fun(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
                fun = combine_functions(mean))
        ,"rds/table_summary3.rds",  update = FALSE
    )
    
    expect_equal_to_reference(
        cro_fun(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
                fun = mean)
        ,"rds/table_summary4.rds",  update = FALSE
    )
    
    expect_equal_to_reference(
        cro_fun_df(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
                   fun = function(x) {
                       res = t(colMeans(x) )
                       rownames(res) = "mean"
                       res
                   })
        ,"rds/table_summary5.rds",  update = FALSE
    )
    
    expect_equal_to_reference(
        cross_fun_df(mtcars, ..[!perl("vs|am")], 
                        col_vars = list("Total", mtcars$am),
                        fun = function(x) {
                            res = t(colMeans(x) )
                            rownames(res) = "mean"
                            res
                        })
        ,"rds/table_summary5.rds",  update = FALSE
    )
    
    # expect_equal_to_reference(
    # cro_fun(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
    #               row_labels = c("row_vars", "row_vars_values", "stat_names"),
    #               col_labels = c("col_vars", "summary_vars", "col_vars_values"), fun = mean)
    # ,"rds/table_summary6.rds",  update = FALSE
    # )
    
    
    ########## rowlabels
    
    # expect_equal_to_reference(
    # cro_fun(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
    #               row_vars = mtcars$vs,
    #              fun = combine_functions(mean))
    # ,"rds/table_summary7.rds",  update = FALSE
    # )
    
    
    # expect_equal_to_reference(
    # cro_fun(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
    #               row_vars = mtcars$vs,
    #               row_labels = c("row_vars", "row_vars_values", "summary_vars"),
    #               col_labels = c("col_vars", "col_vars_values", "stat_names"), fun = mean)
    # ,"rds/table_summary8.rds",  update = FALSE
    # )
    
    # expect_equal_to_reference(
    # cro_fun(mtcars %>% columns(-"vs", -"am"), 
    #               col_vars = mtcars$vs,
    #         row_vars = list("Total", mtcars$am), 
    #         fun = mean)
    # ,"rds/table_summary9.rds",  update = FALSE
    # )
    
    # expect_equal_to_reference(
    # cro_fun(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
    #               row_vars = mtcars$vs,
    #               row_labels = c("row_vars", "row_vars_values",  "col_vars_values", "summary_vars", "stat_names"),
    #               col_labels = c("col_vars"), fun = mean)
    # ,"rds/table_summary10.rds",  update = FALSE
    # )
    ##############################
    add_val_lab(mtcars$vs) = c("Don't know" = 88)
    
    # expect_equal_to_reference(
    # cro_fun(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
    #               row_vars = mtcars$vs, fun = combine_functions(mean))
    # ,"rds/table_summary11.rds",  update = FALSE
    # )
    
    # expect_equal_to_reference(
    # cro_fun(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
    #               row_vars = mtcars$vs,
    #               row_labels = c("row_vars", "row_vars_values", "summary_vars"),
    #               col_labels = c("col_vars", "col_vars_values", "stat_names"), fun = mean)
    # ,"rds/table_summary12.rds",  update = FALSE
    # )
    
    
    # expect_equal_to_reference(
    # cro_fun(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
    #               row_vars = mtcars$vs,
    #               row_labels = c("row_vars", "row_vars_values", "summary_vars", "stat_names"),
    #               col_labels = c("col_vars", "col_vars_values", "summary_vars"), fun = mean)
    # ,"rds/table_summary13.rds",  update = FALSE
    # )
    
    
    expect_equal_to_reference(
        cro_fun_df(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
                   row_vars = mtcars$vs, 
                   fun = function(x) {
                       res = t(colMeans(x) )
                       rownames(res) = "mean"
                       res
                   })
        ,"rds/table_summary14.rds",  update = FALSE
    )
    
    
    # expect_equal_to_reference(
    # cro_fun(mtcars %>% columns(-"vs", -"am"), col_vars = list("Total", mtcars$am),
    #               row_vars = mtcars$vs,
    #               row_labels = c("row_vars", "row_vars_values",  "col_vars_values", "summary_vars", "stat_names"),
    #               col_labels = c("col_vars"), fun = mean)
    # ,"rds/table_summary15.rds",  update = FALSE
    # )
    
    #########
    data("product_test")
    w = product_test
    codeframe_likes = num_lab("
                          1 Liked everything
                          2 Disliked everything
                          3 Chocolate
                          4 Appearance
                          5 Taste
                          6 Stuffing
                          7 Nuts
                          8 Consistency
                          98 Other
                          99 Hard to answer
                          ")
    
    w = w %>% 
        let( 
        # recode age by groups
        age_cat = if_val(s2a, lo %thru% 25 ~ 1, lo %thru% hi ~ 2)
        
    ) %>% 
        # Apply labels
        apply_labels(
            c1 = "Preferences",
            c1 = num_lab("
                          1 VSX123 
                          2 SDF456
                          3 Hard to say
                          "),
            
            age_cat = "Age",
            age_cat = c("18 - 25" = 1, "26 - 35" = 2),
            
            a1_1 = "Likes. VSX123",
            b1_1 = "Likes. SDF456",
            a1_1 = codeframe_likes,
            b1_1 = codeframe_likes,
            
            a22 = "Overall quality. VSX123",
            b22 = "Overall quality. SDF456",
            a22 = num_lab("
                           1 Extremely poor 
                           2 Very poor
                           3 Quite poor
                           4 Neither good, nor poor
                           5 Quite good
                           6 Very good
                           7 Excellent
                           "),
            b22 = num_lab("
                           1 Extremely poor 
                           2 Very poor
                           3 Quite poor
                           4 Neither good, nor poor
                           5 Quite good
                           6 Very good
                           7 Excellent
                           ")
        )
    
    expect_equal_to_reference(
        calc(w, cro_fun(list(a22, b22), col_vars = mrset(a1_1 %to% a1_6), fun = combine_functions(w_mean)))
        , "rds/table_summary16.rds",  update = FALSE
    )
    expect_equal_to_reference(
        calc(w, cro_fun(list(a22, b22), col_vars = list(list(mrset(a1_1 %to% a1_6))), 
                        fun = combine_functions(w_mean)))
        , "rds/table_summary16.rds",  update = FALSE
    )
    
    expect_equal_to_reference(
        calc(w, cro_fun(list(a22, b22), col_vars = mdset(as.dichotomy(a1_1 %to% a1_6, keep_unused = TRUE)),
                        fun = combine_functions(w_mean)))
        , "rds/table_summary16.rds",  update = FALSE
    )
    
    expect_equal_to_reference(
        calc(w, cro_fun(list(a22, b22), 
                        col_vars = list(list(mdset(as.dichotomy(a1_1 %to% a1_6, keep_unused = TRUE)))),
                        fun = combine_functions(w_mean)))
        , "rds/table_summary16.rds",  update = FALSE
    )
    
    expect_equal_to_reference(
        calc(w, cro_fun(list(a22, b22),
                        col_vars = list(total(label = "Total")),
                        row_vars = list(mrset(a1_1 %to% a1_6)),
                        fun = combine_functions(w_mean)))
        , "rds/table_summary17.rds",  update = FALSE
    )
    expect_identical(
        calc(w, cro_fun(list(a22, b22), col_vars = list("Total"),
                        row_vars = list(list(mrset(a1_1 %to% a1_6))), ### list(list)
                        fun = combine_functions(w_mean)))
        , calc(w, cro_fun(list(a22, b22), 
                          col_vars = list("Total"), 
                          row_vars = list(mrset(a1_1 %to% a1_6)), 
                          fun = combine_functions(w_mean)))
    )
    
    expect_identical(
        calc(w, cro_fun(list(a22, b22), col_vars = list("Total"),
                        row_vars =  mdset(as.dichotomy(a1_1 %to% a1_6, keep_unused = TRUE)),
                        fun = combine_functions(w_mean)))
        , calc(w, cro_fun(list(a22, b22), 
                          col_vars = list("Total"), 
                          row_vars = list(mrset(a1_1 %to% a1_6)), 
                          fun = combine_functions(w_mean)))
    )
    expect_identical(
        calc(w, cro_fun(list(a22, b22), col_vars = list("Total"),
                        row_vars =  list(list(mdset(as.dichotomy(a1_1 %to% a1_6, keep_unused = TRUE)))),
                        fun = combine_functions(w_mean)))
        , calc(w, cro_fun(list(a22, b22), 
                          col_vars = list("Total"), 
                          row_vars = list(mrset(a1_1 %to% a1_6)), 
                          fun = combine_functions(w_mean)))
    )
    # calc(w, fre(a1_1 %to% a1_6))
    context("cro_mean_sd_n")
    
    data(mtcars)
    mtcars = apply_labels(mtcars,
                          mpg = "Miles/(US) gallon",
                          cyl = "Number of cylinders",
                          disp = "Displacement (cu.in.)",
                          hp = "Gross horsepower",
                          drat = "Rear axle ratio",
                          wt = "Weight (lb/1000)",
                          qsec = "1/4 mile time",
                          vs = "V/S",
                          vs = c("V" = 1,
                                 "Straight" = 0),
                          am = "Transmission (0 = automatic, 1 = manual)",
                          am = c(" automatic" = 0,
                                 " manual"=1),
                          gear = "Number of forward gears",
                          carb = "Number of carburetors"
    )
    
    expect_equal_to_reference(
        with(mtcars, cro_mean_sd_n(list(mpg, disp, wt), list(total(), am, vs))),
        "rds/cro_mean_sd_n1.rds",  update = FALSE
    )
    
    
    expect_equal_to_reference(
        cross_mean_sd_n(mtcars, list(mpg, disp, wt), list(total(), am, vs)),
        "rds/cro_mean_sd_n1.rds",  update = FALSE
    )
    
    expect_error(cross_mean_sd_n(mtcars, list(mpg, disp, wt), list(total(), am, vs), labels = 1))
    expect_error(cross_mean_sd_n(mtcars, list(mpg, disp, wt), list(total(), am, vs), 
                                    labels = c("", "", "")))
    
    expect_equal_to_reference(
         cross_mean_sd_n(mtcars, list(mpg, disp, wt), list(total(), am, vs), weight = 0.1,
                                    weighted_valid_n = TRUE),
        "rds/cro_mean_sd_n2.rds",  update = FALSE
    )
    
    
    expect_equal_to_reference(
        cross_mean_sd_n(mtcars, 
                           list(mpg, disp, wt), 
                           list(total(), am, vs), 
                           labels = c("m", "s", "n"),
                           weight = 0.1,
                           weighted_valid_n = TRUE),
        "rds/cro_mean_sd_n3.rds",  update = FALSE
    )
    
    
    mtcars2 = mtcars
    
    mtcars2$mpg[1:5] = NA
    set.seed(123)
    mtcars2$ww = runif(NROW(mtcars), 0.5, 1.5)
    mtcars2$ww[6:10] = NA
    mtcars2$empty = NA
    expect_equal_to_reference(cross_mean_sd_n(mtcars2, 
                                                 empty, 
                                                 list(total(), am, vs),
                                                 weight = ww,
                                                 weighted_valid_n = TRUE),
                              "rds/cro_mean_sd_n4.rds",  update = FALSE
    )
    
    expect_equal_to_reference(
        cross_mean_sd_n(mtcars2, 
                           empty, 
                           list(total(), am, vs),
                           weight = ww,
                           weighted_valid_n = FALSE),
        "rds/cro_mean_sd_n4_1.rds",  update = FALSE
    )
    
    expect_equal_to_reference(
        tab_cells(mtcars2) %>% 
            tab_cells(empty) %>% 
            tab_cols(total(), am, vs) %>% 
            tab_weight(ww) %>% 
            tab_stat_mean_sd_n(weighted_valid_n = FALSE) %>% 
            tab_pivot(),
        "rds/cro_mean_sd_n4_1.rds",  update = FALSE
    )
    mtcars2$empty = NA_real_
    expect_equal_to_reference(cross_mean_sd_n(mtcars2, 
                                                 empty, 
                                                 list(total(), am, vs),
                                                 weight = ww,
                                                 weighted_valid_n = TRUE),
                              "rds/cro_mean_sd_n4.rds",  update = FALSE
    )
    
    expect_equal_to_reference(cross_mean_sd_n(mtcars2, 
                                                 empty, 
                                                 list(total(), am, vs),
                                                 weight = ww,
                                                 weighted_valid_n = FALSE),
                              "rds/cro_mean_sd_n4_1.rds",  update = FALSE
    )
    
    context("cro_fun unsafe labels")
    
    fun = function(x) c(sum(x), mean(x), length(x))
    
    fun2 = combine_functions(sum = sum, mean = mean, length = length)
    
    expect_identical(cro_fun(list(mtcars$mpg, mtcars$disp, mtcars$wt), 
                             list(total(), mtcars$am, mtcars$vs), 
                             fun = fun,
                             unsafe = c("sum", "mean", "length")),
                     cro_fun(list(mtcars$mpg, mtcars$disp, mtcars$wt), 
                             list(total(), mtcars$am, mtcars$vs), 
                             fun = fun2)
    )
    
    fun2 = combine_functions("1" = sum, "2" = mean, "3" = length)
    
    expect_identical(cro_fun(list(mtcars$mpg, mtcars$disp, mtcars$wt), 
                             list(total(), mtcars$am, mtcars$vs), 
                             fun = fun,
                             unsafe = 1:3),
                     cro_fun(list(mtcars$mpg, mtcars$disp, mtcars$wt), 
                             list(total(), mtcars$am, mtcars$vs), 
                             fun = fun2)
    )
    
    expect_error(cro_fun(list(mtcars$mpg, mtcars$disp, mtcars$wt), 
                         list(total(), mtcars$am, mtcars$vs), 
                         fun = fun,
                         unsafe = c("sum", "mean", "length","ffaafa")))
    
    expect_error(cro_fun(list(mtcars$mpg, mtcars$disp, mtcars$wt), 
                         list(total(), mtcars$am, mtcars$vs), 
                         fun = fun,
                         unsafe = c("sum", "length", "length")))
    
    
    #################
    
    expect_equal(
        cro_fun(c(1:3, NA, NA, NA), c(1,1,1,2,2,2), weight = 1.9, fun = w_sd),
        structure(list(row_labels = "c(1:3, NA, NA, NA)", `c(1, 1, 1, 2, 2, 2)|1` = 0.899172196132572, 
                       `c(1, 1, 1, 2, 2, 2)|2` = NA_real_), .Names = c("row_labels", 
                                                                       "c(1, 1, 1, 2, 2, 2)|1", "c(1, 1, 1, 2, 2, 2)|2"), row.names = c(NA, 
                                                                                                                                        -1L), class = c("etable", "data.frame"))
    )
    
    
    # TODO 
    # a = 1:20
    # 
    # cro_mean_sd_n(list(a))
    # cro_fun(list(a), fun = sum)
    # cro_fun_df(a, fun = colSums)
    # cro(list(1:20))
    
    # bug 2019.01.23
    
    res = cro_mean_sd_n(1:4, total(), row_vars = set_val_lab(rep(NA, 4), c(a=1, b =2, c= 3, d= 4)))
    correct = structure(list(row_labels = c("set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|a|1:4|Mean", 
                                            "set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|a|1:4|Std. dev.", 
                                            "set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|a|1:4|Unw. valid N", 
                                            "set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|b|1:4|Mean", 
                                            "set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|b|1:4|Std. dev.", 
                                            "set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|b|1:4|Unw. valid N", 
                                            "set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|c|1:4|Mean", 
                                            "set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|c|1:4|Std. dev.", 
                                            "set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|c|1:4|Unw. valid N", 
                                            "set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|d|1:4|Mean", 
                                            "set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|d|1:4|Std. dev.", 
                                            "set_val_lab(rep(NA, 4), c(a = 1, b = 2, c = 3, d = 4))|d|1:4|Unw. valid N"
    ), `#Total` = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, 
                    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
    )), row.names = c(NA, -12L), class = c("etable", "data.frame"
    ))
    expect_identical(res, correct)
    
}


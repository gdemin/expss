context("significance tests")
if(isTRUE(getOption("covr"))) { 

    
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
    
    tbl = t(table(mtcars$am, mtcars$vs))
    
    expect_equal(compare_proportions(12/18, 7/14, 18, 14),
                 prop.test(tbl, correct = FALSE)$p.value)
    
    tbl[2,1] = 0
    
    expect_equal(compare_proportions(12/18, 0/7, 18, 7),
                 suppressWarnings(prop.test(tbl, correct = FALSE))$p.value)
    
    t_test_pval = t.test(mpg ~ am, data = mtcars)$p.value 
    
    res = with(mtcars, 
               compare_means(
                   mean(mpg[am==0]), mean(mpg[am==1]), 
                   sd(mpg[am==0]),  sd(mpg[am==1]),
                   length(mpg[am==0]), length(mpg[am==1])
               ))
    
    expect_equal(res, t_test_pval)
    
    
    res = with(mtcars, 
               compare_means(
                   c(mean(mpg[am==0]), mean(mpg[am==0])), c(mean(mpg[am==1]), mean(mpg[am==1])), 
                   c(sd(mpg[am==0]), sd(mpg[am==0])),  c(sd(mpg[am==1]), sd(mpg[am==1])),
                   c(length(mpg[am==0]), length(mpg[am==0])), c(length(mpg[am==1]), length(mpg[am==1]))
               ))
    
    expect_equal(res, c(t_test_pval, t_test_pval))
    
    t_test_pval = t.test(mpg ~ am, data = mtcars, var.equal = TRUE)$p.value 
    
    res = with(mtcars, 
               compare_means(
                   c(mean(mpg[am==0]), mean(mpg[am==0])), c(mean(mpg[am==1]), mean(mpg[am==1])), 
                   c(sd(mpg[am==0]), sd(mpg[am==0])),  c(sd(mpg[am==1]), sd(mpg[am==1])),
                   c(length(mpg[am==0]), length(mpg[am==0])), c(length(mpg[am==1]), length(mpg[am==1])),
                   var_equal = TRUE
               ))
    
    expect_equal(res, c(t_test_pval, t_test_pval))
    
    context("significance_cpct")
    
    mtcars_table = cro_cpct(list(mtcars$cyl, mtcars$gear),
                            list(total(), mtcars$vs, mtcars$am))
    
    
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, keep = "none"),
        "rds/signif_cpct1.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, inequality_sign = TRUE,  keep = "none"),
        "rds/signif_cpct1_greater_sign.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, keep = "none", subtable_marks = "less"),
        "rds/signif_cpct1_less.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, inequality_sign = TRUE,
                          keep = "none", subtable_marks = "less"),
        "rds/signif_cpct1_less_sign.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, keep = "none", subtable_marks = "both"),
        "rds/signif_cpct1_both.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, inequality_sign = FALSE,
                          keep = "none", subtable_marks = "both"),
        "rds/signif_cpct1_both_no_signs.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, keep = "none", na_as_zero = TRUE),
        "rds/signif_cpct2.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_cpct(mtcars_table,
                          keep = "none",
                          sig_labels = paste0("(",letters, ")")),
        "rds/signif_cpct3.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_cpct(mtcars_table,
                          keep = "none",
                          sig_labels = NULL),
        "rds/signif_cpct4.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_cpct(mtcars_table, keep = "none",
                          compare_type = c("subtable", "first_column")),
        "rds/signif_cpct5.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_cpct(mtcars_table, keep = "none",
                          compare_type = c("subtable", "adjusted_first_column")),
        "rds/signif_cpct6.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_cpct(mtcars_table, keep = "none",
                          compare_type = c("subtable", "adjusted_first_column"),
                          na_as_zero = TRUE
        ),
        "rds/signif_cpct7.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, keep = "none"
                          , compare_type = c("previous_column")),
        "rds/signif_cpct8.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, keep = "none",
                          compare_type = c("previous_column"),
                          na_as_zero = TRUE),
        "rds/signif_cpct9.rds",  update = FALSE)
    
    #####################
    expect_equal_to_reference(
        significance_cpct(mtcars_table, keep = "bases"),
        "rds/signif_cpct10.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table),
        "rds/signif_cpct11.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table),
        "rds/signif_cpct11.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, delta_cpct = 50),
        "rds/signif_cpct11_delta.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, keep = "percent"),
        "rds/signif_cpct12.rds",  update = FALSE)
    
    
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, 
                          compare_type = c("subtable", "first_column")),
        "rds/signif_cpct13.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_cpct(mtcars_table, 
                          compare_type = c("subtable", "adjusted_first_column")),
        "rds/signif_cpct14.rds",  update = FALSE)
    expect_equal_to_reference(
        significance_cpct(mtcars_table,
                          compare_type = c("subtable", "adjusted_first_column"),
                          na_as_zero = TRUE
        ),
        "rds/signif_cpct15.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, 
                          compare_type = c("previous_column")),
        "rds/signif_cpct16.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, 
                          delta_cpct = 65,
                          compare_type = c("previous_column")),
        "rds/signif_cpct16_delta.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, 
                          delta_cpct = 65,
                          na_as_zero = TRUE,
                          compare_type = c("previous_column")),
        "rds/signif_cpct16_delta2.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, 
                          compare_type = c("previous_column"), 
                          na_as_zero = TRUE),
        "rds/signif_cpct17.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, 
                          compare_type = c("previous_column"), 
                          na_as_zero = TRUE,
                          min_base = 14),
        "rds/signif_cpct18.rds",  update = FALSE)
    
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, 
                          na_as_zero = TRUE,
                          sig_level = 0.9),
        "rds/signif_cpct19.rds",  update = FALSE)
    
    #####################
    mtcars_table2 = cro_cpct(list(mtcars$vs, mtcars$am),
                             list(total(), mtcars$cyl))
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table2, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"),
                          bonferroni = FALSE,
                          na_as_zero = FALSE, 
                          sig_level = 0.004),
        "rds/signif_cpct20.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table2, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"),
                          sig_level = 0.1),
        "rds/signif_cpct20greater.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table2, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"),
                          subtable_marks = "less",
                          sig_level = 0.1),
        "rds/signif_cpct20less.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table2, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"),
                          subtable_marks = "both",
                          sig_level = 0.1),
        "rds/signif_cpct20both.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table2, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"),
                          bonferroni = TRUE,
                          na_as_zero = FALSE, 
                          sig_level = 0.004),
        "rds/signif_cpct20boneferrony.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table2, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"),
                          bonferroni = FALSE,
                          na_as_zero = FALSE, 
                          sig_level = 0.002),
        "rds/signif_cpct20_2.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table2, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"),
                          bonferroni = TRUE,
                          na_as_zero = FALSE, 
                          sig_level = 0.002),
        "rds/signif_cpct20boneferrony_2.rds",  update = FALSE)
    
    #####################
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, 
                          compare_type = c("subtable", "first_column"), 
                          digits = 2),
        "rds/signif_cpct21.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table, 
                          compare_type = c("subtable", "first_column"), 
                          delta_cpct = 30,
                          digits = 2),
        "rds/signif_cpct21_delta.rds",  update = FALSE)
    
    expect_error(
        significance_cpct(mtcars_table, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"), 
                          total_marker = "@"))
    
    mtcars_table2 = mtcars_table
    mtcars_table2[[1]] = gsub("#", "@", mtcars_table2[[1]])
    expect_equal_to_reference(
        significance_cpct(mtcars_table2, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"), 
                          total_marker = "@"),
        "rds/signif_cpct22.rds",  update = FALSE)
    
    expect_error(
        significance_cpct(mtcars_table2, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"), 
                          total_marker = "@",
                          total_row = 2))
    
    mtcars_table3 = cro_cpct(list(mtcars$cyl, mtcars$gear),
                             list(total(), mtcars$vs, mtcars$am),
                             weight = 1/100,
                             total_row_position = "above",
                             total_statistic = c("w_cases", "u_cases")
    )
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable")),
        "rds/signif_cpct23.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"), 
                          total_row = 2),
        "rds/signif_cpct24.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3, 
                          compare_type = c("first_column", 
                                           "previous_column",
                                           "subtable"), 
                          total_row = "#Total cases"),
        "rds/signif_cpct24.rds",  update = FALSE)
    
    mtcars_table3 = cro_cpct(list(mtcars$cyl, mtcars$gear),
                             list(total(), mtcars$vs, mtcars$am),
                             total_row_position = "above"
    )
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3, keep = "none"),
        "rds/signif_cpct1a.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3, keep = "none", sig_labels = "a"),
        "rds/signif_cpct25.rds",  update = FALSE)
    
    mtcars_table3 = cro_cpct(list(mtcars$cyl, mtcars$gear),
                             unvr(list(total(), mtcars$vs, mtcars$am)),
                             total_row_position = "above"
    )
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3,
                          compare_type = c("previous_column", "subtable"),
                          sig_labels = "a"),
        "rds/signif_cpct26.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3,
                          compare_type = c("previous_column", "subtable"),
                          sig_labels = NULL),
        "rds/signif_cpct26a.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3,
                          compare_type = c("previous_column", "subtable")),
        "rds/signif_cpct27.rds",  update = FALSE)
    
    mtcars_table3 = cro_cpct(list(mtcars$cyl),
                             unvr(list(total(), mtcars$vs, mtcars$am)),
                             total_row_position = "above"
    )
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3,
                          compare_type = c("previous_column", "subtable")),
        "rds/signif_cpct28.rds",  update = FALSE)
    
    colnames(mtcars_table3) = rep("", NCOL(mtcars_table3))
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3,
                          compare_type = c("previous_column", "subtable")),
        "rds/signif_cpct29.rds",  update = FALSE)
    
    expect_error(
        significance_cpct(mtcars_table3,
                          compare_type = c("first_column", "adjusted_first_column"))
    )
    
    mtcars_table3 = cro_cpct(list(mtcars$cyl),
                             (list(total(), mtcars$vs, mtcars$am)),
                             total_row_position = "above"
    )
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3[ ,1],
                          compare_type = c("first_column", "subtable")),
        "rds/signif_cpct30.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3[ ,1:2],
                          compare_type = c("first_column", "subtable")),
        "rds/signif_cpct31.rds",  update = FALSE)
    
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3[ ,1:3],
                          compare_type = c("first_column", "subtable")),
        "rds/signif_cpct32.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table3[ ,c(1:2,4,5)],
                          compare_type = c("first_column", "subtable")),
        "rds/signif_cpct33.rds",  update = FALSE)
    
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table[ ,1]),
        "rds/signif_cpct34.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table[ ,1],
                          subtable_marks = "both"
        ),
        "rds/signif_cpct34.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table[ ,1],
                          compare_type = c("previous", "first", "subtable"),
                          subtable_marks = "both"
        ),
        "rds/signif_cpct34.rds",  update = FALSE)
    
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table[ ,1],
                          keep = "bases"),
        "rds/signif_cpct34.rds",  update = FALSE)
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table[ ,1],
                          keep = "none"),
        "rds/signif_cpct35.rds",  update = FALSE)
    
    if(sessionInfo()$R.version$arch!="i386"){
        expect_equal_to_reference(
            significance_cpct(cro_cpct(NA)),
            "rds/signif_cpct36.rds",  update = FALSE)
    }
    
    mtcars_table4 = mtcars %calc% cro_cpct(list(cyl, gear),
                                           list(total(), vs, am, vs %nest% am))
    
    
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table4, compare_type = c("first_column", "subtable")),
        "rds/signif_cpct37.rds",  update = FALSE)
    
    colnames(mtcars_table4) = gsub("ss", "s\ns", colnames(mtcars_table4), perl = TRUE)
    colnames(mtcars_table4) = gsub("aight", "ai\rght", colnames(mtcars_table4), perl = TRUE)
    expect_equal_to_reference(
        significance_cpct(mtcars_table4, compare_type = c("first_column", "subtable")),
        "rds/signif_cpct37b.rds",  update = FALSE)
    
    mtcars_table5 = cro_cpct(list(mtcars$cyl, mtcars$gear),
                             unvr(list(total(), mtcars$vs, mtcars$am)))
    
    
    expect_equal_to_reference(
        significance_cpct(mtcars_table5), "rds/signif_cpct38.rds",  update = FALSE)
    
    
}
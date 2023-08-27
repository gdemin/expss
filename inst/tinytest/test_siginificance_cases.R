if(isTRUE(getOption("covr"))){ 
    context("significance_cases")
    mtcars = NULL
    rm(mtcars)
    mtcars = datasets::mtcars
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
    
    mtcars_table = cro_cases(list(mtcars$cyl, mtcars$gear),
                             list(total(), mtcars$vs, mtcars$am))
    expss_digits(0)
    # table(mtcars$cyl, mtcars$vs) %>% chisq.test()
    expect_equal_to_reference(
        significance_cases(mtcars_table),
        "rds/significance_cases1.rds",  update = TRUE
    )
    
    # table(mtcars$gear) %>% chisq.test()
    expect_equal_to_reference(
        significance_cases(mtcars_table, sig_level = 0.1),
        "rds/significance_cases2.rds",  update = TRUE
    )
    
    expect_equal_to_reference(
        significance_cases(mtcars_table, sig_level = 0.1, min_base = 20),
        "rds/significance_cases3.rds",  update = TRUE
    )
    
    expect_equal_to_reference(
        significance_cases(mtcars_table, sig_level = 0.1, keep = "none"),
        "rds/significance_cases4.rds",  update = TRUE
    )
    
    expect_equal_to_reference(
        significance_cases(mtcars_table, sig_level = 0.1, 
                           keep = "cases"),
        "rds/significance_cases5.rds",  update = TRUE
    )
    
    expect_equal_to_reference(
        significance_cases(mtcars_table, sig_level = 0.1, 
                           keep = "bases"),
        "rds/significance_cases6.rds",  update = TRUE
    )
    
    expect_equal_to_reference(
        significance_cases(mtcars_table, sig_level = 0.1, digits = 3),
        "rds/significance_cases7.rds",  update = TRUE
    )
    
    
    expect_equal_to_reference(
        significance_cases(mtcars_table[,1], sig_level = 0.1, keep = "none"),
        "rds/significance_cases8.rds",  update = TRUE
    )
    
    expect_equal_to_reference(
        significance_cases(mtcars_table[,1], sig_level = 0.1, 
                           keep = "cases"),
        "rds/significance_cases9.rds",  update = TRUE
    )
    
    expect_equal_to_reference(
        significance_cases(mtcars_table[,1], sig_level = 0.1, 
                           keep = "bases"),
        "rds/significance_cases10.rds",  update = TRUE
    )
    
    mtcars_table = cro_cases(list(mtcars$cyl, mtcars$gear),
                             list(total(), mtcars$vs, mtcars$am), total_row_position = "none")
    
    # table(mtcars$cyl, mtcars$vs) %>% chisq.test()
    expect_error(
        significance_cases(mtcars_table)
    )
    
    mtcars_table = cro_cases(list(mtcars$cyl, mtcars$gear),
                             list(total(), mtcars$vs, mtcars$am), total_row_position = "above")
    
    expect_equal_to_reference(
        significance_cases(mtcars_table),
        "rds/significance_cases11.rds",  update = TRUE
    )
    
    mtcars_table = cro_cases(list(mtcars$cyl, mtcars$gear),
                             list(total(), mtcars$vs, mtcars$am), total_row_position = "above",
                             total_statistic = c("u_rpct", "u_cases"))
    
    expect_equal_to_reference(
        significance_cases(mtcars_table, total_row = 2),
        "rds/significance_cases12.rds",  update = TRUE
    )
    
    expect_equal_to_reference(
        significance_cases(mtcars_table, total_row = 2, keep = "none", sig_level = 0.1),
        "rds/significance_cases4.rds",  update = TRUE
    )
    
    expect_equal_to_reference(
        significance_cases(mtcars_table, total_row = 2, keep = "bases"),
        "rds/significance_cases13.rds",  update = TRUE
    )
    
    expect_equal_to_reference(
        significance_cases(mtcars_table, total_row = 2, keep = "cases", sig_level = 0.1),
        "rds/significance_cases14.rds",  update = TRUE
    )
    
    
    expect_equal_to_reference(
        significance_cases(mtcars_table, min_base = 20, total_row = 2, keep = "cases", 
                           sig_level = 0.1),
        "rds/significance_cases15.rds",  update = TRUE
    )
    
    data(mtcars)
    mtcars = apply_labels(mtcars,
                          mpg = "Miles/(US) gallon",
                          cyl = "Number of cylinders",
                          cyl = c("empty  value" = 99),
                          disp = "Displacement (cu.in.)",
                          hp = "Gross horsepower",
                          drat = "Rear axle ratio",
                          wt = "Weight (lb/1000)",
                          qsec = "1/4 mile time",
                          vs = "Engine",
                          vs = c("V-engine" = 0,
                                 "Straight engine" = 1,
                                 Other = 98,
                                 Unknown = 99),
                          am = "Transmission",
                          am = c("Automatic" = 0,
                                 "Manual"=1),
                          gear = "Number of forward gears",
                          carb = "Number of carburetors"
    )
    
    mtcars_table = cro_cases(list(mtcars$cyl, mtcars$gear),
                             list(total(), mtcars$vs, mtcars$am))
    
    expect_equal_to_reference(
        significance_cases(mtcars_table),
        "rds/significance_cases16.rds",  update = TRUE
    )
    
    
    expect_equal_to_reference(
        significance_cases(cro_cases(NA)),
        "rds/significance_cases17.rds",  update = TRUE
    )
    
tbl = cross_cases(mtcars, am, vs) %>% drop_empty_columns()

correct_true = structure(list(row_labels = structure(c("Transmission|Automatic", 
"Transmission|Manual", "Transmission|#Chi-squared p-value", "Transmission|#Total cases"
), class = "AsIs"), `Engine|V-engine` = structure(c("12.0", " 6.0", 
"", "18.0"), class = "AsIs"), `Engine|Straight engine` = structure(c("7.0", 
"7.0", "", "14.0"), class = "AsIs")), row.names = c("1", "2", 
 "3", "31"), class = c("etable", "data.frame"))

correct_false = structure(list(row_labels = structure(c("Transmission|Automatic", 
"Transmission|Manual", "Transmission|#Chi-squared p-value", "Transmission|#Total cases"
), class = "AsIs"), `Engine|V-engine` = structure(c("12.0", " 6.0", 
"<0.4", "18.0"), class = "AsIs"), `Engine|Straight engine` = structure(c("7.0", 
"7.0", "", "14.0"), class = "AsIs")), row.names = c("1", "2", 
"3", "31"), class = c("etable", "data.frame"))
    # expect_equal(significance_cases(tbl, sig_level = 0.4, correct = TRUE), correct_true)
    # expect_equal(significance_cases(tbl, sig_level = 0.4, correct = FALSE), correct_false)
    expss_digits(NULL)
    
}
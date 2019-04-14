test_that("prepend_names/prepend_values", { 
    skip_on_cran()
    context("prepend_names/prepend_values")
    
    data(mtcars)
    
    res = mtcars %>% 
        prepend_names() %>% 
        calculate(
            cro_cpct(list(cyl, gear), list(total(), vs, am))
        )
    
    expect_equal_to_reference(res, "rds/prepend1.rds",  update = FALSE)
    
    res = mtcars %>% 
        prepend_values() %>% 
        calculate(
            cro_cpct(list(cyl, gear), list(total(), vs, am))
        )
    
    expect_equal_to_reference(res, "rds/prepend2.rds",  update = FALSE)
    
    res = mtcars %>% 
        prepend_all() %>% 
        calculate(
            cro_cpct(list(cyl, gear), list(total(), vs, am))
        )
    
    expect_equal_to_reference(res, "rds/prepend1.rds",  update = FALSE)
    
    #########
    
    res = mtcars %>% 
        tab_prepend_names() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    
    
    expect_equal_to_reference(res, "rds/prepend1.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_prepend_values() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    
    expect_equal_to_reference(res, "rds/prepend1.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_prepend_all() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    expect_equal_to_reference(res, "rds/prepend1.rds",  update = FALSE)
    
    #########
    
    res = mtcars %>% 
        tab_prepend_names() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    
    
    expect_equal_to_reference(res, "rds/prepend1.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_prepend_values() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    
    expect_equal_to_reference(res, "rds/prepend1.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_prepend_all() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    expect_equal_to_reference(res, "rds/prepend1.rds",  update = FALSE)
    
    #########
    
    res = mtcars %>% 
        tab_mis_val(99) %>% 
        tab_prepend_names() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    
    
    expect_equal_to_reference(res, "rds/prepend1.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_mis_val(99) %>% 
        tab_prepend_values() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    
    expect_equal_to_reference(res, "rds/prepend1.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_mis_val(99) %>% 
        tab_prepend_all() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    expect_equal_to_reference(res, "rds/prepend1.rds",  update = FALSE)
    
    ##########################
    
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
    
    res = mtcars %>% 
        prepend_names() %>% 
        calculate(
            cro_cpct(list(cyl, gear), list(total(), vs, am))
        )
    
    expect_equal_to_reference(res, "rds/prepend3.rds",  update = FALSE)
    
    res = mtcars %>% 
        prepend_values() %>% 
        calculate(
            cro_cpct(list(cyl, gear), list(total(), vs, am))
        )
    
    expect_equal_to_reference(res, "rds/prepend4.rds",  update = FALSE)
    
    res = mtcars %>% 
        prepend_all() %>% 
        calculate(
            cro_cpct(list(cyl, gear), list(total(), vs, am))
        )
    
    expect_equal_to_reference(res, "rds/prepend5.rds",  update = FALSE)
    
    ################
    #########
    
    res = mtcars %>% 
        tab_prepend_names() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    
    
    expect_equal_to_reference(res, "rds/prepend3.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_prepend_values() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    
    expect_equal_to_reference(res, "rds/prepend4.rds",  update = FALSE)
    
    res = mtcars %>% 
        tab_prepend_all() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_cells(cyl, gear) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()
    expect_equal_to_reference(res, "rds/prepend5.rds",  update = FALSE)
    
    ############################
    
    expect_error(prepend_names(mtcars$am))
    
    res = mtcars %>% 
        calculate(
            cro_cpct(prepend_values(list(cyl, gear)), 
                     list(total(), prepend_all(vs), 
                          prepend_values(am)))
        )
    
    expect_equal_to_reference(res, "rds/prepend4.rds",  update = FALSE)
    
    #######################################
    
    res = mtcars %>% 
        tab_cells(cyl, gear) %>% 
        tab_prepend_all() %>% 
        tab_cols(total(), vs, am) %>% 
        tab_stat_cpct() %>% 
        tab_pivot()     
    
    expect_equal_to_reference(res, "rds/prepend6.rds",  update = FALSE)
    
})
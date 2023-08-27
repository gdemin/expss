context("split_by")

data(airquality)


data(mtcars)
# add labels to dataset
mtcars = apply_labels(mtcars, 
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      carb = NULL,
                      qsec = "1/4 mile time",
                      hp = "Gross horsepower",
                      vs = "Engine",
                      vs = num_lab(" 
                                   0 V-engine
                                   1 Straight engine
                                   "),
                      
                      am = "Transmission",
                      am = num_lab(" 
                                   0 Automatic
                                   1 Manual
                                   99 Not specified
                                   ")
                      )

mtcars_list = mtcars %>% split_by(am, vs, drop = FALSE)
expect_equal_to_reference(mtcars_list, "rds/split_by4.rds",  update = FALSE)

mtcars_list = mtcars %>% split_by(am, vs, drop = TRUE)
expect_equal_to_reference(mtcars_list, "rds/split_by3.rds",  update = FALSE)


dt_mtcars = as.data.table(mtcars)


dt_list = dt_mtcars %>% split_by(am, vs, drop = TRUE)
expect_equal_to_reference(dt_list, "rds/split_by10.rds",  update = FALSE)

dt_unsplit = split_off(dt_list)
expect_equal_to_reference(dt_unsplit, "rds/split_by11.rds",  update = FALSE)

dt_unsplit = split_off(dt_list, groups = "my_groups", rownames = "my_rows")
expect_equal_to_reference(dt_unsplit, "rds/split_by12.rds",  update = FALSE)

tbl_list = mtcars_list %>% calc(
    cro(cyl, gear)
)

expect_equal_to_reference(tbl_list, "rds/split_by13.rds",  update = FALSE)

tbls = mtcars_list %>% calc(
    cro(cyl, gear)
) %>% 
    split_off()

expect_equal_to_reference(tbls, "rds/split_by14.rds",  update = FALSE)


tbls = mtcars_list %>% calc(
    cro(cyl, gear)
) %>% 
    split_off(groups = TRUE, rownames = TRUE)

expect_equal_to_reference(tbls, "rds/split_by15.rds",  update = FALSE)

tbls = mtcars_list %>% calc(
    cro(cyl, gear)
) %>% 
    split_off(groups = "My group", rownames = TRUE)


expect_equal_to_reference(tbls, "rds/split_by16.rds",  update = FALSE)

tbls = mtcars_list %>% calc(
    cro(cyl, gear) %>% set_caption(names(val_lab(am)[1]))
) %>% 
    split_off()

expect_equal_to_reference(tbls, "rds/split_by17.rds",  update = FALSE)


tbls = mtcars_list %>% calc(
    cro(cyl, gear) %>% set_caption(names(val_lab(am)[1]))
) %>% 
    split_off(groups = TRUE)

expect_equal_to_reference(tbls, "rds/split_by18.rds",  update = FALSE)

tbls_list = tbls %>% split_by(row_labels)
expect_equal_to_reference(tbls_list, "rds/split_by19.rds",  update = FALSE)

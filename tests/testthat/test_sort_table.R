context("sort_table_*")

data(mtcars)

# apply labels
mtcars = apply_labels(mtcars,
                      cyl = "Number of cylinders",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c(automatic = 0,
                             manual=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)


tab = calculate(mtcars, table_cpct(list(cyl, gear, carb), list("#total", vs, am)))
expect_equal_to_reference(
    sort_table_asc(tab),
    "rds/sort_table_1"
)

expect_equal_to_reference(
    sort_table_desc(tab),
    "rds/sort_table_2"
)

tab = calculate(mtcars, table_cpct(list(cyl, gear, carb), list("#total", vs, am), 
                                   total_row_position = "above"))

expect_equal_to_reference(
    sort_table_asc(tab),
    "rds/sort_table_3"
)

expect_equal_to_reference(
    sort_table_desc(tab),
    "rds/sort_table_4"
)

expect_identical(
    sort_table_desc(tab, excluded_rows = NULL)[[2]],
    sort_desc(tab, 2)[[2]]
)

tab = calculate(mtcars, table_cpct(list(cyl, gear, carb), list("#total", vs, am), 
                                   total_row_position = "none"))

expect_equal_to_reference(
    sort_table_asc(tab),
    "rds/sort_table_5"
)

expect_equal_to_reference(
    sort_table_desc(tab),
    "rds/sort_table_6"
)

expect_equal_to_reference(
    sort_table_asc(tab[FALSE,]),
    "rds/sort_table_7"
)

expect_equal_to_reference(
    sort_table_desc(tab[FALSE,]),
    "rds/sort_table_7"
)

tab = calculate(mtcars, table_cpct(list(cyl, gear, carb), list("#total", vs, am)))
expect_equal_to_reference(
    sort_table_asc(tab, excluded_rows = grepl("#|8", tab[[1]], perl = TRUE)),
    "rds/sort_table_8"
)

expect_equal_to_reference(
    sort_table_desc(tab, excluded_rows = grepl("#|1", tab[[1]], perl = TRUE)),
    "rds/sort_table_9"
)


expect_equal_to_reference(
    sort_table_asc(tab, excluded_rows = grep("#|8", tab[[1]], perl = TRUE)),
    "rds/sort_table_8"
)

expect_equal_to_reference(
    sort_table_desc(tab, excluded_rows = grep("#|1", tab[[1]], perl = TRUE)),
    "rds/sort_table_9"
)


expect_equal_to_reference(
    sort_table_asc(tab, excluded_rows = "#|8"),
    "rds/sort_table_8"
)

expect_equal_to_reference(
    sort_table_desc(tab, excluded_rows = "#|1"),
    "rds/sort_table_9"
)

expect_equal_to_reference(
    sort_table_asc(tab, excluded_rows = c("#","8")),
    "rds/sort_table_8"
)

expect_equal_to_reference(
    sort_table_desc(tab, excluded_rows = c("#","1")),
    "rds/sort_table_9"
)

expect_equal_to_reference(
    sort_table_asc(tab, columns = "Engine|V-engine"),
    "rds/sort_table_10"
)

expect_equal_to_reference(
    sort_table_desc(tab, columns = "Engine|V-engine"),
    "rds/sort_table_11"
)

expect_identical(
    sort_table_asc(tab, excluded_rows = rep(TRUE, nrow(tab))),
    tab
    
)

expect_error(
    sort_table_asc(tab, excluded_rows = TRUE)
)

expect_error(
    sort_table_desc(tab, columns = "Enginesas|V-engine")
)

expect_error(
    sort_table_asc(tab, excluded_rows = c(1, NA))
)

expect_error(
    sort_table_asc(tab, excluded_rows = c(NA, rep(TRUE, nrow(tab)-1)))
)


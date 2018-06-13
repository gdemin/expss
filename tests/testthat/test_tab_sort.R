context("tab_sort_*")

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


tab = calculate(mtcars, cro_cpct(list(cyl, gear, carb), list("#total", vs, am)))
expect_equal_to_reference(
    tab_sort_asc(tab),
    "rds/tab_sort_1.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_desc(tab),
    "rds/tab_sort_2.rds",  update = FALSE
)

tab = calculate(mtcars, cro_cpct(list(cyl, gear, carb), list("#total", vs, am), 
                                   total_row_position = "above"))

expect_equal_to_reference(
    tab_sort_asc(tab),
    "rds/tab_sort_3.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_desc(tab),
    "rds/tab_sort_4.rds",  update = FALSE
)

expect_identical(
    tab_sort_desc(tab, excluded_rows = NULL)[[2]],
    sort_desc(tab, 2)[[2]]
)

tab = calculate(mtcars, cro_cpct(list(cyl, gear, carb), list("#total", vs, am), 
                                 total_row_position = "none"))

expect_equal_to_reference(
    tab_sort_asc(tab),
    "rds/tab_sort_5.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_desc(tab),
    "rds/tab_sort_6.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_asc(tab[FALSE,]),
    "rds/tab_sort_7.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_desc(tab[FALSE,]),
    "rds/tab_sort_7.rds",  update = FALSE
)

tab = calculate(mtcars, cro_cpct(list(cyl, gear, carb), list("#total", vs, am)))
expect_equal_to_reference(
    tab_sort_asc(tab, excluded_rows = grepl("#|8", tab[[1]], perl = TRUE)),
    "rds/tab_sort_8.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_desc(tab, excluded_rows = grepl("#|1", tab[[1]], perl = TRUE)),
    "rds/tab_sort_9.rds",  update = FALSE
)


expect_equal_to_reference(
    tab_sort_asc(tab, excluded_rows = grep("#|8", tab[[1]], perl = TRUE)),
    "rds/tab_sort_8.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_desc(tab, excluded_rows = grep("#|1", tab[[1]], perl = TRUE)),
    "rds/tab_sort_9.rds",  update = FALSE
)


expect_equal_to_reference(
    tab_sort_asc(tab, excluded_rows = "#|8"),
    "rds/tab_sort_8.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_desc(tab, excluded_rows = "#|1"),
    "rds/tab_sort_9.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_asc(tab, excluded_rows = c("#","8")),
    "rds/tab_sort_8.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_desc(tab, excluded_rows = c("#","1")),
    "rds/tab_sort_9.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_asc(tab, "Engine|V-engine"),
    "rds/tab_sort_10.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_desc(tab, "Engine|V-engine"),
    "rds/tab_sort_11.rds",  update = FALSE
)

param = "Engine|V-engine"

expect_error(
    tab_sort_desc(tab, param)
)


expect_equal_to_reference(
    tab_sort_desc(tab, (param)),
    "rds/tab_sort_11.rds",  update = FALSE
)

expect_equal_to_reference(
    tab_sort_asc(tab, perl("V-engine")),
    "rds/tab_sort_10.rds",  update = FALSE
)

expect_identical(
    tab_sort_asc(tab, excluded_rows = rep(TRUE, nrow(tab))),
    tab
    
)

expect_error(
    tab_sort_asc(tab, excluded_rows = TRUE)
)

expect_error(
    tab_sort_desc(tab, columns = "Enginesas|V-engine")
)

expect_error(
    tab_sort_asc(tab, excluded_rows = c(1, NA))
)

expect_error(
    tab_sort_asc(tab, excluded_rows = c(NA, rep(TRUE, nrow(tab)-1)))
)


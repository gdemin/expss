context("drop_empty_*")
data("mtcars")
mtcars = apply_labels(mtcars,
            vs = "Engine",
            vs = num_lab("
                      0 V-engine 
                      1 Straight engine
                      9 Other
                      "),
            am = "Transmission",
            am = num_lab("
                     0 Automatic 
                     1 Manual
                     9 Other
                     ")
)
# with_empty = calculate(mtcars, table_cases(am, vs))
# 
# 
# with_empty = with_empty %merge% with_empty
# with_empty = with_empty %add_rows% with_empty

with_empty = structure(list(row_labels = c("Transmission|Automatic", "Transmission|Manual", 
"Transmission|Other", "Transmission|#Total cases", "Transmission|Automatic", 
"Transmission|Manual", "Transmission|Other", "Transmission|#Total cases"
), `Engine|V-engine` = c(12L, 6L, NA, 18L, 12L, 6L, NA, 18L), 
`Engine|Straight engine` = c(7L, 7L, NA, 14L, 7L, 7L, NA, 
14L), `Engine|Other` = c(NA_integer_, NA_integer_, NA_integer_, 
NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_
), `Engine|V-engine` = c(12L, 6L, NA, 18L, 12L, 6L, NA, 18L
), `Engine|Straight engine` = c(7L, 7L, NA, 14L, 7L, 7L, 
NA, 14L), `Engine|Other` = c(NA_integer_, NA_integer_, NA_integer_, 
NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_
)), .Names = c("row_labels", "Engine|V-engine", "Engine|Straight engine", 
"Engine|Other", "Engine|V-engine", "Engine|Straight engine", 
"Engine|Other"), row.names = c(NA, 8L), class = c( 
"etable", "data.frame"))

expect_equal_to_reference(drop_r(with_empty), "rds/drop_empty1.rds",  update = FALSE)
expect_equal_to_reference(drop_c(with_empty), "rds/drop_empty2.rds",  update = FALSE)
expect_equal_to_reference(drop_rc(with_empty), "rds/drop_empty3.rds",  update = FALSE)

expect_equal_to_reference(drop_r(with_empty, excluded_rows = NULL),
                          "rds/drop_empty1.rds",  update = FALSE)

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grep("Other", with_empty[[1]]), excluded_columns = NULL),
                          "rds/drop_empty4.rds",  update = FALSE)
expect_equal_to_reference(drop_r(with_empty, excluded_rows = grep("Manual", with_empty[[1]]), excluded_columns = NULL),
                          "rds/drop_empty5.rds",  update = FALSE)
expect_equal_to_reference(drop_r(with_empty, excluded_rows = grep("Other", with_empty[[1]]), excluded_columns = 1),
    "rds/drop_empty6.rds",  update = FALSE)
expect_equal_to_reference(drop_r(with_empty,
                                 excluded_rows = grep("Manual", with_empty[[1]]), 
                                 excluded_columns = 1),
    "rds/drop_empty7.rds",  update = FALSE)

expect_equal_to_reference(drop_r(with_empty, excluded_rows = "Other", excluded_columns = 1),
                          "rds/drop_empty6.rds",  update = FALSE)

expect_equal_to_reference(drop_r(with_empty, excluded_rows = "Other|Manual", excluded_columns = 1),
                          "rds/drop_empty6.rds",  update = FALSE)

expect_equal_to_reference(drop_r(with_empty, excluded_rows = c("Manual", "Other"), excluded_columns = 1),
                          "rds/drop_empty6.rds",  update = FALSE)


expect_equal_to_reference(drop_r(with_empty, excluded_rows = "Manual", excluded_columns = 1),
                          "rds/drop_empty7.rds",  update = FALSE)

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grep("Manual", with_empty[[1]]), 
                                 excluded_columns = colnames(with_empty)=="row_labels"),
                          "rds/drop_empty7.rds",  update = FALSE)

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grep("Manual", with_empty[[1]]), 
                                 excluded_columns = "row_labels"),
                          "rds/drop_empty7.rds",  update = FALSE)

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grepl("Manual", with_empty[[1]]), excluded_columns = 1),
                          "rds/drop_empty7.rds",  update = FALSE)

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grepl("Manual", with_empty[[1]]), 
                                 excluded_columns = colnames(with_empty)=="row_labels"),
                          "rds/drop_empty7.rds",  update = FALSE)

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grepl("Manual", with_empty[[1]]), 
                                 excluded_columns = "row_labels"),
                          "rds/drop_empty7.rds",  update = FALSE)


########
expect_equal_to_reference(drop_c(with_empty, excluded_rows = grep("Other", with_empty[[1]]), excluded_columns = NULL),
                          "rds/drop_empty2.rds",  update = FALSE)
expect_equal_to_reference(drop_c(with_empty, excluded_rows = grep("Manual", with_empty[[1]]), excluded_columns = NULL),
                          "rds/drop_empty2.rds",  update = FALSE)
expect_equal_to_reference(drop_c(with_empty, excluded_rows = grepl("Manual", with_empty[[1]]), excluded_columns = NULL),
                          "rds/drop_empty2.rds",  update = FALSE)

expect_equal_to_reference(drop_c(with_empty, excluded_rows = "Other", excluded_columns = NULL),
                          "rds/drop_empty2.rds",  update = FALSE)
expect_equal_to_reference(drop_c(with_empty, excluded_rows = "Manual", excluded_columns = NULL),
                          "rds/drop_empty2.rds",  update = FALSE)


expect_equal_to_reference(drop_c(with_empty, excluded_rows = !is.na(with_empty[[2]]), excluded_columns = NULL),
                          "rds/drop_empty2a.rds",  update = FALSE)


expect_equal_to_reference(drop_c(with_empty, excluded_rows = "Automatic|Manual|Total", excluded_columns = NULL),
                          "rds/drop_empty2a.rds",  update = FALSE)

expect_equal_to_reference(drop_c(with_empty, excluded_rows = c("Automatic", "Manual", "Total"), excluded_columns = NULL),
                          "rds/drop_empty2a.rds",  update = FALSE)

expect_equal_to_reference(drop_c(with_empty, excluded_rows = which(!is.na(with_empty[[2]])), excluded_columns = NULL),
                          "rds/drop_empty2a.rds",  update = FALSE)


expect_equal_to_reference(drop_c(with_empty, excluded_rows = NULL, excluded_columns = 4),
                          "rds/drop_empty10.rds",  update = FALSE)
expect_equal_to_reference(drop_c(with_empty, excluded_columns = grep("Other", colnames(with_empty))),
                          "rds/drop_empty11.rds",  update = FALSE)

expect_equal_to_reference(drop_c(with_empty, excluded_columns = grepl("Other", colnames(with_empty))),
                          "rds/drop_empty11.rds",  update = FALSE)

expect_equal_to_reference(drop_c(with_empty, excluded_rows = NULL, 
                                 excluded_columns = "Engine|Other"),
                          "rds/drop_empty11.rds",  update = FALSE)

expect_equal_to_reference(drop_c(with_empty, excluded_rows = NULL, 
                                 excluded_columns = c("Engine|Straight engine", "Engine|Other")),
                          "rds/drop_empty11.rds",  update = FALSE)

#######

expect_error(
    drop_r(with_empty, excluded_rows = c(8, 9:11, NA))
    )
# expect_error(
#     drop_r(with_empty, excluded_rows = c(rep(FALSE, 8), TRUE))
#     )
expect_error(
    drop_r(with_empty, excluded_columns = c("sddsfsd"))
)
expect_error(
    drop_r(with_empty, excluded_columns = c(6, 9:11, NA))
)
#####
expect_error(
    drop_c(with_empty, excluded_rows = c(8, 9:11, NA))
)
# expect_error(
#     drop_r(with_empty, excluded_rows = c(rep(FALSE, 8), TRUE))
#     )
expect_error(
    drop_c(with_empty, excluded_columns = c("sddsfsd"))
)
expect_error(
    drop_c(with_empty, excluded_columns = c(6, 9:11, NA))
)


mtcars_1 = cbind(empty = NA, mtcars)
mtcars_1 = drop_empty_columns(mtcars_1)
expect_identical(mtcars, mtcars_1)

mtcars_1 = cbind(empty = NA, mtcars)
mtcars_1 = rbind(NA, mtcars_1)
mtcars_1[1, 1] = "#"
mtcars_1 = drop_empty_rows(mtcars_1, excluded_columns = 1)
expect_identical(cbind(empty = as.character(NA), mtcars, stringsAsFactors = FALSE), mtcars_1)
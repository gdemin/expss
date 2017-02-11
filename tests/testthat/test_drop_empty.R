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
with_empty = calculate(mtcars, table_cases(am, vs))


with_empty = with_empty %merge% with_empty
with_empty = with_empty %add_rows% with_empty

expect_equal_to_reference(drop_r(with_empty), "rds/drop_empty1.rds")
expect_equal_to_reference(drop_c(with_empty), "rds/drop_empty2.rds")
expect_equal_to_reference(drop_rc(with_empty), "rds/drop_empty3.rds")

expect_equal_to_reference(drop_r(with_empty, excluded_rows = NULL),
                          "rds/drop_empty1.rds")

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grep("Other", with_empty[[1]]), excluded_columns = NULL),
                          "rds/drop_empty4.rds")
expect_equal_to_reference(drop_r(with_empty, excluded_rows = grep("Manual", with_empty[[1]]), excluded_columns = NULL),
                          "rds/drop_empty5.rds")
expect_equal_to_reference(drop_r(with_empty, excluded_rows = grep("Other", with_empty[[1]]), excluded_columns = 1),
    "rds/drop_empty6.rds")
expect_equal_to_reference(drop_r(with_empty, excluded_rows = grep("Manual", with_empty[[1]]), excluded_columns = 1),
    "rds/drop_empty7.rds")

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grep("Manual", with_empty[[1]]), 
                                 excluded_columns = colnames(with_empty)=="row_labels"),
                          "rds/drop_empty7.rds")

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grep("Manual", with_empty[[1]]), 
                                 excluded_columns = "row_labels"),
                          "rds/drop_empty7.rds")

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grepl("Manual", with_empty[[1]]), excluded_columns = 1),
                          "rds/drop_empty7.rds")

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grepl("Manual", with_empty[[1]]), 
                                 excluded_columns = colnames(with_empty)=="row_labels"),
                          "rds/drop_empty7.rds")

expect_equal_to_reference(drop_r(with_empty, excluded_rows = grepl("Manual", with_empty[[1]]), 
                                 excluded_columns = "row_labels"),
                          "rds/drop_empty7.rds")


########
expect_equal_to_reference(drop_c(with_empty, excluded_rows = grep("Other", with_empty[[1]]), excluded_columns = NULL),
                          "rds/drop_empty2.rds")
expect_equal_to_reference(drop_c(with_empty, excluded_rows = grep("Manual", with_empty[[1]]), excluded_columns = NULL),
                          "rds/drop_empty2.rds")
expect_equal_to_reference(drop_c(with_empty, excluded_rows = grepl("Manual", with_empty[[1]]), excluded_columns = NULL),
                          "rds/drop_empty2.rds")

expect_equal_to_reference(drop_c(with_empty, excluded_rows = !is.na(with_empty[[2]]), excluded_columns = NULL),
                          "rds/drop_empty2a.rds")

expect_equal_to_reference(drop_c(with_empty, excluded_rows = which(!is.na(with_empty[[2]])), excluded_columns = NULL),
                          "rds/drop_empty2a.rds")


expect_equal_to_reference(drop_c(with_empty, excluded_rows = NULL, excluded_columns = 4),
                          "rds/drop_empty10.rds")
expect_equal_to_reference(drop_c(with_empty, excluded_columns = grep("Other", colnames(with_empty))),
                          "rds/drop_empty11.rds")

expect_equal_to_reference(drop_c(with_empty, excluded_columns = grepl("Other", colnames(with_empty))),
                          "rds/drop_empty11.rds")

expect_equal_to_reference(drop_c(with_empty, excluded_rows = NULL, 
                                 excluded_columns = "Engine|Other"),
                          "rds/drop_empty11.rds")

expect_equal_to_reference(drop_c(with_empty, excluded_rows = NULL, 
                                 excluded_columns = c("Engine|Straight engine", "Engine|Other")),
                          "rds/drop_empty11.rds")

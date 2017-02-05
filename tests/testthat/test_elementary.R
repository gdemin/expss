context("elementary_cases")
# suppressMessages(library(data.table))
elementary_cases = expss:::elementary_cases
data(mtcars)
# expect_error(elementary_cases(mtcars, var_names = "am", ban_names = "vs", weight_name = NULL))
mt = data.table::data.table(mtcars)
expect_equal_to_reference(elementary_cases(mt, var_names = "am", ban_names = "vs", weight_name = NULL),
                          "rds/elementary_cases1.rds")

expect_equal_to_reference(elementary_cases(mt, var_names = "am", ban_names = "vs", weight_name = "wt"),
                          "rds/elementary_cases2.rds")


mt$empty = as.numeric(NA)
mt$empty2 = as.numeric(NA)
expect_equal_to_reference(elementary_cases(mt, var_names = "empty", ban_names = "vs", weight_name = NULL),
                          "rds/elementary_cases3.rds")
expect_equal_to_reference(elementary_cases(mt, var_names = "am", ban_names = "empty", weight_name = NULL),
"rds/elementary_cases3.rds")

expect_equal_to_reference(elementary_cases(mt, var_names = "empty2", ban_names = "empty", weight_name = NULL),
"rds/elementary_cases3.rds")
expect_equal_to_reference(elementary_cases(mt, var_names = "am", ban_names = "am", weight_name = NULL),
"rds/elementary_cases4.rds")

expect_equal_to_reference(elementary_cases(mt, var_names = "empty", ban_names = "vs", weight_name = "wt"),
"rds/elementary_cases3.rds")
expect_equal_to_reference(elementary_cases(mt, var_names = "am", ban_names = "empty", weight_name = "wt"),
"rds/elementary_cases3.rds")
expect_equal_to_reference(elementary_cases(mt, var_names = "empty2", ban_names = "empty", weight_name = "wt"),
"rds/elementary_cases3.rds")
expect_equal_to_reference(elementary_cases(mt, var_names = "am", ban_names = "am", weight_name = "wt"),
"rds/elementary_cases5.rds")

mt$weight = ifelse(mt$am==1, NA, 2)
expect_equal_to_reference(elementary_cases(mt, var_names = "am", ban_names = "vs", weight_name = "weight"),
"rds/elementary_cases6.rds")
expect_equal_to_reference(elementary_cases(mt, var_names = "am", ban_names = "vs", weight_name = "empty"),
"rds/elementary_cases3a.rds")


context("elementary_cases multiple response")

test = data.table::data.table(v1 = c(1,1,NA), v2 = c(NA, 2, 2), b1 = c(1, 1, 2), b2 = c(3, NA, 1))
test$weight = c(1, 2, 3)
test$empty1 = as.numeric(NA)
test$empty2 = as.numeric(NA)

expect_equal_to_reference(elementary_cases(test, var_names = c("v1", "v2"), ban_names = "b1", weight_name = NULL),
                          "rds/elementary_cases7.rds")

expect_equal_to_reference(elementary_cases(test, var_names = "b1", ban_names = c("v1", "v2"), weight_name = NULL),
                          "rds/elementary_cases8.rds")

expect_equal_to_reference(elementary_cases(test, var_names = c("v1", "v2"), ban_names = "b1", weight_name = "weight"),
                          "rds/elementary_cases9.rds")

expect_equal_to_reference(elementary_cases(test, var_names = "b1", ban_names = c("v1", "v2"), weight_name = "weight"),
                          "rds/elementary_cases10.rds")

#############
expect_equal_to_reference(elementary_cases(test, var_names = c("v1", "v2"), ban_names = c("b1", "b2"), weight_name = NULL),
                          "rds/elementary_cases11.rds")


expect_equal_to_reference(elementary_cases(test, var_names = c("v1", "v2"), ban_names = c("b1", "b2"), weight_name = "weight"),
                          "rds/elementary_cases12.rds")

expect_equal_to_reference(elementary_cases(test, var_names = c("empty1"), ban_names = c("b1", "b2"), weight_name = NULL),
                          "rds/elementary_cases3.rds")


expect_equal_to_reference(elementary_cases(test, var_names = c("v1", "v2"), ban_names = c("empty1"), weight_name = "weight"),
                          "rds/elementary_cases3.rds")

expect_equal_to_reference(elementary_cases(test, var_names = c("v1", "v2"), ban_names = c("b1", "b2"), weight_name = "empty1"),
                          "rds/elementary_cases3b.rds")

expect_equal_to_reference(elementary_cases(test, var_names = c("v1", "v2", "empty1"), ban_names = c("b1", "b2", "empty1"), weight_name = "weight"),
                          "rds/elementary_cases12.rds")




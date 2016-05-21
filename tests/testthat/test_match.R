context("match")

a = as.data.frame(matrix(1:9, ncol = 3))

expect_equal(match_row(1, a), c(1, NA, NA))
expect_equal(match_col(1, a), c(V1 = 1, V2 = NA, V3 = NA))


expect_equal(match_row(c(1, 5, 9), a), c(1, 2, 3))
expect_equal(match_col(c(1, 5, 9), a), c(V1 = 1, V2 = 2, V3 = 3))

expect_equal(match_row(gt(2), a), c(2, 2, 1))
expect_equal(match_col(gt(2), a), c(V1 = 3, V2 = 1, V3 = 1))

V1 = 1:3
V2 = 4:6
V3 = 7:9

expect_equal(match_row(1, V1, V2, V3), c(1, NA, NA))
expect_equal(match_col(1, V1, V2, V3), c(V1 = 1, V2 = NA, V3 = NA))


expect_equal(match_row(c(1, 5, 9), V1, V2, V3), c(1, 2, 3))
expect_equal(match_col(c(1, 5, 9), V1, V2, V3), c(V1 = 1, V2 = 2, V3 = 3))

expect_error(match_row(c(1, 5), V1, V2, V3))
expect_error(match_col(c(1, 5), V1, V2, V3))

expect_error(match_row(c(1, 5, 9, 9), V1, V2, V3))
expect_error(match_col(c(1, 5, 9, 9), V1, V2, V3))

expect_equal(match_row(gt(2), V1, V2, V3), c(2, 2, 1))
expect_equal(match_col(gt(2), V1, V2, V3), c(V1 = 3, V2 = 1, V3 = 1))

expect_error(match_row(NULL, V1, V2, V3))
expect_error(match_col(NULL, V1, V2, V3))

expect_error(match_row(numeric(0), V1, V2, V3))
expect_error(match_col(numeric(0), V1, V2, V3))


context("index")

a = as.data.frame(matrix(1:9, ncol = 3))

expect_equal(index_row(1, a), c(1, 2, 3))
expect_equal(index_col(1, a), c(V1 = 1, V2 = 4, V3 = 7))

expect_equal(index_row(NA, a), c(NA, NA, NA))
expect_equal(index_col(NA, a), c(V1 = NA, V2 = NA, V3 = NA))


expect_equal(index_row(c(1, NA, 2), a), c(1, NA, 6))
expect_equal(index_col(c(1, 3, NA), a), c(V1 = 1, V2 = 6, V3 = NA))

V1 = 1:3
V2 = 4:6
V3 = 7:9

expect_equal(index_row(1,  V1, V2, V3), c(1, 2, 3))
expect_equal(index_col(1,  V1, V2, V3), c(V1 = 1, V2 = 4, V3 = 7))

expect_equal(index_row(NA, V1, V2, V3), c(NA, NA, NA))
expect_equal(index_col(NA, V1, V2, V3), c(V1 = NA, V2 = NA, V3 = NA))


expect_equal(index_row(c(1, NA, 2),  V1, V2, V3), c(1, NA, 6))
expect_equal(index_col(c(1, 3, NA),  V1, V2, V3), c(V1 = 1, V2 = 6, V3 = NA))

expect_error(index_row(1:2,  V1, V2, V3))
expect_error(index_col(1:2,  V1, V2, V3))

expect_error(index_row(1:5,  V1, V2, V3))
expect_error(index_col(1:5,  V1, V2, V3))

expect_error(index_row(NULL, V1, V2, V3))
expect_error(index_col(NULL, V1, V2, V3))

expect_error(index_row(numeric(0), V1, V2, V3))
expect_error(index_col(numeric(0), V1, V2, V3))

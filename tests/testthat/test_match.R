context("match")

a = as.data.frame(matrix(1:9, ncol = 3))

expect_equal(match_row(1, a), c(1, NA, NA))
expect_equal(match_col(1, a), c(V1 = 1, V2 = NA, V3 = NA))


expect_equal(match_row(c(1, 5, 9), a), c(1, 2, 3))
expect_equal(match_col(c(1, 5, 9), a), c(V1 = 1, V2 = 2, V3 = 3))

expect_equal(match_row(gt(2), a), c(2, 2, 1))
expect_equal(match_col(gt(2), a), c(V1 = 3, V2 = 1, V3 = 1))

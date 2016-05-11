context("common case")

a = 1:3
b = 3:1
d = NA

expect_equal(sum_row(a, b, d), c(4,4,4))
expect_equal(unname(sum_col(a, b, d)), c(6,6,0))


expect_equal(mean_row(a, b, d), c(2,2,2))
expect_equal(unname(mean_col(a, b, d)), c(2,2,NA))

expect_equal(median_row(a, b, d), c(2,2,2))
expect_equal(unname(median_col(a, b, d)), c(2,2,NA))

expect_equal(max_row(a, b, d), c(3,2,3))
expect_equal(unname(max_col(a, b, d)), c(3,3,NA))

expect_equal(min_row(a, b, d), c(1,2,1))
expect_equal(unname(min_col(a, b, d)), c(1,1,NA))

temp = cbind(a, b, NA)
expect_equal(sd_row(a, b, d), apply(temp, 1, sd, na.rm = TRUE))
expect_equal(unname(sd_col(a, b, d)), unname(apply(temp, 2, sd)))

expect_equal(apply_row(sum, a, b, d), 1*c(NA,NA,NA))
expect_equal(unname(apply_col(sum, a, b, d)), c(6,6,NA))

context("edge cases")

a = 1:3
b = 3:1
d = numeric(0)

expect_equal(sum_row(a, b, d), c(4,4,4))
expect_equal(unname(sum_col(a, b, d)), c(6,6,0))


expect_equal(mean_row(a, b, d), c(2,2,2))
expect_equal(unname(mean_col(a, b, d)), c(2,2,NA))

expect_equal(median_row(a, b, d), c(2,2,2))
expect_equal(unname(median_col(a, b, d)), c(2,2,NA))

expect_equal(max_row(a, b, d), c(3,2,3))
expect_equal(unname(max_col(a, b, d)), c(3,3,NA))

expect_equal(min_row(a, b, d), c(1,2,1))
expect_equal(unname(min_col(a, b, d)), c(1,1,NA))

temp = cbind(a, b, NA)
expect_equal(sd_row(a, b, d), apply(temp, 1, sd, na.rm = TRUE))
expect_equal(unname(sd_col(a, b, d)), unname(apply(temp, 2, sd, na.rm = TRUE)))

expect_equal(apply_row(sum, a, b, d), 1*c(NA,NA,NA))
expect_equal(unname(apply_col(sum, a, b, d)), c(6,6,NA))
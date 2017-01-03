context("criteria")

expect_equal(gt(5)(5), FALSE)
expect_equal((5 | gt(5))(5), TRUE)


expect_equal((6 & gt(5))(5), FALSE)
expect_equal((6 & gt(5))(6), TRUE)

expect_equal((gt(5) & 6)(5), FALSE)
expect_equal((gt(5) & 6)(6), TRUE)

a = 1:4
expect_equal((eq(4) & (a<2))(a), rep(FALSE, 4))
expect_equal((eq(4) | (a<2))(a), c(TRUE, FALSE, FALSE, TRUE))

expect_equal(((a<2) & eq(4))(a), rep(FALSE, 4))
expect_equal(((a<2) | eq(4))(a), c(TRUE, FALSE, FALSE, TRUE))

num_crit = as.criterion(4)

expect_identical(num_crit(1:4), c(FALSE, FALSE, FALSE, TRUE))

logi_crit = as.criterion(c(TRUE, FALSE, FALSE, TRUE))
expect_identical(logi_crit(1:4), c(TRUE, FALSE, FALSE, TRUE))


fun_crit = as.criterion(function(x) x>2)

expect_identical((num_crit | logi_crit | fun_crit) (1:4), c(TRUE, FALSE, TRUE, TRUE))
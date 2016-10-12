context("criteria")

expect_equal(gt(5)(5), FALSE)
expect_equal((5 | gt(5))(5), TRUE)


expect_equal((6 & gt(5))(5), FALSE)
expect_equal((6 & gt(5))(6), TRUE)

expect_equal((gt(5) & 6)(5), FALSE)
expect_equal((gt(5) & 6)(6), TRUE)
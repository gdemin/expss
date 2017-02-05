context("insert_value_before")
aaa = 1:5
expss:::insert_value_before(aaa, 1) = 10
expect_identical(aaa, c(10, 1:5))

aaa = 1:5
expss:::insert_value_before(aaa, 3) = 10
expect_identical(aaa, c(1, 2, 10, 3:5))

aaa = 1:5
expss:::insert_value_before(aaa, 5) = 10
expect_identical(aaa, c(1:4, 10, 5))

aaa = 1:5
expss:::insert_value_before(aaa, 11) = 10
expect_identical(aaa, 1:5)

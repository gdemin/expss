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


context("insert_value_after")
aaa = 1:5
expss:::insert_value_after(aaa, 1) = 10
expect_identical(aaa, c(1, 10, 2:5))

aaa = 1:5
expss:::insert_value_after(aaa, 3) = 10
expect_identical(aaa, c(1:3, 10, 4:5))

aaa = 1:5
expss:::insert_value_after(aaa, 5) = 10
expect_identical(aaa, c(1:5, 10))

aaa = 1:5
expss:::insert_value_after(aaa, 11) = 10
expect_identical(aaa, 1:5)

context("uniq_elements")

uniq_elements = expss:::uniq_elements

a = matrix(1:9, 3)

expect_identical(uniq_elements(a), 1:9)

a = list(1:3, a, sheet(5:6))
expect_identical(uniq_elements(a), 1:9)

## just for coverage

expss:::.onAttach 
expss:::.onAttach 
expss:::.onAttach 
expss:::.onAttach 
expss:::.onAttach 
expss:::.onAttach 
expss:::.onAttach 
expss:::.onAttach 
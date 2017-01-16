context("do_repeat")

data(iris)

test_iris = iris

test_iris$i1 = 1L
test_iris$i2 = 2L
test_iris$i3 = 3L

res_iris = iris

res_iris = do_repeat(res_iris, i = qc(i1, i2, i3), expr = {
    i = .item_num
})

expect_identical(res_iris, test_iris)

test_iris$i1 = 10
test_iris$i2 = 20
test_iris$i3 = 30

res_iris = iris

res_iris = do_repeat(res_iris, i = qc(i1, i2, i3), value = c(10, 20, 30), 
    expr = {
    i = value
})


expect_identical(res_iris, test_iris)

test_iris = iris
test_iris$log = log(iris$Sepal.Length)
test_iris$exp = exp(iris$Sepal.Length)

res_iris = iris

res_iris = do_repeat(res_iris, i = qc(log, exp), fun = qc(log, exp), 
                     expr = {
                         i = fun(Sepal.Length)
                     })

expect_identical(res_iris, test_iris)

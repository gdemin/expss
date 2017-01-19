context("do_repeat")

data(iris)

test_iris = iris
test_iris$i1 = 10
test_iris$i2 = 20
test_iris$i3 = 30

res_iris = iris

res_iris = do_repeat(res_iris, i = qc(i1, i2, i3), value = c(10, 20, 30), 
    {
    i = value
})


expect_identical(res_iris, test_iris)
expect_error(do_repeat(res_iris, i = qc(i1, i2, i3), c(10, 20, 30), 
                       {
                           i = value
                       }))

expect_error(do_repeat(res_iris, i = qc(i1, i2, i3), value = c(10, 20), 
                       {
                           i = value
                       }))

test_iris = iris
test_iris$log = log(iris$Sepal.Length)
test_iris$exp = exp(iris$Sepal.Length)

res_iris = iris

res_iris = do_repeat(res_iris, i = qc(log, exp), fun = qc(log, exp), 
                      {
                         i = fun(Sepal.Length)
                     })

expect_identical(res_iris, test_iris)

data(iris)
test_iris = iris

test_iris$a = 16
res_iris = do_repeat(iris, i= 4, {
    a = sum(c(i, i, i, i))
    
    
})

expect_identical(res_iris, test_iris)


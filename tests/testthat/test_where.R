context("where")

data(iris)

expect_identical(where(iris, Species == "setosa"), iris[iris$Species == "setosa", ])
expect_identical(where(iris, 1:5), iris[1:5, ])

expect_identical(iris %where% (Species == "setosa"), iris[iris$Species == "setosa", ])
expect_identical(iris %where% 1:5, iris[1:5, ])

d_iris = iris

default_dataset(d_iris)

.where(Species == "setosa")
expect_identical(d_iris, iris[iris$Species == "setosa", ])

d_iris = iris
.where(1:5)
expect_identical(d_iris, iris[1:5, ])

set.seed(123)

rows = sample(150, 10)

set.seed(123)
expect_identical(where(iris, sample(.n, 10)), iris[rows, ])
set.seed(123)
expect_identical(where(iris, -sample(.n, 10)), iris[-rows, ])

set.seed(123)
expect_identical(iris %where% sample(.n, 10), iris[rows, ])
set.seed(123)
expect_identical(iris %where% -sample(.n, 10), iris[-rows, ])

expect_error(.where("a"))
expect_error(where(iris, "a"))
expect_error(iris %where% "a")
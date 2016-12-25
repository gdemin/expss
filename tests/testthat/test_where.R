context("where")

data(iris)

expect_identical(where(iris, Species == "setosa"), iris[iris$Species == "setosa", ])
expect_identical(where(iris, 1:5), iris[1:5, ])

expect_identical(iris %where% (Species == "setosa"), iris[iris$Species == "setosa", ])
expect_identical(iris %where% (iris$Species == "setosa"), iris[iris$Species == "setosa", ])



a = 150
# expect_identical(iris %where% (a < 51), iris[iris$Species == "setosa", ])
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
expect_identical(where(iris, sample(.N, 10)), iris[rows, ])
set.seed(123)
expect_identical(where(iris, -sample(.N, 10)), iris[-rows, ])

set.seed(123)
expect_identical(iris %where% sample(.n, 10), iris[rows, ])
set.seed(123)
expect_identical(iris %where% -sample(.n, 10), iris[-rows, ])

set.seed(123)
expect_identical(iris %where% sample(.N, 10), iris[rows, ])
set.seed(123)
expect_identical(iris %where% -sample(.N, 10), iris[-rows, ])

expect_error(.where("a"))
expect_error(where(iris, "a"))
expect_error(iris %where% "a")

#### vectors
set.seed(123)
rand_vec = sample(10, 20, replace = TRUE)

set.seed(123)
rows = sample(20, 10)

set.seed(123)
expect_identical(where(rand_vec, sample(.N, .N/2)), rand_vec[rows])
set.seed(123)
expect_identical(where(rand_vec, sample(.n, .n/2)), rand_vec[rows])

# expect_identical(where(rand_vec, rand_vec>5), rand_vec[rand_vec>5])

set.seed(123)
rand_matr = matrix(sample(10, 60, replace = TRUE), ncol = 3)

set.seed(123)
rows = sample(20, 10)

set.seed(123)
expect_identical(where(rand_matr, sample(.N, .N/2)), rand_matr[rows,])
set.seed(123)
expect_identical(where(rand_matr, sample(.n, .n/2)), rand_matr[rows,])

colnames(rand_matr) = c("v1", "v2", "v3")
expect_error(where(rand_matr, v1<5))
# expect_identical(where(rand_matr, rand_matr[,"v1"]<5), rand_matr[rand_matr[,"v1"]<5, ])

expect_identical(
    where(list(iris, rand_vec, rand_matr), 1:2),
    list(iris[1:2,], rand_vec[1:2], rand_matr[1:2,])
)

expect_identical(
    where(list(iris, rand_vec, rand_matr), 1:2),
    list(iris[1:2,], rand_vec[1:2], rand_matr[1:2,])
)


cond = iris$Petal.Length>1.5
expect_identical(
    where(list(iris, iris, iris), Petal.Length>1.5),
    list(iris[cond,], iris[cond,], iris[cond,])
)

set.seed(123)
cond1 = sample(nrow(iris), 3)
cond2 = sample(nrow(iris), 3)
cond3 = sample(nrow(iris), 3)
set.seed(123)
expect_identical(
    where(list(iris, iris, iris), sample(.N, 3)),
    list(iris[cond1,], iris[cond2,], iris[cond3,])
)

set.seed(123)
expect_identical(
    where(list(iris, iris, iris), sample(.n, 3)),
    list(iris[cond1,], iris[cond2,], iris[cond3,])
)

expect_identical(where(1:3, c(TRUE, FALSE, NA)), 1L)

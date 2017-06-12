context("where")

data(iris)

expect_identical(where(iris, Species == "setosa"), iris[iris$Species == "setosa", ])

item = "setosa"
expect_identical(where(iris, Species == item), iris[iris$Species == item, ])
test_scoping = function(item){
    filt = item
    where(iris, Species == filt)
}
expect_identical(test_scoping(item), iris[iris$Species == item, ])

list_iris = list(iris) 
expect_identical(where(list_iris, Species == "setosa"), list(iris[iris$Species == "setosa", ]))
expect_identical(where(list_iris, Species == item), list(iris[iris$Species == item, ]))

test_scoping = function(item){
    filt = item
    where(list_iris, Species == filt)
}
expect_identical(test_scoping(item), list(iris[iris$Species == item, ]))

global_item = "versicolor"
test_scoping = function(){
    local_item = "setosa"
    where(list_iris, (Species == global_item) | (Species == local_item))
}
expect_identical(test_scoping(), list(iris[iris$Species == global_item | iris$Species == "setosa", ]))

test_scoping = function(){
    local_item = iris$Species == "setosa"
    where(list_iris, local_item)
}
expect_identical(test_scoping(), list(iris[iris$Species == "setosa", ]))


expect_identical(where(iris, 1:5), iris[1:5, ])


list_iris = list(iris$Species) 
test_scoping = function(){
    local_item = iris$Species == "setosa"
    where(list_iris, local_item)
}
expect_identical(test_scoping(), list(iris[iris$Species == "setosa", "Species"]))


a = 150

d_iris = iris

default_dataset(d_iris)

.where(Species == "setosa")
expect_identical(d_iris, iris[iris$Species == "setosa", ])

d_iris = iris

## cond - special name which exists inside `where`
cond = "setosa"
.where(Species == cond)
expect_identical(d_iris, iris[iris$Species == "setosa", ])

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




expect_error(.where("a"))
expect_error(where(iris, "a"))

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

data(iris)

iris_list = list(iris[,-1], iris[,-5])

curr_name = 1

expect_identical(where(iris_list,  curr_name),
                 list(iris_list[[1]][1, , drop = FALSE], iris_list[[2]][1, , drop = FALSE])
)


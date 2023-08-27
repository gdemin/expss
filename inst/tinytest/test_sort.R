context("sort vector")
suppressWarnings(RNGversion("3.5.0"))

set.seed(123)

a = sample(c(1:10, NA, NA, NA))

expect_identical(sort_asc(a), sort(a, decreasing = FALSE, na.last = FALSE))
expect_identical(sort_desc(a), sort(a, decreasing = TRUE, na.last = TRUE))

expect_identical(sort_asc(a, na.last = TRUE), sort(a, decreasing = FALSE, na.last = TRUE))
expect_identical(sort_desc(a, na.last = FALSE), sort(a, decreasing = TRUE, na.last = FALSE))

expect_identical(sort_asc(a, na.last = NA), sort(a, decreasing = FALSE, na.last = NA))
expect_identical(sort_desc(a, na.last = NA), sort(a, decreasing = TRUE, na.last = NA))


context("sort data.frame")
data(iris)

expect_error(sort_asc(iris))

expect_identical(sort_asc(iris, Sepal.Length), iris[order(iris$Sepal.Length, decreasing = FALSE), ])
expect_identical(sort_asc(iris, Sepal.Length, 2), iris[order(iris$Sepal.Length, iris$Sepal.Width, decreasing = FALSE), ])
expect_identical(sort_asc(iris, Sepal.Length, Sepal.Width, 3:4), 
                 iris[order(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, decreasing = FALSE), ])

var_name = "Sepal.Length"
expect_identical(sort_asc(iris, ..$var_name), iris[order(iris$Sepal.Length, decreasing = FALSE), ])
expect_identical(sort_asc(iris, ..[(var_name)]), iris[order(iris$Sepal.Length, decreasing = FALSE), ])


param = qc(Sepal.Length, Sepal.Width)
expect_error(sort_asc(iris, param, 3:4))
expect_identical(sort_asc(iris, (param), 3:4), 
                 iris[order(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, decreasing = FALSE), ])



expect_identical(sort_asc(iris, 3:4, Sepal.Length, Sepal.Width), 
                 iris[order(iris$Petal.Length, iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width,  decreasing = FALSE), ])


expect_identical(sort_desc(iris, Sepal.Length), iris[order(iris$Sepal.Length, decreasing = TRUE), ])
expect_identical(sort_desc(iris, Sepal.Length, 2), iris[order(iris$Sepal.Length, iris$Sepal.Width, decreasing = TRUE), ])
expect_identical(sort_desc(iris, qc(Sepal.Length, Sepal.Width), 3:4), 
                 iris[order(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, decreasing = TRUE), ])

expect_identical(sort_desc(iris, 3:4, Sepal.Length, Sepal.Width), 
                 iris[order(iris$Petal.Length, iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width,  decreasing = TRUE), ])

single_col = iris[,"Sepal.Length", drop = FALSE]
expect_identical(sort_asc(single_col, Sepal.Length), 
                 single_col[order(single_col$Sepal.Length, decreasing = FALSE), , drop = FALSE])
expect_identical(sort_desc(single_col, Sepal.Length), 
                 single_col[order(single_col$Sepal.Length, decreasing = TRUE), , drop = FALSE])
############################


data(mtcars)
expect_identical(sort_asc(mtcars, 2:1), sort_asc(mtcars, "cyl", "mpg"))

iris[1:5,1] = NA

expect_identical(sort_asc(iris, Sepal.Length), iris[order(iris$Sepal.Length, decreasing = FALSE, na.last = FALSE), ])
expect_identical(sort_desc(iris, Sepal.Length), iris[order(iris$Sepal.Length, decreasing = TRUE, na.last = TRUE), ])

expect_identical(sort_asc(iris, Sepal.Length, na.last = TRUE), 
                 iris[order(iris$Sepal.Length, decreasing = FALSE, na.last = TRUE), ])
expect_identical(sort_desc(iris, Sepal.Length, na.last = FALSE), 
                 iris[order(iris$Sepal.Length, decreasing = TRUE, na.last = FALSE), ])

#################



expect_error(sort_asc(iris, "not_exists"))
expect_error(sort_asc(iris, not_exists))
expect_error(sort_asc(iris, 12))

expect_error(sort_asc(as.list(1:3)))
expect_error(sort_desc(as.list(1:3)))

context("sort matrices")
set.seed(123)
mat = matrix(sample(1:4, 18, replace = TRUE), ncol = 3)

expect_identical(sort_asc(mat, 1), mat[order(mat[,1], decreasing = FALSE), ])
expect_identical(sort_asc(mat, 1:2), mat[order(mat[,1], mat[,2], decreasing = FALSE), ])
expect_identical(sort_asc(mat, 1,2), mat[order(mat[,1], mat[,2], decreasing = FALSE), ])

expect_identical(sort_desc(mat, 1), mat[order(mat[,1], decreasing = TRUE), ])
expect_identical(sort_desc(mat, 1:2), mat[order(mat[,1], mat[,2], decreasing = TRUE), ])
expect_identical(sort_desc(mat, 1,2), mat[order(mat[,1], mat[,2], decreasing = TRUE), ])

expect_error(sort_desc(mat, "v1"))

expect_identical(sort_asc(mat[,1,drop = FALSE], 1), mat[,1,drop = FALSE][order(mat[,1], decreasing = FALSE), , drop = FALSE])

colnames(mat) = paste0('v',1:3)
expect_identical(sort_asc(mat, "v1"), mat[order(mat[,1], decreasing = FALSE), ])
expect_identical(sort_asc(mat, v1), mat[order(mat[,1], decreasing = FALSE), ])
expect_identical(sort_desc(mat, 1, v2), mat[order(mat[,1], mat[,2], decreasing = TRUE), ])
expect_identical(sort_desc(mat, list(1, "v2")), mat[order(mat[,1], mat[,2], decreasing = TRUE), ])





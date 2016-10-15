context("sort vector")

set.seed(123)

a = sample(c(1:10, NA, NA, NA))

expect_identical(sort_asc(a), sort(a, decreasing = FALSE, na.last = FALSE))
expect_identical(sort_desc(a), sort(a, decreasing = TRUE, na.last = TRUE))

expect_identical(sort_asc(a, na.last = TRUE), sort(a, decreasing = FALSE, na.last = TRUE))
expect_identical(sort_desc(a, na.last = FALSE), sort(a, decreasing = TRUE, na.last = FALSE))

expect_identical(sort_asc(a, na.last = NA), sort(a, decreasing = FALSE, na.last = NA))
expect_identical(sort_desc(a, na.last = NA), sort(a, decreasing = TRUE, na.last = NA))

b = a
sort_asc(b) = c(na.last = FALSE)

expect_identical(b, sort(a, decreasing = FALSE, na.last = FALSE))

b = a
sort_desc(b) = c(na.last = TRUE)
expect_identical(sort_desc(a), sort(a, decreasing = TRUE, na.last = TRUE))

b = a
sort_desc(b) = c(na.last = NA)
expect_identical(sort_desc(a, na.last = NA), sort(a, decreasing = TRUE, na.last = NA))

b = a
sort_desc(b) = list(na.last = NA)
expect_identical(sort_desc(a, na.last = NA), sort(a, decreasing = TRUE, na.last = NA))


context("sort data.frame")
data(iris)
expect_error(sort_asc(iris))
expect_identical(sort_asc(iris, "Sepal.Length"), iris[order(iris$Sepal.Length, decreasing = FALSE), ])
expect_identical(sort_asc(iris, "Sepal.Length", 2), iris[order(iris$Sepal.Length, iris$Sepal.Width, decreasing = FALSE), ])
expect_identical(sort_asc(iris, qc(Sepal.Length, Sepal.Width), 3:4), 
                 iris[order(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, decreasing = FALSE), ])

expect_identical(sort_asc(iris, 3:4, qc(Sepal.Length, Sepal.Width)), 
                 iris[order(iris$Petal.Length, iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width,  decreasing = FALSE), ])


expect_identical(sort_desc(iris, "Sepal.Length"), iris[order(iris$Sepal.Length, decreasing = TRUE), ])
expect_identical(sort_desc(iris, "Sepal.Length", 2), iris[order(iris$Sepal.Length, iris$Sepal.Width, decreasing = TRUE), ])
expect_identical(sort_desc(iris, qc(Sepal.Length, Sepal.Width), 3:4), 
                 iris[order(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, decreasing = TRUE), ])

expect_identical(sort_desc(iris, 3:4, qc(Sepal.Length, Sepal.Width)), 
                 iris[order(iris$Petal.Length, iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width,  decreasing = TRUE), ])

single_col = iris[,"Sepal.Length", drop = FALSE]
expect_identical(sort_asc(single_col, "Sepal.Length"), 
                 single_col[order(single_col$Sepal.Length, decreasing = FALSE), , drop = FALSE])
expect_identical(sort_desc(single_col, "Sepal.Length"), 
                 single_col[order(single_col$Sepal.Length, decreasing = TRUE), , drop = FALSE])
############################

data(iris)

s_iris = iris
sort_asc(s_iris) = "Sepal.Length"
expect_identical(s_iris, iris[order(iris$Sepal.Length, decreasing = FALSE), ])

s_iris = iris
sort_asc(s_iris) = list(qc(Sepal.Length, Sepal.Width), 3:4)
expect_identical(s_iris, 
                 iris[order(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, decreasing = FALSE), ])

s_iris = iris
sort_desc(s_iris) = list(qc(Sepal.Length, Sepal.Width), 3:4)
expect_identical(s_iris, 
                 iris[order(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, decreasing = TRUE), ])



#######################
expect_identical(iris %sort_desc% "Sepal.Length", iris[order(iris$Sepal.Length, decreasing = TRUE), ])
expect_identical(iris %sort_desc% "Species", iris[order(iris$Species, decreasing = TRUE), ])
expect_identical(iris %sort_desc% list("Sepal.Length", 2), iris[order(iris$Sepal.Length, iris$Sepal.Width, decreasing = TRUE), ])
expect_identical(iris %sort_asc% list("Sepal.Length", 2), iris[order(iris$Sepal.Length, iris$Sepal.Width, decreasing = FALSE), ])

data(mtcars)
expect_identical(sort_asc(mtcars, 2:1), sort_asc(mtcars, "cyl", "mpg"))

iris[1:5,1] = NA

expect_identical(sort_asc(iris, "Sepal.Length"), iris[order(iris$Sepal.Length, decreasing = FALSE, na.last = FALSE), ])
expect_identical(sort_desc(iris, "Sepal.Length"), iris[order(iris$Sepal.Length, decreasing = TRUE, na.last = TRUE), ])

expect_identical(sort_asc(iris, "Sepal.Length", na.last = TRUE), 
                 iris[order(iris$Sepal.Length, decreasing = FALSE, na.last = TRUE), ])
expect_identical(sort_desc(iris, "Sepal.Length", na.last = FALSE), 
                 iris[order(iris$Sepal.Length, decreasing = TRUE, na.last = FALSE), ])

#################
iris[1:5,1] = NA

s_iris = iris

sort_asc(s_iris) = c("Sepal.Length", na.last = TRUE)

expect_identical(s_iris, 
                 iris[order(iris$Sepal.Length, decreasing = FALSE, na.last = TRUE), ])
s_iris = iris

sort_desc(s_iris) = list("Sepal.Length", na.last = FALSE)
expect_identical(s_iris, 
                 iris[order(iris$Sepal.Length, decreasing = TRUE, na.last = FALSE), ])


######################

def = iris
default_dataset(def)

.sort_asc("Sepal.Length")
expect_identical(def, iris[order(iris$Sepal.Length, decreasing = FALSE, na.last = FALSE), ])

.sort_desc("Sepal.Length")
expect_identical(def, iris[order(iris$Sepal.Length, decreasing = TRUE, na.last = TRUE), ])

.sort_asc("Sepal.Length", na.last = TRUE)
expect_identical(def, 
                 iris[order(iris$Sepal.Length, decreasing = FALSE, na.last = TRUE), ])

.sort_desc("Sepal.Length", na.last = FALSE)
expect_identical(def, 
                 iris[order(iris$Sepal.Length, decreasing = TRUE, na.last = FALSE), ])


expect_error(sort_asc(iris, "not_exists"))
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
expect_identical(sort_desc(mat, 1,"v2"), mat[order(mat[,1], mat[,2], decreasing = TRUE), ])
expect_identical(sort_desc(mat, list(1,"v2")), mat[order(mat[,1], mat[,2], decreasing = TRUE), ])

############
set.seed(123)
mat = matrix(sample(1:4, 18, replace = TRUE), ncol = 3)
s_mat = mat

sort_asc(s_mat) = 1 
expect_identical(s_mat, mat[order(mat[,1], decreasing = FALSE), ])

s_mat = mat

sort_desc(s_mat) = 1:2 
expect_identical(sort_desc(mat, 1:2), mat[order(mat[,1], mat[,2], decreasing = TRUE), ])







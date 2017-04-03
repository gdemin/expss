context("common case")

a = 1:3
b = 3:1
d = NA
data(iris)

expect_equal(sum_row(a, b, d), c(4,4,4))
expect_equal(sum_col(a, b, d), c(a = 6, b=6, d = 0))
expect_equal(sum_col(iris[,-5], iris[,-c(1,5)]), c(colSums(iris[,-5]), colSums(iris[,-c(1,5)])))

expect_equal(sum_col(x1 = a, x2 = b, x3= d), c(x1 = 6, x2 = 6, x3 = 0))
expect_equal(sum_col(x1 = a, x2 = b, d), c(x1 = 6, x2 = 6, d = 0))

data(iris)
expect_equal(mean(iris[,-5]), mean(unlist(iris[,-5])))
expect_equal(mean_row(a, b, d), c(2,2,2))
expect_equal(mean_col(a, b, d), c(a = 2, b = 2, d = NA))

expect_equal(median_row(a, b, d), c(2,2,2))
expect_equal(median_col(a, b, d), c(a = 2, b = 2, d = NA))

expect_equal(max_row(a, b, d), c(3,2,3))
expect_equal(max_col(a, b, d), c(a = 3, b = 3, d = NA))

expect_equal(min_row(a, b, d), c(1,2,1))
expect_equal(min_col(a, b, d), c(a = 1, b = 1, d = NA))

#######
expect_equal(max_row(iris[,-5]), apply(iris[,-5], 1, max))
expect_equal(max_col(iris), apply(iris, 2, max))

expect_equal(min_row(iris[,-5]), apply(iris[,-5], 1, min))
expect_equal(min_col(iris), apply(iris, 2, min))

temp = cbind(a, b, NA)
expect_equal(sd_row(a, b, d), apply(temp, 1, sd, na.rm = TRUE))
expect_equal(unname(sd_col(a, b, d)), unname(apply(temp, 2, sd)))

expect_equal(apply_row(sum, a, b, d), 1*c(NA,NA,NA))
expect_equal(unname(apply_col(sum, a, b, d)), c(6,6,NA))

context("edge cases")

a = 1:3
b = 3:1
d = numeric(0)

expect_equal(sum_row(a, b, d), c(4,4,4))
expect_equal(unname(sum_col(a, b, d)), c(6,6,0))


expect_equal(mean_row(a, b, d), c(2,2,2))
expect_equal(unname(mean_col(a, b, d)), c(2,2,NA))

expect_equal(median_row(a, b, d), c(2,2,2))
expect_equal(unname(median_col(a, b, d)), c(2,2,NA))

expect_equal(max_row(a, b, d), c(3,2,3))
expect_equal(unname(max_col(a, b, d)), c(3,3,NA))

expect_equal(min_row(a, b, d), c(1,2,1))
expect_equal(unname(min_col(a, b, d)), c(1,1,NA))

temp = cbind(a, b, NA)
expect_equal(sd_row(a, b, d), apply(temp, 1, sd, na.rm = TRUE))
expect_equal(unname(sd_col(a, b, d)), unname(apply(temp, 2, sd, na.rm = TRUE)))

expect_equal(apply_row(sum, a, b, d), 1*c(NA,NA,NA))
expect_equal(unname(apply_col(sum, a, b, d)), c(6,6,NA))

new_median = with(iris, median_row(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width))
new_mean = with(iris, mean_row(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width))

expect_equal(new_median, apply(iris[,-5], 1, median))
expect_equal(new_mean, apply(iris[,-5], 1, mean))


context("any/all")

a = c(T, NA, F, T, NA, F, T, NA, F)
b = c(T, T, T, F, F, F, NA, NA, NA)

expect_identical(any_in_row(a, b),
                 c(TRUE,  TRUE,  TRUE,  TRUE, NA, FALSE, TRUE, NA, NA))

expect_identical(any_in_row(data.frame(a, b)),
                 c(TRUE,  TRUE,  TRUE,  TRUE, NA, FALSE, TRUE, NA, NA))

expect_identical(any_in_col(rbind(a, b)),
                 c('1' = TRUE,  '2' = TRUE,  '3'=TRUE,  '4'=TRUE, '5' = NA, '6' = FALSE, '7' = TRUE, '8' = NA, '9' = NA))

expect_identical(any_in_col(a, b),
                 c(a = TRUE,  b = TRUE))

####
expect_identical(all_in_row(a, b),
                 c(TRUE, NA, FALSE, FALSE, FALSE, FALSE,    NA,    NA, FALSE))

expect_identical(all_in_row(data.frame(a, b)),
                 c(TRUE, NA, FALSE, FALSE, FALSE, FALSE,    NA,    NA, FALSE))

expect_identical(all_in_col(rbind(a, b)),
                 c('1' = TRUE, '2' = NA, '3' = FALSE, '4' = FALSE, '5' = FALSE, '6' = FALSE, '7' = NA, '8' = NA, '9' = FALSE))

expect_identical(all_in_col(a, b),
                 c(a = FALSE,  b = FALSE))




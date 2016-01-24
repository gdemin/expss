context("simple vector")

expect_identical(if_val(1:5, 1~-1), c(-1, 2, 3, 4, 5))
expect_identical(if_val(1:5, 1~-1, 2 ~ NA), c(-1, NA, 3, 4, 5))
expect_identical(if_val(1:5, ">2"~99), c(1, 2, 99, 99, 99))
expect_identical(if_val(1:5, ">2"~99, . ~ 0), c(0, 0, 99, 99, 99))
expect_identical(if_val(1:5, 1:3 ~ 1, . ~ NA), c(1, 1, 1, NA, NA))
expect_identical(if_val(1:5, 1:3 ~ 1, 2:5 ~ 2), c(1, 1, 1, 2, 2))
expect_identical(if_val(1:5, "<2" ~ 10, "<3" ~ 11, "<4" ~ 12, . ~ NA), c(10, 11, 12, NA, NA))

x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)

expect_identical(if_val(x, ">2"~y), c(1, 8, 1, 9, NA))
expect_identical(if_val(x, list(">2"~y)), c(1, 8, 1, 9, NA))
expect_identical(if_val(x, ">2"~y, "<=2" ~ z), c(4, 8, 4, 9, NA))
expect_identical(if_val(x, ">2"~y, "<=2" ~ z, .~99), c(4, 8, 4, 9, 99))
expect_identical(if_val(x, list(">2"~y, "<=2" ~ z, .~99)), c(4, 8, 4, 9, 99))

expect_identical(if_val(x, (z>4)~y), c(1, 3, 1, 9, 9))

context("dplyr")
library(dplyr)
library(magrittr)


x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)

dfs = data.frame(
    x = c(2,4,2,4,NA),
    y = c(18,18,18,19,19),
    z = c(14,14,14,15,15)
    
)

dfs  %<>% mutate(
    w = if_val(x, ">2"~y)
)

expect_identical(dfs$w, c(2, 18, 2, 19, NA))

dfs$x = NULL
dfs$w = NULL
dfs  %<>% mutate(
    w = if_val(x, ">2"~y)
)
expect_identical(dfs$w, c(1, 18, 1, 19, NA))





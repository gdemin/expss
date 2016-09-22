context("info")

vec = c(1:2,NA)
expect_equal_to_reference(info(vec),"rds/info1.rds")

val_lab(vec) = c(a=1,b=2,d=45)
expect_equal_to_reference(info(vec),"rds/info2.rds")
var_lab(vec) = "Short vec"

expect_equal_to_reference(info(vec),"rds/info3.rds")

vec = data.frame(first=vec,second=vec)
expect_equal_to_reference(info(vec),"rds/info4.rds")

data(iris)
expect_equal_to_reference(info(iris),"rds/info5.rds")
expect_equal_to_reference(info(iris,max_levels=3),"rds/info6.rds")
expect_equal_to_reference(info(iris,frequencies = FALSE),"rds/info7.rds")
expect_equal_to_reference(info(iris,stats = FALSE, frequencies = FALSE),"rds/info8.rds")

vec = NA
expect_equal_to_reference(info(vec),"rds/info9.rds")

vec = rep(NA, 5)
expect_equal_to_reference(info(vec),"rds/info10.rds")

vec = data.frame(first=vec,second=vec)
expect_equal_to_reference(info(vec),"rds/info11.rds")

lst = list(a = c(1:5,NA), b = c(45,NA))
expect_equal_to_reference(info(lst),"rds/info12.rds")

vec = c(2, 2, 2, 1, NA, NA, NA, 3, 3)
expect_equal_to_reference(info(vec), "rds/info13.rds")


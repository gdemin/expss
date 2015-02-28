context("info")

vec = c(1:2,NA)
expect_equal_to_reference(info(vec),"info1.rds")

val_lab(vec) = c(a=1,b=2,d=45)
expect_equal_to_reference(info(vec),"info2.rds")
var_lab(vec) = "Short vec"

expect_equal_to_reference(info(vec),"info3.rds")

vec = data.frame(first=vec,second=vec)
expect_equal_to_reference(info(vec),"info4.rds")

data(iris)
expect_equal_to_reference(info(iris),"info5.rds")
expect_equal_to_reference(info(iris,max_levels=3),"info6.rds")
expect_equal_to_reference(info(iris,frequencies = FALSE),"info7.rds")

context("dtfrm")

df1 = data.frame(a = letters[1:3], "This is my long name" = 1:3, 
                 check.names = FALSE,
                 stringsAsFactors = FALSE
                 )
df2 = dtfrm(a = letters[1:3], "This is my long name" = 1:3)

expect_identical(df1, df2)

a = 1:3
b = 3:1
 
list1 = list(a, b)
names(list1) = c("a", "b")
list2 = lst(a, b)

expect_identical(list1, list2)

list1 = list(a, b)
names(list1) = c("a", "d")
list2 = lst(a, d = b)

expect_identical(list1, list2)

list1 = list(a, b)
names(list1) = c("a", "Very long and `complex` name")
list2 = lst(a, "Very long and `complex` name" = b)

expect_identical(list1, list2)

data(iris)
ir = iris
default_dataset(ir)
 
res1 = .dtfrm(Sepal.Width,  Sepal.Length)
expect_identical(res1, with(iris, dtfrm(Sepal.Width,  Sepal.Length)))
res2 = .lst(Sepal.Width,  Sepal.Length)
expect_identical(res2, with(iris, lst(Sepal.Width,  Sepal.Length)))

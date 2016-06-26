context("dfrm")

df1 = data.frame(a = letters[1:3], "This is my long name" = 1:3, 
                 check.names = FALSE,
                 stringsAsFactors = FALSE
                 )
df2 = dfrm(a = letters[1:3], "This is my long name" = 1:3)

expect_identical(df1, df2)

a = 1:3
b = 3:1
 
list1 = list(a, b)
names(list1) = c("a", "b")
list2 = lst(a, b)

expect_identical(list1, list2)
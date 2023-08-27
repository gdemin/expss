context("add_columns")
 
sh1 = sheet(a = 1:5, b = 5:1)
sh2 = sheet(a = 4:2, d = 4:2)

res = text_to_columns_csv("
                a,b,d
                1,5,NA
                2,4,2
                3,3,3
                4,2,4
                5,1,NA
                ")

expect_identical(add_columns(sh1, sh2), res)
expect_error(add_columns(sh1, sh2, by = "ee"))
expect_error(add_columns(sh1, sh2, by = c("d","ee")))
expect_error(add_columns(sh1, sh2, by = c("a","a")))
expect_identical(add_columns(sh1, sh2, by = "a"), res)
expect_identical(add_columns(sh1, sh2, by = 1), res)

sh2 = sheet(e = 4:2, d = 4:2)
expect_identical(add_columns(sh1, sh2, by = 1), res)
expect_identical(add_columns(sh1, sh2, by = c(a = "e")), res)
expect_error(add_columns(sh1, sh2))

sh2 = sheet(a = c(4:2,2), d = c(4:2, 42L))
expect_error(add_columns(sh1, sh2, by =1))
expect_identical(add_columns(sh1, sh2, by = "a", ignore_duplicates = TRUE), res)

sh2 = sheet(a = 4:2, d = 4:2)

res = text_to_columns_csv("
                    a,b,d
                    1,5,NA
                    2,4,4
                    3,3,3
                    4,2,2
                    5,1,NA
                    ")

expect_identical(add_columns(sh1, sh2, by = c("b" = "a")), res)

res = text_to_columns_csv("
                a,b,b_1
                    1,5,NA
                    2,4,2
                    3,3,3
                    4,2,4
                    5,1,NA
                    ")

sh2 = sheet(a = 4:2, b = 4:2)

expect_warning(add_columns(sh1, sh2, by = "a"))
expect_identical(suppressWarnings(add_columns(sh1, sh2, by = "a")), res)

sh2 = sheet(a = 4:2, b = 2:4, d = c(NA, NA, 42L))

res = text_to_columns_csv("
                a,b,d
                    1,5,NA
                    2,4,42
                    3,3,NA
                    4,2,NA
                    5,1,NA
                    ")

expect_identical(add_columns(sh1, sh2), res)
expect_identical(add_columns(sh1, sh2, by = c("a", "b")), res)
expect_identical(add_columns(sh1, sh2, by = c("b", "a")), res)
sh2 = sheet(a = 4:2, ee = 2:4, d = c(NA, NA, 42L))
expect_identical(add_columns(sh1, sh2, by = c("b" = "ee", "a")), res)

sh2 = sheet(a = 4:2, b = 2:4, d = c(NA, NA, 42L))
res = text_to_columns_csv("
                a,b,d
                    1,5,NA
                    2,4,NA
                    3,3,NA
                    4,2,42
                    5,1,NA
                    ")

expect_identical(add_columns(sh1, sh2, by = c("b" = "a", "a" = "b")), res)




############################
context("add_columns data.table")
# data.table is modified by reference
sh1 = data.table(a = 1:5, b = 5:1)
sh2 = data.table(a = 4:2, d = 4:2)
res = data.table(text_to_columns_csv("
                a,b,d
                1,5,NA
                2,4,2
                3,3,3
                4,2,4
                5,1,NA
                "))

# sh1 and sh2 both are data.tables
expect_identical(add_columns(sh1, sh2), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = "a"), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = 1), res)

sh1 = data.table(a = 1:5, b = 5:1)
sh2 = data.table(e = 4:2, d = 4:2)
expect_identical(add_columns(sh1, sh2, by = 1), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = c(a = "e")), res)

# only data is data.table
sh2 = sheet(a = 4:2, d = 4:2)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = "a"), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = 1), res)

sh1 = data.table(a = 1:5, b = 5:1)
sh2 = sheet(e = 4:2, d = 4:2)
expect_identical(add_columns(sh1, sh2, by = 1), res)
sh1 = data.table(a = 1:5, b = 5:1)
expect_identical(add_columns(sh1, sh2, by = c(a = "e")), res)
sh1 = data.table(a = 1:5, b = 5:1)
sh2 = data.table(a = 4:2, b = 2:4, d = c(NA, NA, 42L))
res = as.data.table(text_to_columns_csv("
                a,b,d
                    1,5,NA
                    2,4,NA
                    3,3,NA
                    4,2,42
                    5,1,NA
                    "))

expect_identical(add_columns(sh1, sh2, by = c("b" = "a", "a" = "b")), res)


# only dict is data.table
sh1 = sheet(a = 1:5, b = 5:1)
sh2 = data.table(a = 4:2, d = 4:2)
res = text_to_columns_csv("
                a,b,d
               1,5,NA
               2,4,2
               3,3,3
               4,2,4
               5,1,NA
               ")
expect_identical(add_columns(sh1, sh2), res)
expect_identical(add_columns(sh1, sh2, by = "a"), res)
expect_identical(add_columns(sh1, sh2, by = 1), res)

sh2 =  data.table(e = 4:2, d = 4:2)
expect_identical(add_columns(sh1, sh2, by = 1), res)
expect_identical(add_columns(sh1, sh2, by = c(a = "e")), res)


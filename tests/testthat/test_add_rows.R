context("add_rows")

a = data.frame(x = 1:5, y = 6:10)
b = data.frame(y = 6:10, z = 11:15)
d = data.frame(y = 6:10, w = 16:20)
e = data.frame(f = 21:25, g = 26:30)

expect_equal_to_reference(add_rows(a, b), "rds/add_rows1.rds")
expect_equal_to_reference(add_rows(a, b, d), "rds/add_rows2.rds")
expect_equal_to_reference(add_rows(a, b, d, e), "rds/add_rows3.rds")

expect_equal_to_reference(add_rows(a, b, nomatch_columns = "drop"), "rds/add_rows4.rds")
expect_equal_to_reference(add_rows(a, b, d, nomatch_columns = "drop"), "rds/add_rows5.rds")
expect_equal_to_reference(add_rows(a, b, d, e, nomatch_columns = "drop"), "rds/add_rows6.rds")

expect_equal_to_reference(add_rows(a, NA), "rds/add_rows6a.rds")
expect_equal_to_reference(add_rows(a, 1:2), "rds/add_rows6b.rds")

context("add_rows default dataset")
a = data.frame(x = 1:5, y = 6:10)
default_dataset(a)
.add_rows(b)
expect_equal_to_reference(a, "rds/add_rows1.rds")

a = data.frame(x = 1:5, y = 6:10)
.add_rows(b, d)
expect_equal_to_reference(a, "rds/add_rows2.rds")

a = data.frame(x = 1:5, y = 6:10)
.add_rows(b, d, e)
expect_equal_to_reference(a, "rds/add_rows3.rds")

a = data.frame(x = 1:5, y = 6:10)
.add_rows(b, nomatch_columns = "drop")
expect_equal_to_reference(a, "rds/add_rows4.rds")

a = data.frame(x = 1:5, y = 6:10)
.add_rows(b, d, nomatch_columns = "drop")
expect_equal_to_reference(a, "rds/add_rows5.rds")

a = data.frame(x = 1:5, y = 6:10)
.add_rows(b, d, e, nomatch_columns = "drop")
expect_equal_to_reference(a, "rds/add_rows6.rds")

a = data.frame(x = 1:5, y = 6:10)
.add_rows(NA)
expect_equal_to_reference(a, "rds/add_rows6a.rds")

a = data.frame(x = 1:5, y = 6:10)
.add_rows(1:2)
expect_equal_to_reference(a, "rds/add_rows6b.rds")

expect_error(add_rows(a, b, nomatch_columns = "stop"))

context("add_rows.etable")

a = data.frame(x = 1:5, y = 6:10)
b = data.frame(y = 6:10, z = 11:15)
d = data.frame(y = 6:10, w = 16:20)
e = data.frame(f = 21:25, g = 26:30)


class(a) = union(c("table_cases", "etable"), class(a))
class(b) = union(c("table_cases", "etable"), class(b))
class(d) = union(c("table_cpct", "etable"), class(d))

expect_equal_to_reference(add_rows(a, b), "rds/add_rows7e.rds")
expect_equal_to_reference(add_rows(a, b, d), "rds/add_rows8e.rds")
expect_equal_to_reference(add_rows(a, b, d, e), "rds/add_rows9e.rds")

expect_equal_to_reference(add_rows(a, NA), "rds/add_rows10e.rds")
expect_equal_to_reference(add_rows(a, 1:2), "rds/add_rows11e.rds")
expect_equal_to_reference(add_rows(a, b, d, e), "rds/add_rows12e.rds")


context("labels preserving")

a = dtfrm(x = 1:5, y = 11:15)
b = dtfrm(x = 6:10, z = 16:20)

var_lab(b$z) = "my z"
val_lab(b$z) = c("zzz" = 1)

var_lab(a$y) = "my y"
val_lab(a$y) = c("yyy" = 1)

res = rbind(
    dtfrm(x = a$x, y = a$y, z = NA),
    dtfrm(x = b$x, y = NA, z = b$z)
)

var_lab(res$z) = "my z"
val_lab(res$z) = c("zzz" = 1)

var_lab(res$y) = "my y"
val_lab(res$y) = c("yyy" = 1)

expect_identical(add_rows(a, b), res)

context("simple_table")
data(mtcars)
mtcars = modify(mtcars, {
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(vs) = "vs"
    val_lab(vs) = c("V-engine" = 0, "Straight engine" = 1)
    var_lab(am) = "am"
    val_lab(am) = c("automatic transmission" = 1, "manual transmission" = 0)
    var_lab(gear) = "gear"
    var_lab(carb) = "carb"})

tab1 = with(mtcars, cro_mean(mpg, am))
tab2 = with(mtcars, cro_cpct(vs, am))
tab3 = with(mtcars, cro_cpct(vs, carb))


expect_equal_to_reference(tab1 %add_rows% tab2, "rds/add_rows7.rds")
expect_identical(tab1 %add_rows% tab2, add_rows(tab1, tab2))

expect_equal_to_reference(tab1 %add_rows% tab3, "rds/add_rows8.rds")
expect_identical(tab1 %add_rows% tab3, add_rows(tab1, tab3, nomatch_columns = "add"))
expect_equal_to_reference(add_rows(tab1, tab3, nomatch_columns = "drop"), "rds/add_rows9.rds")
expect_error(add_rows(tab1, tab3, nomatch_columns = "stop"))


expect_equal_to_reference(with(mtcars, fre(am) %add_rows% fre(vs)), "rds/add_rows10.rds")

a = matrix(1:9, 3)
expect_identical(add_rows(a, NA), rbind(a, NA))


context("add_rows duplicated column names")


data(iris)

iris1 = iris
iris2 = iris

var_lab(iris1[[1]]) = "1"
var_lab(iris2[[1]]) = "2"

colnames(iris1)[1:5] = "a"
colnames(iris2)[1:5] = "b"

res = rbind(cbind(iris1, b = NA, b = NA, b = NA, b = NA, b = NA),
            cbind(a = NA, a = NA, a = NA, a = NA, a = NA, iris2)
)

var_lab(res[[1]]) = "1"
var_lab(res[[6]]) = "2"

expect_identical(add_rows(iris1, iris2), res)



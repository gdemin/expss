context("add_rows")

a = data.frame(x = 1:5, y = 6:10)
b = data.frame(y = 6:10, z = 11:15)
d = data.frame(y = 6:10, w = 16:20)
e = data.frame(f = 21:25, g = 26:30)

expect_known_value(add_rows(a, b), "rds/add_rows1.rds",  update = FALSE)
expect_known_value(add_rows(a, b, d), "rds/add_rows2.rds",  update = FALSE)
expect_known_value(add_rows(a, b, d, e), "rds/add_rows3.rds",  update = FALSE)

expect_known_value(add_rows(a, b, nomatch_columns = "drop"), "rds/add_rows4.rds",  update = FALSE)
expect_known_value(add_rows(a, b, d, nomatch_columns = "drop"), "rds/add_rows5.rds",  update = FALSE)


expect_known_value(add_rows(a, NA), "rds/add_rows6a.rds",  update = FALSE)
expect_known_value(add_rows(a, 1:2), "rds/add_rows6b.rds",  update = FALSE)


context("add_rows.etable")

a = data.frame(x = 1:5, y = 6:10)
b = data.frame(y = 6:10, z = 11:15)
d = data.frame(y = 6:10, w = 16:20)
e = data.frame(f = 21:25, g = 26:30)


class(a) = union(c("etable"), class(a))
class(b) = union(c("etable"), class(b))
class(d) = union(c("etable"), class(d))

expect_known_value(add_rows(a, b), "rds/add_rows7e.rds",  update = FALSE)
expect_known_value(add_rows(a, a), "rds/add_rows7ee.rds",  update = FALSE)
expect_known_value(add_rows(a, b, d), "rds/add_rows8e.rds",  update = FALSE)
expect_known_value(add_rows(a, b, d, e), "rds/add_rows9e.rds",  update = FALSE)

expect_known_value(add_rows(a, NA), "rds/add_rows10e.rds",  update = FALSE)
expect_known_value(add_rows(a, 1:2), "rds/add_rows11e.rds",  update = FALSE)



context("labels preserving")

a = sheet(x = 1:5, y = 11:15)
b = sheet(x = 6:10, z = 16:20)

var_lab(b$z) = "my z"
val_lab(b$z) = c("zzz" = 1)

var_lab(a$y) = "my y"
val_lab(a$y) = c("yyy" = 1)

res = rbind(
    sheet(x = a$x, y = a$y, z = NA),
    sheet(x = b$x, y = NA, z = b$z)
)

var_lab(res$z) = "my z"
val_lab(res$z) = c("zzz" = 1)

var_lab(res$y) = "my y"
val_lab(res$y) = c("yyy" = 1)

expect_identical(add_rows(a, b), res)

context("etable")
data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (lb/1000)",
                      qsec = "1/4 mile time",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c("automatic transmission" = 1,
                             "manual transmission"=0),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)


tab1 = with(mtcars, cro_mean(mpg, list(unvr(am), total())))
tab2 = with(mtcars, cro_cpct(list(unvr(vs)), list(unvr(am), total())))
tab3 = with(mtcars, cro_cpct(list(unvr(vs)), list(unvr(carb), total())))


expect_known_value(add_rows(tab1, tab2), "rds/add_rows7.rds",  update = FALSE)
expect_identical(add_rows(tab1, tab2), add_rows(tab1, tab2))

expect_known_value(add_rows(tab1, tab3), "rds/add_rows8.rds",  update = FALSE)
expect_identical(add_rows(tab1, tab3), add_rows(tab1, tab3, nomatch_columns = "add"))
expect_known_value(add_rows(tab1, tab3, nomatch_columns = "drop"), "rds/add_rows9.rds",  update = FALSE)
expect_error(add_rows(tab1, tab3, nomatch_columns = "stop"))



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



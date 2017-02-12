context("table_cases")

data(mtcars)
mtcars = modify(mtcars,{
    var_lab(vs) = "Engine"
    val_lab(vs) = c("V-engine" = 0,
                    "Straight engine" = 1)
    var_lab(am) = "Transmission"
    val_lab(am) = c(automatic = 0,
                    manual=1)
})

expect_equal_to_reference(table_cases(mtcars$am, mtcars$vs), "rds/table_cases1.rds")
expect_equal_to_reference(table_cases(mtcars$am, mtcars$vs, total_row_position = "none"), "rds/table_cases2.rds")
expect_equal_to_reference(table_cases(mtcars$am, mtcars$vs, total_row_position = "above"), "rds/table_cases3.rds")
expect_equal_to_reference(table_cases(mtcars$am, mtcars$vs, total_row_position = "below"), "rds/table_cases1.rds")


mult1 = dtfrm(v1 = c(1,1,NA), v2 = c(NA, 2, 2))
mult2 = dtfrm(b1 = c(1, 1, 2), b2 = c(3, NA, 1))
weight = c(1, 2, 3)
empty1 = as.numeric(NA)
empty2 = as.numeric(NA)

expect_equal_to_reference(table_cases(mrset(mult1), mult2$b1),
                          "rds/table_cases7.rds")

expect_equal_to_reference(table_cases(mult2$b1, mrset(mult1)),
                          "rds/table_cases8.rds")

expect_equal_to_reference(table_cases(mrset(mult1), mult2$b1, weight),
                          "rds/table_cases9.rds")
expect_equal_to_reference(table_cases(mrset(mult1), mult2$b1, weight, weighted_total = TRUE),
                          "rds/table_cases9a.rds")

expect_equal_to_reference(table_cases(mult2$b1, mrset(mult1), weight),
                          "rds/table_cases10.rds")

expect_equal_to_reference(table_cases(mult2$b1, mrset(mult1), weight, weighted_total = TRUE),
                          "rds/table_cases10a.rds")

#############
expect_equal_to_reference(table_cases(mrset(mult1), mrset(mult2)),
                          "rds/table_cases11.rds")


expect_equal_to_reference(table_cases(mrset(mult1), mrset(mult2),  weight),
                          "rds/table_cases12.rds")

expect_equal_to_reference(table_cases(mrset(mult1), mrset(mult2),  weight, weighted_total = TRUE),
                          "rds/table_cases12a.rds")

expect_equal_to_reference(table_cases(empty1, mrset(mult2)),
                          "rds/table_cases4.rds")


expect_equal_to_reference(table_cases(mrset(mult1), empty1, weight),
                          "rds/table_cases5.rds")

expect_equal_to_reference(table_cases(mrset(mult1), mrset(mult2), empty1),
                          "rds/table_cases6.rds")

expect_equal_to_reference(table_cases(mrset(mult1, empty1), mrset(mult2, empty1), weight = weight),
                          "rds/table_cases13.rds")

expect_equal_to_reference(table_cases(mrset(mult1), "Total", weight = weight),
                          "rds/table_cases14.rds")

expect_equal_to_reference(table_cases("Total", mrset(mult2), weight = weight),
                          "rds/table_cases15.rds")

# fre(mtcars$vs)
# with(mtcars, cro(am, vs))

data(mtcars)
mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"
    var_lab(vs) = "Engine"
    val_lab(vs) = c("V-engine" = 0,
                    "Straight engine" = 1)
    var_lab(am) = "Transmission"
    val_lab(am) = c(automatic = 0,
                    manual=1)
    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
})
expect_equal_to_reference(table_cases(list(mtcars$am), list(mtcars$vs)), "rds/table_cases1.rds")
expect_equal_to_reference(table_cases(list(mtcars$am, mtcars$vs), list(mtcars$vs)), "rds/table_cases22.rds")
expect_equal_to_reference(table_cases(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear)), "rds/table_cases23.rds")

expect_equal_to_reference(
    table_cases(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear), variable_labels_position = "row"),
    "rds/table_cases23.rds")

expect_equal_to_reference(
    table_cases(dtfrm(mtcars$am, mtcars$cyl), dtfrm(mtcars$vs, mtcars$gear), variable_labels_position = "row"),
    "rds/table_cases23.rds")

expect_equal_to_reference(
    table_cases(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear), variable_labels_position = "inside_columns"),
    "rds/table_cases23a.rds")

expect_equal_to_reference(
    table_cases(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear), variable_labels_position = "outside_columns"),
    "rds/table_cases23b.rds")

expect_equal_to_reference(
    table_cases(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear), total_row_position = "above"),
    "rds/table_cases24.rds")
expect_equal_to_reference(
    table_cases(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear), total_row_position = "none"),
    "rds/table_cases25.rds")
expect_equal_to_reference(
    table_cases(list(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length), list(iris$Species, iris$Petal.Width)),
    "rds/long_table26.rds")


context("table_cases some exotics")

mult1 = dtfrm(v1 = c(1,1,NA), v2 = c(NA, 2, 2))
mult2 = dtfrm(b1 = c(1, 1, 2), b2 = c(3, NA, 1))
var_lab(mult1) = "mult1"
var_lab(mult2) = "mult2"
weight = c(1, 2, 3)
empty1 = as.numeric(NA)
empty2 = as.numeric(NA)


expect_equal_to_reference(
    table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), variable_labels_position = "inside_columns")
    ,"rds/table_cases31.rds")

expect_equal_to_reference(
    table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), variable_labels_position = "outside_columns"),
    "rds/table_cases32.rds")


var_lab(mult2) = "mult1"


expect_equal_to_reference(
    table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), variable_labels_position = "inside_columns"),
    "rds/table_cases33.rds")

expect_equal_to_reference(
    table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), variable_labels_position = "outside_columns"),
    "rds/table_cases34.rds")

val_lab(mult1) = c(a = 1, b = 2, c = 3)
val_lab(mult2) = val_lab(mult1)

expect_equal_to_reference(
    table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), variable_labels_position = "inside_columns"),
    "rds/table_cases35.rds")

expect_equal_to_reference(
    table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), variable_labels_position = "outside_columns"),
    "rds/table_cases36.rds")

add_val_lab(mult1) = c(d = 4)
add_val_lab(mult2) = c(d = 2)


expect_equal_to_reference(
    table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), variable_labels_position = "outside_columns"),
    "rds/table_cases37.rds")

expect_equal_to_reference(
    table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), variable_labels_position = "inside_columns"),
    "rds/table_cases38.rds")

expect_equal_to_reference(
    table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total")),
    "rds/table_cases39.rds")

if_val(mult2) = other ~ NA
var_lab(mult2) = "mult2"
# expect_equal_to_reference(
#     table_cases(list(mult1, mult2), list("total", "total"))
#     ,"rds/table_cases40.rds")

#########################################

# table_cases(list(mult1, mult2), list("total"))

# table_rpct(mtcars$am, mtcars$vs)

# mult1 = dtfrm(a = c(1:2, NA), b = c(NA, 1, 3))
#
# val_lab(mult1$a) = c("one" = 1, "two" = 2, "three" = 3)
#
# table_cases(mult1, "Total")
#
# ### lost labels:(
# mult1 = dtfrm(a = c(1:2, NA), b = c(NA, 1, 3))
#
# val_lab(mult1$b) = c("one" = 1, "two" = 2, "three" = 3)
#
# table_cases(mult1, "Total")
#
#
# aaa = list(data.table(a = "a", b = 1), data.table(a = 1, d ="b"))
# rbindlist(aaa, use.names = TRUE, fill = TRUE)
#
#
# bbb = list(data.table(a = "a", b = 1), data.table(a = set_val_lab(1, c("wah"=1)), d ="b"))
# ddd = rbindlist(bbb, use.names = TRUE, fill = TRUE)
# ddd = rbindlist(bbb)
# str(ddd$a)

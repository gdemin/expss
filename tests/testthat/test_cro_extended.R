context("cro extended")

data(mtcars)
mtcars = modify(mtcars,{
    var_lab(vs) = "Engine"
    val_lab(vs) = c("V-engine" = 0,
                    "Straight engine" = 1)
    var_lab(am) = "Transmission"
    val_lab(am) = c(automatic = 0,
                    manual=1)
})

expect_equal_to_reference(cro(mtcars$am, mtcars$vs), "rds/table_cases1.rds",  update = FALSE)
expect_equal_to_reference(calc_cro_cases(mtcars, am, vs), "rds/table_cases1.rds",  update = FALSE)
expect_equal_to_reference(cro(mtcars$am, mtcars$vs, total_row_position = "none"), "rds/table_cases2.rds",  update = FALSE)
expect_equal_to_reference(cro(mtcars$am, mtcars$vs, total_row_position = "above"), "rds/table_cases3.rds",  update = FALSE)
expect_equal_to_reference(cro(mtcars$am, mtcars$vs, total_row_position = "below"), "rds/table_cases1.rds",  update = FALSE)


mult1 = dtfrm(v1 = c(1,1,NA), v2 = c(NA, 2, 2))
mult2 = dtfrm(b1 = c(1, 1, 2), b2 = c(3, NA, 1))
weight = c(1, 2, 3)
empty1 = as.numeric(NA)
empty2 = as.numeric(NA)

expect_equal_to_reference(cro(list(mrset(mult1)), list(mult2$b1)),
                          "rds/table_cases7.rds",  update = FALSE)

expect_equal_to_reference(cro(list(mult2$b1), list(mrset(mult1))),
                          "rds/table_cases8.rds",  update = FALSE)

expect_equal_to_reference(cro(list(mrset(mult1)), list(mult2$b1), weight = weight),
                          "rds/table_cases9.rds",  update = FALSE)
expect_equal_to_reference(cro(list(mrset(mult1)), list(mult2$b1), weight = weight, total_statistic = "w_cases"),
                          "rds/table_cases9a.rds",  update = FALSE)

expect_equal_to_reference(cro(list(mult2$b1), list(mrset(mult1)), weight = weight),
                          "rds/table_cases10.rds",  update = FALSE)

expect_equal_to_reference(cro(list(mult2$b1), list(mrset(mult1)), weight = weight, total_statistic = "w_cases"),
                          "rds/table_cases10a.rds",  update = FALSE)

# expect_equal_to_references(cro(numeric(0)))

#############
expect_equal_to_reference(cro(list(mrset(mult1)), list(mrset(mult2))),
                          "rds/table_cases11.rds",  update = FALSE)


expect_equal_to_reference(cro(list(mrset(mult1)), list(mrset(mult2)),  weight = weight),
                          "rds/table_cases12.rds",  update = FALSE)

expect_equal_to_reference(cro(list(mrset(mult1)), list(mrset(mult2)),  weight = weight, 
                              total_statistic = "w_cases"),
                          "rds/table_cases12a.rds",  update = FALSE)

expect_equal_to_reference(cro(list(empty1), list(mrset(mult2))),
                          "rds/table_cases4.rds",  update = FALSE)


expect_equal_to_reference(cro(list(mrset(mult1)), list(empty1), weight = weight),
                          "rds/table_cases5.rds",  update = FALSE)

expect_equal_to_reference(cro(list(mrset(mult1)), list(mrset(mult2)), weight = empty1),
                          "rds/table_cases6.rds",  update = FALSE)

expect_equal_to_reference(cro(list(mrset(mult1, empty1)), 
                              list(mrset(mult2, empty1)), weight = weight),
                          "rds/table_cases13.rds",  update = FALSE)

expect_equal_to_reference(cro(list(mrset(mult1)), list("Total"), weight = weight),
                          "rds/table_cases14.rds",  update = FALSE)

expect_equal_to_reference(cro(list("Total"), list(mrset(mult2)), weight = weight),
                          "rds/table_cases15.rds",  update = FALSE)

expect_equal_to_reference(cro(list(mrset("Total")), list(mrset(mult2)), weight = weight),
                          "rds/table_cases15.rds",  update = FALSE)

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
expect_equal_to_reference(cro(list(mtcars$am), list(mtcars$vs)), "rds/table_cases1.rds",  update = FALSE)
expect_equal_to_reference(cro(list(mtcars$am, mtcars$vs), list(mtcars$vs)), "rds/table_cases22.rds",  update = FALSE)
expect_equal_to_reference(cro(list(mtcars$am, list(mtcars$vs)), list(mtcars$vs)), "rds/table_cases22.rds",  update = FALSE)
expect_equal_to_reference(cro(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear)), "rds/table_cases23.rds",  update = FALSE)

expect_equal_to_reference(
    cro(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear)),
    "rds/table_cases23.rds",  update = FALSE)

expect_equal_to_reference(
    cro(dtfrm(mtcars$am, mtcars$cyl), dtfrm(mtcars$vs, mtcars$gear)),
    "rds/table_cases23.rds",  update = FALSE)

# expect_equal_to_reference(
#     table_cases(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear), 
# variable_labels_position = "inside_columns"),
#     "rds/table_cases23a.rds",  update = FALSE)

# expect_equal_to_reference(
#     table_cases(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear),
#                 variable_labels_position = "outside_columns"),
#     "rds/table_cases23b.rds",  update = FALSE)

expect_equal_to_reference(
    cro(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear), total_row_position = "above"),
    "rds/table_cases24.rds",  update = FALSE)

expect_equal_to_reference(
    cro(list(mtcars$am, mtcars$cyl), list(mtcars$vs, mtcars$gear), total_row_position = "none"),
    "rds/table_cases25.rds",  update = FALSE)

expect_equal_to_reference(
    cro(list(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length),
                list(iris$Species, iris$Petal.Width)),
    "rds/long_table26.rds",  update = FALSE)


context("cro extended some exotics")

mult1 = dtfrm(v1 = c(1,1,NA), v2 = c(NA, 2, 2))
mult2 = dtfrm(b1 = c(1, 1, 2), b2 = c(3, NA, 1))
var_lab(mult1) = "mult1"
var_lab(mult2) = "mult2"
weight = c(1, 2, 3)
empty1 = as.numeric(NA)
empty2 = as.numeric(NA)


# expect_equal_to_reference(
#     table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), 
#                 variable_labels_position = "inside_columns")
#     ,"rds/table_cases31.rds",  update = FALSE)

# expect_equal_to_reference(
#     table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), 
#                 variable_labels_position = "outside_columns"),
#     "rds/table_cases32.rds",  update = FALSE)


var_lab(mult2) = "mult1"


# expect_equal_to_reference(
#     table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"),
#                 variable_labels_position = "inside_columns"),
#     "rds/table_cases33.rds",  update = FALSE)

# expect_equal_to_reference(
#     table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), 
#                 variable_labels_position = "outside_columns"),
#     "rds/table_cases34.rds",  update = FALSE)

val_lab(mult1) = c(a = 1, b = 2, c = 3)
val_lab(mult2) = val_lab(mult1)

# expect_equal_to_reference(
#     table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), 
#                 variable_labels_position = "inside_columns"),
#     "rds/table_cases35.rds",  update = FALSE)

# expect_equal_to_reference(
#     table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), 
#                 variable_labels_position = "outside_columns"),
#     "rds/table_cases36.rds",  update = FALSE)

add_val_lab(mult1) = c(d = 4)
add_val_lab(mult2) = c(d = 2)


# expect_equal_to_reference(
#     table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), 
#                 variable_labels_position = "outside_columns"),
#     "rds/table_cases37.rds",  update = FALSE)

# expect_equal_to_reference(
#     table_cases(list(mrset(mult1), mrset(mult2)), list("total", "total"), 
#                 variable_labels_position = "inside_columns"),
#     "rds/table_cases38.rds",  update = FALSE)

expect_equal_to_reference(
    cro(list(mrset(mult1), mrset(mult2)), list("total", "total")),
    "rds/table_cases39.rds",  update = FALSE)



context("cro multiple by multiple")

data("product_test")
codeframe_likes = num_lab("
                          1 Liked everything
                          2 Disliked everything
                          3 Chocolate
                          4 Appearance
                          5 Taste
                          6 Stuffing
                          7 Nuts
                          8 Consistency
                          98 Other
                          99 Hard to answer
                          ")


var_lab(product_test$a1_1) = "Likes. VSX123"
val_lab(product_test$a1_1) = codeframe_likes
var_lab(product_test$b1_1) = "Likes. SDF456"
val_lab(product_test$b1_1) = codeframe_likes
expect_equal_to_reference(
    calc(product_test, cro(mrset(a1_1 %to% a1_6), mrset(b1_1 %to% b1_6))),
    "rds/cro_mult_by_mult.rds",  update = FALSE)

expect_equal_to_reference(
    calc(product_test, cro_cpct(mrset(a1_1 %to% a1_6), mrset(b1_1 %to% b1_6))),
    "rds/cro_cpct_mult_by_mult.rds",  update = FALSE)

expect_equal_to_reference(
    calc_cro_cpct(product_test, mrset(a1_1 %to% a1_6), mrset(b1_1 %to% b1_6)),
    "rds/cro_cpct_mult_by_mult.rds",  update = FALSE)

expect_equal_to_reference(
    calc(product_test, cro_cpct_responses(mrset(a1_1 %to% a1_6), mrset(b1_1 %to% b1_6))),
    "rds/cro_cpct_responses_mult_by_mult.rds",  update = FALSE)


expect_equal_to_reference(
    calc_cro_cpct_responses(product_test, mrset(a1_1 %to% a1_6), mrset(b1_1 %to% b1_6)),
    "rds/cro_cpct_responses_mult_by_mult.rds",  update = FALSE)


expect_equal_to_reference(
    calc(product_test, cro_rpct(mrset(a1_1 %to% a1_6), mrset(b1_1 %to% b1_6))),
    "rds/cro_rpct_mult_by_mult.rds",  update = FALSE)

expect_equal_to_reference(
    calc_cro_rpct(product_test, mrset(a1_1 %to% a1_6), mrset(b1_1 %to% b1_6)),
    "rds/cro_rpct_mult_by_mult.rds",  update = FALSE)


expect_equal_to_reference(
    calc(product_test, cro_tpct(mrset(a1_1 %to% a1_6), mrset(b1_1 %to% b1_6))),
    "rds/cro_tpct_mult_by_mult.rds",  update = FALSE)

expect_equal_to_reference(
    calc_cro_tpct(product_test, mrset(a1_1 %to% a1_6), mrset(b1_1 %to% b1_6)),
    "rds/cro_tpct_mult_by_mult.rds",  update = FALSE)

set.seed(1)
df <- data.frame(area=rep(c('Area 1','Area 2'), each=6),
                 var_orange=sample(0:1, 12, T),
                 var_banana=sample(0:1, 12, T),
                 var_melon=sample(0:1, 12, T),
                 var_mango=sample(0:1, 12, T))

expect_equal_to_reference(
    with(df, cro_cpct(mdset(var_orange, var_banana, var_melon, var_mango), list(area))),
    "rds/cro_mdset_names_of_result.rds",  update = FALSE
)

expect_equal_to_reference(
    calc_cro_cpct(df, mdset(var_orange, var_banana, var_melon, var_mango), list(area)),
    "rds/cro_mdset_names_of_result.rds",  update = FALSE
)


expect_equal_to_reference(
    cro_cpct(mtcars$am, total(label = "|")),
    "rds/cro_total_empty_label.rds",  update = FALSE)

expect_equal_to_reference(
    cro_cpct(mtcars$am, total(label = "")),
    "rds/cro_total_empty_label.rds",  update = FALSE)


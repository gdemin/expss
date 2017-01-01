context("merge")

data("mtcars")

mtcars = modify(mtcars, {
    var_lab(vs) = "vs"
    var_lab(am) = "am"
    val_lab(am) = c("automatic transmission" = 1, "manual transmission" = 0)
    var_lab(gear) = "gear"
    var_lab(carb) = "carb"})


### need tests for simple_table - fre and cro

context("merge simple_table")
expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(carb, am)
    freq %merge% cross


}), "rds/merge7.rds")

expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(carb, am)
    cross %merge% freq


}), "rds/merge8.rds")

expect_equal_to_reference(with(mtcars,{
    freq = fre(carb[carb!=2])
    cross = cro(carb[carb!=4], am[carb!=4])
    freq %merge% cross


}), "rds/merge9.rds")

# weird
expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(vs, am)
    cross %merge% freq


}), "rds/merge10.rds")

expect_equal_to_reference(with(mtcars,{
    freq1 = fre(gear)
    freq2 = fre(carb)
    merge(freq1, freq2, suffixes = c(" gear", " carb"))
}), "rds/merge11.rds")


context("left join")

a = dtfrm(x = 1:5, y = 11:15)
b = dtfrm(x = 3:7, z = 13:17)
d = dtfrm(t = 3:7, z = 13:17)
expect_identical(a %merge% b, merge(a, b, all.x = TRUE, all.y = FALSE))
expect_error(a %merge% d)


context("merge duplicated columns")

data(mtcars)
expect_equal_to_reference(fre(mtcars$cyl[mtcars$mpg<20]) %merge% fre(mtcars$cyl) ,
                           "rds/merge12.rds")


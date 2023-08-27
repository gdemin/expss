context("prop vector")

a = c(25, 25)

expect_identical(prop(a), c(.5, .5))

a = c(25, 25, NA)

expect_identical(prop(a), c(.5, .5, NA))
expect_identical(prop_col(a), c(.5, .5, NA))
expect_identical(prop_row(a), c(1, 1, NA))


expect_identical(prop(0), NaN)
expect_identical(prop(1), 1)

expect_identical(prop_col(0), NaN)
expect_identical(prop_col(1), 1)

expect_identical(prop_row(0), NaN)
expect_identical(prop_row(1), 1)


context("prop matrix")

a = cbind(c(25, 25, NA), c(100, NA, 50))

expect_identical(prop(a), cbind(c(.125, .125, NA), c(.5, NA, .25)))
expect_identical(prop_col(a), cbind(c(.5, .5, NA), c(2/3, NA, 1/3)))
expect_identical(prop_row(a), cbind(c(25/125, 1, NA), c(100/125, NA, 1)))

context("prop data.frame")

a = sheet(a = c(25, 25, NA), b = c(100, NA, 50))

expect_identical(prop(a), sheet(a = c(.125, .125, NA), b = c(.5, NA, .25)))
expect_identical(prop_col(a), sheet(a = c(.5, .5, NA), b = c(2/3, NA, 1/3)))
expect_identical(prop_row(a), sheet(a = c(25/125, 1, NA), b = c(100/125, NA, 1)))

fac = factor(c("a", "b", "c"))
char = c("a", "b", "c")
dat = as.POSIXct("2016-09-27") 

a = sheet(fac, a = c(25, 25, NA), b = c(100, NA, 50), char, dat)

expect_identical(prop(a), sheet(fac, a = c(.125, .125, NA), b = c(.5, NA, .25), char, dat))
expect_identical(prop_col(a), sheet(fac, a = c(.5, .5, NA), b = c(2/3, NA, 1/3), char, dat))
expect_identical(prop_row(a), sheet(fac, a = c(25/125, 1, NA), b = c(100/125, NA, 1), char, dat))

data("warpbreaks")
tbl = with(warpbreaks, table(wool, tension))

expect_identical(prop(tbl), prop.table(tbl))
expect_identical(prop_col(tbl), prop.table(tbl, 2))
expect_identical(prop_row(tbl), prop.table(tbl, 1))


tbl = table(state.division, state.region)
expect_identical(prop(tbl), prop.table(tbl))
expect_identical(prop_col(tbl), prop.table(tbl, 2))
expect_identical(prop_row(tbl), prop.table(tbl, 1))

a_lst = list(a, tbl)
expect_identical(prop(a_lst), list(prop(a), prop(tbl)))
expect_identical(prop_col(a_lst), list(prop_col(a), prop_col(tbl)))
expect_identical(prop_row(a_lst), list(prop_row(a), prop_row(tbl)))





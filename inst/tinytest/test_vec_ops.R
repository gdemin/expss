context("vec ops")

expect_equal(1:4 %a% 5, 1:5)
expect_equal(c(a=1, b=4) %a% c(d = 5), c(a=1, b=4, d = 5))

expect_equal(c(a=1, b=4, d = 5) %d% c(d = 5), c(a=1, b=4))
expect_equal(c(a=1, b=4, d = 5) %d% lt(4), c(b=4, d=5))
expect_equal(v_diff(c(a=1, b=4, d = 5), lt(4)), c(b=4, d=5))

expect_equal(c(a=1, b=4, d = 5) %d% lte(4), c(d=5))
expect_equal(c(a=1, b=4, d = 5) %d% le(4), c(d=5))

expect_equal(c(a=1, b=4, d = 5) %d% gte(4), c(a=1))
expect_equal(c(a=1, b=4, d = 5) %d% ge(4), c(a=1))

expect_equal(c(a=1, b=4, d = 5) %d% eq(4), c(a=1, d=5))
expect_equal(c(a=1, b=4, d = 5) %d% neq(4), c(b=4))
expect_equal(c(a=1, b=4, d = 5) %d% ne(4), c(b=4))
expect_equal(c(a=1, b=4, d = 5) %d% perl(4), c(a=1, d=5))

expect_equal(c(a=1, b=4, d = 5) %r% 2, c(a=1, b=4, d = 5, a=1, b=4, d = 5))

expect_equal( c(1, 2, NA, 3) %i% other, c(1, 2, NA, 3))
expect_equal( c(1, 2, NA, 3) %i% other(), c(1, 2, NA, 3))
expect_equal( c(1, 2, NA, 3) %i% not_na, c(1, 2, 3))
expect_equal( c(1, 2, NA, 3) %i% not_na(), c(1, 2, 3))
expect_equal( v_intersect(c(1, 2, NA, 3), not_na), c(1, 2, 3))
expect_equal( c(1, 2, NA, 3) %d% is.na, c(1, 2, 3))

expect_equal( c(1, 2, NA, 3) %d% other, numeric(0))
expect_equal( c(1, 2, NA, 3) %d% not_na, 1.0*NA)

expect_equal(length(c(a=1, b=4) %i% c(d = 5)), 0)
expect_equal(c(a=1, b=4, f = 5) %i% c(d = 5), c(f = 5))

expect_equal(c(a=1, b=4) %e% c(d = 5), c(a=1, b=4, d = 5))
expect_equal(c(a=1, b=4, 5) %e% c(d = 5), c(a=1, b=4))
expect_equal(v_xor(c(a=1, b=4, 5), c(d = 5)), c(a=1, b=4))


expect_equal(1:4 %a% 5:6   , 1:6)
expect_equal(1:4 %a% 4:5   , c(1,2,3,4,4,5))
expect_equal(1:4 %u% 4:5   , c(1,2,3,4,5))
expect_equal(v_union(1:4, 4:5)  , c(1,2,3,4,5))



expect_equal(NULL %d% as.POSIXct("2016-09-23"), NULL)
expect_equal(NULL %i% as.POSIXct("2016-09-23"), NULL)
expect_equal(NULL %e% as.POSIXct("2016-09-23"), as.POSIXct("2016-09-23"))
expect_equal(as.POSIXct("2016-09-23") %e% NULL, as.POSIXct("2016-09-23"))
expect_equal(NULL %a% as.POSIXct("2016-09-23"), as.POSIXct("2016-09-23"))
expect_equal(as.POSIXct("2016-09-23") %a% NULL, as.POSIXct("2016-09-23"))
expect_equal(NULL %u% as.POSIXct("2016-09-23"), as.POSIXct("2016-09-23"))
expect_equal(v_union(NULL, as.POSIXct("2016-09-23")), as.POSIXct("2016-09-23"))
expect_equal(as.POSIXct("2016-09-23") %u% NULL, as.POSIXct("2016-09-23"))


expect_equal(1:6 %d% 5:6   , 1:4)
expect_equal(1:6 %d% gt(4) , 1:4)
expect_equal(1:4 %i% 4:5   , 4)
expect_equal(letters %i% perl("[a-d]") , letters[1:4])
expect_equal(letters %i% (fixed("a") | fixed("z")) , c("a", "z"))
expect_equal(1:4 %e% 4:5   , c(1, 2, 3, 5))
expect_equal(1:2 %r% 2     , c(1, 2, 1, 2))


context("%n_i%, %n_d%")

expect_identical(iris %n_d% "Species", iris[, -5, drop = FALSE]) # remove column Species
expect_identical(n_diff(iris, "Species"), iris[, -5, drop = FALSE]) # remove column Species
expect_identical(iris %n_i% perl("^Sepal"), iris[, 1:2])
expect_identical(n_intersect(iris, perl("^Sepal")), iris[, 1:2])
# leave column "Species" and columns which start with "Sepal" 
expect_identical(iris %n_i% (perl("^Sepal")|"Species"), iris[, c(1:2,5)]) 

expect_identical(c(a=1, b=4, d = 5) %n_d% "d", c(a=1, b=4))
expect_identical(length(c(a=1, b=4) %n_i% "d"), 0L)
expect_identical(c(a=1, b=4, f = 5) %n_i% "f", c(f = 5))

expect_identical(list(a=1, b=4, d = 5) %n_d% "d", list(a=1, b=4))
expect_identical(length(list(a=1, b=4) %n_i% "d"), 0L)
expect_identical(list(a=1, b=4, f = 5) %n_i% "f", list(f = 5))


expect_identical(as.matrix(iris) %n_d% "Species", as.matrix(iris)[, -5, drop = FALSE]) # remove column Species
expect_identical(as.matrix(iris) %n_i% perl("^Sepal"), as.matrix(iris)[, 1:2])
# leave column "Species" and columns which start with "Sepal" 
expect_identical(as.matrix(iris) %n_i% (perl("^Sepal")|"Species"), as.matrix(iris)[, c(1:2,5)]) 


expect_error(5 %r% 1:2)

context("edge cases")
expect_identical(iris %n_i% NULL, iris[, FALSE, drop = FALSE])
expect_identical(iris %n_d% NULL, iris)
expect_identical(as.matrix(iris) %n_d% NULL, as.matrix(iris))
expect_identical(1:5 %n_d% NULL, 1:5)
expect_identical(1:5 %d% NULL, 1:5)
expect_identical(1:5 %n_i% NULL, integer(0))

expect_identical(iris %n_i% factor("Species"), iris[, 5, drop = FALSE])
expect_identical(iris %n_d% factor("Species"), iris[,-5])



context("vec ops list")
data(iris)
expect_identical(iris %i% is.numeric, iris[,-5])
expect_identical(iris %i% NULL, iris[FALSE])
expect_error(iris %i% 1)

expect_identical(iris %d% is.numeric, iris[, 5, drop = FALSE])
expect_identical(iris %d% NULL, iris)
expect_error(iris %d% 1)


test_vector = c(-5, 7, 0, 5, -8, 12, 1, 2, 3, -1, -2, -3, NA, Inf, -Inf, NaN)
expect_identical( test_vector %d% gt(0), test_vector[!(test_vector>0 & !is.na(test_vector))])
expect_identical( test_vector %i% gt(0), test_vector[(test_vector>0) & !is.na(test_vector)])
expect_identical(
    sort_asc(test_vector %d% gt(0) %a% (test_vector %i% gt(0))),
    sort_asc(test_vector)
)
expect_identical( test_vector %i% (0 %thru% hi), test_vector[(test_vector>=0) & !is.na(test_vector)])

expect_identical(
    test_vector %i% (0 %thru% hi | NA | NaN),
    c( 7, 0, 5, 12, 1, 2, 3,  NA, Inf, NaN)
)

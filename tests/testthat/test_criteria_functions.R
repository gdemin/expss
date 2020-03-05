context("criteria")

expect_equal(gt(5)(5), FALSE)
expect_equal((5 | gt(5))(5), TRUE)
expect_equal(or(5, gt(5))(5), TRUE)


expect_equal((6 & gt(5))(5), FALSE)
expect_equal((6 & gt(5))(6), TRUE)
expect_equal(and(6, gt(5))(6), TRUE)

expect_equal((gt(5) & 6)(5), FALSE)
expect_equal((gt(5) & 6)(6), TRUE)

a = 1:4
expect_equal((eq(4) & (a<2))(a), rep(FALSE, 4))
expect_equal((eq(4) | (a<2))(a), c(TRUE, FALSE, FALSE, TRUE))

expect_equal(((a<2) & eq(4))(a), rep(FALSE, 4))
expect_equal(((a<2) | eq(4))(a), c(TRUE, FALSE, FALSE, TRUE))

num_crit = as.criterion(4)

expect_identical(num_crit(1:4), c(FALSE, FALSE, FALSE, TRUE))

logi_crit = when(c(TRUE, FALSE, FALSE, TRUE))
expect_identical(logi_crit(1:4), c(TRUE, FALSE, FALSE, TRUE))


fun_crit = as.criterion(function(x) x>2)

expect_identical((num_crit | logi_crit | fun_crit) (1:4), c(TRUE, FALSE, TRUE, TRUE))
expect_identical(or(num_crit, logi_crit, fun_crit) (1:4), c(TRUE, FALSE, TRUE, TRUE))


# check correctness of perl/regex/fixed

pattern = "[:alpha:]"

test_str = "abc"

expect_false(contains(pattern)(test_str))
expect_false(fixed(pattern)(test_str))
expect_true(regex(pattern)(test_str))
expect_error(suppressWarning(perl(pattern)(test_str)))

expect_true(not(contains(pattern))(test_str))
expect_true(not(fixed(pattern))(test_str))
expect_false(not(regex(pattern))(test_str))


test_str = c("Abc", "abc", "bcd", "a")

expect_equal(like("a")(test_str), c(FALSE, FALSE, FALSE, TRUE))
expect_equal(like("a*")(test_str), c(TRUE, TRUE, FALSE, TRUE))
expect_equal(like("*d")(test_str), c(FALSE, FALSE, TRUE, FALSE))


vec = c(1:3, 1:3)
val_lab(vec) = num_lab("
                       1 One
                       2 Two
                       3 Three
                       ")

expect_identical(
    has_label("One")(vec),
    vec == 1)

expect_identical(
    has_label(perl("o|O"))(vec),
    vec %in% 1:2)

expect_identical(
    has_label(like("t*"))(vec),
    vec %in% 2:3)

expect_identical(
    has_label("One")(unlab(vec)),
    vec == 99)

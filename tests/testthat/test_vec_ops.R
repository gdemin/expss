context("vec ops")

expect_equal(1:4 %a% 5, 1:5)
expect_equal(c(a=1, b=4) %a% c(d = 5), c(a=1, b=4, d = 5))

expect_equal(c(a=1, b=4, d = 5) %d% c(d = 5), c(a=1, b=4))
expect_equal(c(a=1, b=4, d = 5) %d% lt(4), c(b=4, d=5))
expect_equal(c(a=1, b=4, d = 5) %d% eq(4), c(a=1, d=5))
expect_equal(c(a=1, b=4, d = 5) %d% perl(4), c(a=1, d=5))

expect_equal(c(a=1, b=4, d = 5) %m% 2, c(a=1, b=4, d = 5, a=1, b=4, d = 5))


expect_equal(length(c(a=1, b=4) %i% c(d = 5)), 0)
expect_equal(c(a=1, b=4, f = 5) %i% c(d = 5), c(f = 5))

expect_equal(c(a=1, b=4) %x% c(d = 5), c(a=1, b=4, d = 5))
expect_equal(c(a=1, b=4, 5) %x% c(d = 5), c(a=1, b=4))

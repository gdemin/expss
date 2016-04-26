context("vec ops")

expect_equal(1:4 %a% 5, 1:5)
expect_equal(c(a=1, b=4) %a% c(d = 5), c(a=1, b=4, d = 5))

expect_equal(c(a=1, b=4, d = 5) %d% c(d = 5), c(a=1, b=4))
expect_equal(c(a=1, b=4, d = 5) %d% lt(4), c(b=4, d=5))
expect_equal(c(a=1, b=4, d = 5) %d% eq(4), c(a=1, d=5))
expect_equal(c(a=1, b=4, d = 5) %d% perl(4), c(a=1, d=5))

expect_equal(c(a=1, b=4, d = 5) %r% 2, c(a=1, b=4, d = 5, a=1, b=4, d = 5))


expect_equal(length(c(a=1, b=4) %i% c(d = 5)), 0)
expect_equal(c(a=1, b=4, f = 5) %i% c(d = 5), c(f = 5))

expect_equal(c(a=1, b=4) %x% c(d = 5), c(a=1, b=4, d = 5))
expect_equal(c(a=1, b=4, 5) %x% c(d = 5), c(a=1, b=4))


expect_equal(1:4 %a% 5:6   , 1:6)
expect_equal(1:4 %a% 4:5   , c(1,2,3,4,4,5))
expect_equal(1:4 %u% 4:5   , c(1,2,3,4,5))
expect_equal(1:6 %d% 5:6   , 1:4)
expect_equal(1:6 %d% gt(4) , 1:4)
expect_equal(1:4 %i% 4:5   , 4)
expect_equal(letters %i% perl("[a-d]") , letters[1:4])
expect_equal(letters %i% (fixed("a") | fixed("z")) , c("a", "z"))
expect_equal(1:4 %x% 4:5   , c(1, 2, 3, 5))
expect_equal(1:2 %r% 2     , c(1, 2, 1, 2))
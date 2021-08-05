context("data.table vec ops")

dt2 = data.table(
    zz = 42,
    b_3 = NA,
    aa = 10 %r% 5,
    b_ = 20 %r% 5,
    b_1 = 11 %r% 5,
    b_2 = 12 %r% 5,
    b_4 = 14 %r% 5,
    b_5 = 15 %r% 5
)

expect_identical(dt2 %n_i% "b_", dt2[,"b_"])
expect_identical(dt2 %n_d% "b_", dt2[,-"b_"])


expect_identical(dt2 %i% anyNA, dt2[,"b_3"])
expect_identical(dt2 %d% anyNA, dt2[,-"b_3"])
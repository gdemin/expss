context("recode labelled recodings")


a = 1:5
expect_error(recode(a, 1 ~ 1:2))
expect_error(recode(a, all = 1 ~ 1:5))
expect_error(recode(a, missing = 1 ~ NA))
expect_error(recode(a, missing = 1 ~ sqrt))
expect_error(recode(a, wah = other ~ copy()))
expect_error(recode(a, wah = other ~ TRUE))

res = c(1, 1, 3, 4, 5)
val_lab(res) = c(Top =1)
expect_equal( recode(a, Top = 1:2 ~ 1, other ~ copy()),
              res
              )

expect_equal( recode(a, from_to(list(1:2, other), list(Top = 1, copy))),
              res
)

res = c(1, 1, 3, 5, 5)
val_lab(res) = c(Top =1, Bottom = 5)
expect_equal( recode(a, Top = 1:2 ~ 1, Bottom = 4:5 ~ 5, other ~ copy()),
              res
)

expect_equal( recode(a, Top = 1:2 ~ 1, 3 ~ 3, Bottom = 4:5 ~ 5),
              res
)

recode(a) = c(Top = 1:2 ~ 1, 3 ~ 3, Bottom = 4:5 ~ 5)
expect_equal(a, res)

a = 1:5
recode(a) = c(Top = 1:2 ~ 1)
res = c(1, 1, 3, 4, 5)
val_lab(res) = c(Top =1)
expect_equal(a, res)

a = 1:5
recode(a) = list(Top = 1:2 ~ 1)
res = c(1, 1, 3, 4, 5)
val_lab(res) = c(Top =1)
expect_equal(a, res)

a = 1:5
val_lab(a) = c(OldTop = 1, Five = 5)
recode(a) = list(Top = 1:2 ~ 1)
res = c(1, 1, 3, 4, 5)
val_lab(res) = c(Top =1, Five = 5)
expect_equal(a, res)

context("recode with labels")
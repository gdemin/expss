context("subst")

i = 1:2
# expect_error(subst(i))
expect_identical(subst("`i`% of sum"), c("1% of sum", "2% of sum"))
expect_identical(subst("q"), "q")
expect_identical(subst("q`i`"), c("q1", "q2"))
expect_identical(subst("q`1:2`"), c("q1", "q2"))
expect_identical(subst("q`i`_`i`"), c("q1_1", "q2_2"))
expect_identical(subst("q`i`_`i`", "q`i`"), c("q1_1", "q2_2", "q1", "q2"))
expect_identical(subst("q`sum(i)`"), c("q3"))
j = 1:2
expect_identical(subst("q`i`_`j`"), c("q1_1", "q1_2", "q2_1", "q2_2"))
k = 1:2
expect_identical(subst("q`i`_`j`_`k`"),
                 c("q1_1_1", "q1_1_2", "q1_2_1", "q1_2_2", "q2_1_1", "q2_1_2", "q2_2_1", "q2_2_2"))

expect_error(subst("`ffgfg`"))


test_subst = function(x){
    i = 45
    subst(x)
}

expect_identical(test_subst("`i`"),'45')
k = 42
expect_identical(test_subst("`k`"),'42')

dfs = data.frame( zzz = 67:68)

expect_identical(with(dfs, subst("`zzz`")), as.character(67:68))
expect_identical(with(dfs, subst("`k`")), '42')

default_dataset(dfs)

expect_identical(.calc(subst("`zzz`")), as.character(67:68))
expect_identical(.calc(subst("`k`")), '42')

default_dataset(NULL)

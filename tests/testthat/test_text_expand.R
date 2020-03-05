context("text_expand")

i = 1:2
# expect_error(text_expand(i))
expect_identical(text_expand("{i}% of sum"), c("1% of sum", "2% of sum"))
expect_identical(text_expand("q"), "q")
expect_identical(text_expand("q{i}"), c("q1", "q2"))
expect_identical(text_expand("q{1:2}"), c("q1", "q2"))

expect_identical(text_expand("q{i}_{i}"), c("q1_1", "q2_2"))
expect_identical(text_expand("q`i`_`i`", delim = c("`", "`")), c("q1_1", "q2_2"))
expect_identical(text_expand("q/i}_/i}", delim = c("/", "}")), c("q1_1", "q2_2"))
expect_identical(text_expand("q{i}_{i}", "q{i}"), c("q1_1", "q2_2", "q1", "q2"))
expect_identical(text_expand("q{sum(i)}"), c("q3"))
j = 1:2
expect_identical(text_expand("q{i}_{j}"), c("q1_1", "q1_2", "q2_1", "q2_2"))
k = 1:2
expect_identical(text_expand("q{i}_{j}_{k}"),
                 c("q1_1_1", "q1_1_2", "q1_2_1", "q1_2_2", "q2_1_1", "q2_1_2", "q2_2_1", "q2_2_2"))

expect_error(text_expand("{ffgfg}"))


test_text_expand = function(x){
    i = 45
    text_expand(x)
}

expect_identical(test_text_expand("{i}"),'45')
k = 42
expect_identical(test_text_expand("{k}"),'42')

dfs = data.frame( zzz = 67:68)

expect_identical(with(dfs, text_expand("{zzz}")), as.character(67:68))
expect_identical(with(dfs, text_expand("{k}")), '42')

default_dataset(dfs)

expect_identical(.calc(text_expand("{zzz}")), as.character(67:68))
expect_identical(.calc(text_expand("{k}")), '42')

default_dataset(NULL)

context("qe")

res = qe(mrset(a1 %to% a6), ..$a, ..p("aaa"))
etal = list(quote(mrset(a1 %to% a6)), 
            quote(..$a), 
            quote(..p("aaa"))
            )
expect_identical(res, etal)
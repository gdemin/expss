context("nest")

simple_res = structure(c(1, 5, 9), labels = structure(1:9,
                      .Names = c("1|5",
                                 "1|6", "1|7", "2|5", "2|6", "2|7", "3|5", "3|6", "3|7")),
class = c("labelled",
 "numeric"))
var_lab(simple_res) = ""
expect_identical(nest(1:3, 5:7), simple_res)

expect_identical(1:3 %nest% 5:7, simple_res)

expect_identical(nest("a", list("b")),
list(structure(1, labels = structure(1L, .Names = "a|b"), class = c("labelled", 
                     "numeric"), label = ""))
)
expect_identical(nest(list("b"), "a"),
structure(1, labels = structure(1L, .Names = "b|a"), class = c("labelled", 
                        "numeric"), label = "")
)


m_choice = dtfrm(a1 = c(1, NA, 1), a2 = c(2,2,NA))

df_res = structure(list(V1 = structure(c(1, NA, 3), labels = structure(1:6, .Names = c("1|5",
"1|6", "1|7", "2|5", "2|6", "2|7")), class = c("labelled", "numeric"
)), V2 = structure(c(4, 5, NA), labels = structure(1:6, .Names = c("1|5",
"1|6", "1|7", "2|5", "2|6", "2|7")), class = c("labelled", "numeric"
))), .Names = c("V1", "V2"), row.names = c(NA, -3L), class = "data.frame")
var_lab(df_res) = ""

expect_identical(nest(m_choice, 5:7), df_res)
expect_identical(nest(mrset(m_choice), 5:7), mrset(df_res))
expect_identical(nest(dummy(m_choice), 5:7), mrset(df_res))
expect_identical(nest(list(m_choice, 1:3), 5:7), list(df_res, simple_res))
expect_identical(nest(list(mrset(m_choice), 1:3), 5:7), list(mrset(df_res), simple_res))
expect_identical(nest(list(as.dichotomy(m_choice), 1:3), 5:7), list(mrset(df_res), simple_res))

df_res2 = structure(list(V1 = structure(c(1, NA, 5), labels = structure(1:6, .Names = c("5|1", 
"5|2", "6|1", "6|2", "7|1", "7|2")), class = c("labelled", "numeric"
)), V2 = structure(c(2, 4, NA), labels = structure(1:6, .Names = c("5|1", 
"5|2", "6|1", "6|2", "7|1", "7|2")), class = c("labelled", "numeric"
))), .Names = c("V1", "V2"), row.names = c(NA, -3L), class = "data.frame")
var_lab(df_res2) = ""

expect_identical(nest(5:7, m_choice), df_res2)
expect_identical(nest(5:7, mrset(m_choice)), mrset(df_res2))
expect_identical(nest(5:7, as.dichotomy(m_choice)), mrset(df_res2))


expect_identical(nest(5:7, list(m_choice, 1:3)), 
                 list(
                     nest(c(5, NA, NA), m_choice),
                     nest(c(5, NA, NA), 1:3),
                     nest(c(NA, 6, NA), m_choice),
                     nest(c(NA, 6, NA), 1:3),
                     nest(c(NA, NA, 7), m_choice),
                     nest(c(NA, NA, 7), 1:3)
                     )
)

expect_identical(nest(5:7, list(mrset(m_choice), 1:3)), 
                 list(
                     nest(c(5, NA, NA), mrset(m_choice)),
                     nest(c(5, NA, NA), 1:3),
                     nest(c(NA, 6, NA), mrset(m_choice)),
                     nest(c(NA, 6, NA), 1:3),
                     nest(c(NA, NA, 7), mrset(m_choice)),
                     nest(c(NA, NA, 7), 1:3)
                 )
)

expect_identical(nest(5:7, list(dummy(m_choice), 1:3)), 
                 list(
                     nest(c(5, NA, NA), mrset(m_choice)),
                     nest(c(5, NA, NA), 1:3),
                     nest(c(NA, 6, NA), mrset(m_choice)),
                     nest(c(NA, 6, NA), 1:3),
                     nest(c(NA, NA, 7), mrset(m_choice)),
                     nest(c(NA, NA, 7), 1:3)
                 )
)

expect_identical(nest(factor(5:7), list(m_choice, 1:3)), 
                 list(
                     nest(c(5, NA, NA), m_choice),
                     nest(c(5, NA, NA), 1:3),
                     nest(c(NA, 6, NA), m_choice),
                     nest(c(NA, 6, NA), 1:3),
                     nest(c(NA, NA, 7), m_choice),
                     nest(c(NA, NA, 7), 1:3)
                 )
)

expect_identical(nest(factor(5:7), list(mrset(m_choice), 1:3)), 
                 list(
                     nest(c(5, NA, NA), mrset(m_choice)),
                     nest(c(5, NA, NA), 1:3),
                     nest(c(NA, 6, NA), mrset(m_choice)),
                     nest(c(NA, 6, NA), 1:3),
                     nest(c(NA, NA, 7), mrset(m_choice)),
                     nest(c(NA, NA, 7), 1:3)
                 )
)

expect_identical(nest(factor(5:7), list(as.dichotomy(m_choice), 1:3)), 
                 list(
                     nest(c(5, NA, NA), mrset(m_choice)),
                     nest(c(5, NA, NA), 1:3),
                     nest(c(NA, 6, NA), mrset(m_choice)),
                     nest(c(NA, 6, NA), 1:3),
                     nest(c(NA, NA, 7), mrset(m_choice)),
                     nest(c(NA, NA, 7), 1:3)
                 )
)

posix_ct = as.POSIXct(c("2017-01-01", "2017-01-02", "2017-01-03"))

expect_identical(
nest(factor(5:7), posix_ct),
structure(c(1, 5, 9), labels = structure(1:9, .Names = c("5|2017-01-01", 
"5|2017-01-02", "5|2017-01-03", "6|2017-01-01", "6|2017-01-02", 
"6|2017-01-03", "7|2017-01-01", "7|2017-01-02", "7|2017-01-03"
)), label = "", class = c("labelled", "numeric"))
)


expect_identical(nest(factor(5:7), list(m_choice, posix_ct)), 
                 list(
                     nest(c(5, NA, NA), m_choice),
                     nest(c(5, NA, NA), posix_ct),
                     nest(c(NA, 6, NA), m_choice),
                     nest(c(NA, 6, NA), posix_ct),
                     nest(c(NA, NA, 7), m_choice),
                     nest(c(NA, NA, 7), posix_ct)
                 )
)

expect_identical(nest(factor(5:7), list(mrset(m_choice), posix_ct)), 
                 list(
                     nest(c(5, NA, NA), mrset(m_choice)),
                     nest(c(5, NA, NA), posix_ct),
                     nest(c(NA, 6, NA), mrset(m_choice)),
                     nest(c(NA, 6, NA), posix_ct),
                     nest(c(NA, NA, 7), mrset(m_choice)),
                     nest(c(NA, NA, 7), posix_ct)
                 )
)

expect_identical(nest(posix_ct, list(m_choice, 1:3)), 
                 list(
                     nest(c("2017-01-01", NA, NA), m_choice),
                     nest(c("2017-01-01", NA, NA), 1:3),
                     nest(c(NA, "2017-01-02", NA), m_choice),
                     nest(c(NA, "2017-01-02", NA), 1:3),
                     nest(c(NA, NA, "2017-01-03"), m_choice),
                     nest(c(NA, NA, "2017-01-03"), 1:3)
                 )
)

expect_identical(nest(posix_ct, list(mrset(m_choice), 1:3)), 
                 list(
                     nest(c("2017-01-01", NA, NA), mrset(m_choice)),
                     nest(c("2017-01-01", NA, NA), 1:3),
                     nest(c(NA, "2017-01-02", NA), mrset(m_choice)),
                     nest(c(NA, "2017-01-02", NA), 1:3),
                     nest(c(NA, NA, "2017-01-03"), mrset(m_choice)),
                     nest(c(NA, NA, "2017-01-03"), 1:3)
                 )
)


a = 1:3
var_lab(a) = "var a"
val_lab(a) = c(one = 1, three = 3)
b = 5:7
var_lab(b) = "var b"
val_lab(b) = c(five = 5, seven = 7)

aa = paste0(var_lab(a), "|",  c("one", 2,  "three"))
bb = paste0(var_lab(b), "|", c("five", 6, "seven"))

aabb = c(t(outer(aa, bb, paste, sep = "|")))


expect_identical(nest(a, b), 
                 set_val_lab(nest(1:3, 5:7), setNames(1:9, aabb))
                 )

expect_identical(nest(1, a, b), 
                 nest(nest(1, a), b)
)

expect_identical(nest(a, b, 1), 
                 nest(nest(a, b), 1)
)

expect_identical(nest(1, b), 
                 set_val_lab(nest(1, 5:7), setNames(1:3, paste0(1, "|", bb)))
)

expect_identical(nest(b, 1), 
                 set_val_lab(nest(5:7, 1), setNames(1:3, paste0(bb, "|", 1)))
)

expect_error(nest(b, 1:2))
expect_error(nest(1:2, b))

# m_choice = dtfrm(a1 = c(1, NA, 1), a2 = c(2,2,NA))
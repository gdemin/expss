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
structure(1, labels = structure(1L, .Names = "a|b"), class = c("labelled", 
                     "numeric"), label = "")
)
expect_identical(nest(list("b"), "a"),
structure(1, labels = structure(1L, .Names = "b|a"), class = c("labelled", 
                        "numeric"), label = "")
)


m_choice = sheet(a1 = c(1, NA, 1), a2 = c(2,2,NA))

df_res = list(a1 = structure(c(1, NA, 3), labels = structure(1:3, .Names = c("1|5", 
"1|6", "1|7")), class = c("labelled", "numeric"), label = ""), 
a2 = structure(c(1, 2, NA), labels = structure(1:3, .Names = c("2|5", 
"2|6", "2|7")), class = c("labelled", "numeric"), label = ""))
var_lab(df_res) = ""

expect_identical(nest(m_choice, 5:7), df_res)
expect_identical(nest(as.list(m_choice), 5:7), df_res)
expect_identical(nest(as.list(m_choice), list(5:7)), df_res)
df_res2 = structure(list(V1 = structure(c(1, NA, 3), labels = structure(1:6, .Names = c("1|5", 
"1|6", "1|7", "2|5", "2|6", "2|7")), class = c("labelled", "numeric"
), label = ""), V2 = structure(c(4, 5, NA), labels = structure(1:6, .Names = c("1|5", 
"1|6", "1|7", "2|5", "2|6", "2|7")), class = c("labelled", "numeric"
), label = "")), row.names = c(NA, -3L), class = c("category", 
              "data.frame"))
expect_identical(nest(mrset(m_choice), 5:7), df_res2)
expect_identical(nest(dummy(m_choice), 5:7), df_res2)
expect_identical(nest(as.dichotomy(m_choice), 5:7), df_res2)
expect_identical(nest(list(m_choice, 1:3), 5:7), c(df_res, list(simple_res)))
expect_identical(nest(list(mrset(m_choice), 1:3), 5:7), list(df_res2, simple_res))
expect_identical(nest(list(as.dichotomy(m_choice), 1:3), 5:7), list(df_res2, simple_res))

df_res3 = structure(list(V1 = structure(c(1, NA, 5), labels = structure(1:6, .Names = c("5|1", 
"5|2", "6|1", "6|2", "7|1", "7|2")), class = c("labelled", "numeric"
), label = ""), V2 = structure(c(2, 4, NA), labels = structure(1:6, .Names = c("5|1", 
"5|2", "6|1", "6|2", "7|1", "7|2")), class = c("labelled", "numeric"
), label = "")), row.names = c(NA, -3L), class = c("category", 
  "data.frame"))

expect_identical(nest(5:7, mrset(m_choice)), df_res3)

expect_identical(unname(nest(5:7, m_choice)), list(
    nest(c(5, NA, NA), m_choice[[1]]),
    nest(c(5, NA, NA), m_choice[[2]]),
    nest(c(NA, 6, NA), m_choice[[1]]),
    nest(c(NA, 6, NA), m_choice[[2]]),
    nest(c(NA, NA, 7), m_choice[[1]]),
    nest(c(NA, NA, 7), m_choice[[2]])
))



expect_identical(nest(5:7, as.dichotomy(m_choice)), mrset(df_res3))


expect_identical(unname(nest(5:7, list(m_choice, 1:3))), 
                 list(
                   nest(c(5, NA, NA), m_choice[[1]]),
                   nest(c(5, NA, NA), m_choice[[2]]),
                   nest(c(5, NA, NA), 1:3),
                   nest(c(NA, 6, NA), m_choice[[1]]),
                   nest(c(NA, 6, NA), m_choice[[2]]),
                   nest(c(NA, 6, NA), 1:3),
                   nest(c(NA, NA, 7), m_choice[[1]]),
                   nest(c(NA, NA, 7), m_choice[[2]]),
                   nest(c(NA, NA, 7), 1:3)
                 )
)

expect_identical(unname(nest(as.dichotomy(5:7), list(m_choice, 1:3))), 
                 list(
                   nest(c(5, NA, NA), m_choice[[1]]),
                   nest(c(5, NA, NA), m_choice[[2]]),
                   nest(c(5, NA, NA), 1:3),
                   nest(c(NA, 6, NA), m_choice[[1]]),
                   nest(c(NA, 6, NA), m_choice[[2]]),
                   nest(c(NA, 6, NA), 1:3),
                   nest(c(NA, NA, 7), m_choice[[1]]),
                   nest(c(NA, NA, 7), m_choice[[2]]),
                   nest(c(NA, NA, 7), 1:3)
                 )
)

expect_identical(unname(nest(5:7, list(mrset(m_choice), 1:3))), 
                 list(
                     nest(c(5, NA, NA), mrset(m_choice)),
                     nest(c(5, NA, NA), 1:3),
                     nest(c(NA, 6, NA), mrset(m_choice)),
                     nest(c(NA, 6, NA), 1:3),
                     nest(c(NA, NA, 7), mrset(m_choice)),
                     nest(c(NA, NA, 7), 1:3)

                 )
)

expect_identical(unname(nest(5:7, list(dummy(m_choice), 1:3))), 
                 list(
                   nest(c(5, NA, NA), mrset(m_choice)),
                   nest(c(5, NA, NA), 1:3),
                   nest(c(NA, 6, NA), mrset(m_choice)),
                   nest(c(NA, 6, NA), 1:3),
                   nest(c(NA, NA, 7), mrset(m_choice)),
                   nest(c(NA, NA, 7), 1:3)
                   
                 )
)

expect_identical(unname(nest(factor(5:7), list(m_choice, 1:3))), 
                 list(
                   nest(c(5, NA, NA), m_choice[[1]]),
                   nest(c(5, NA, NA), m_choice[[2]]),
                   nest(c(5, NA, NA), 1:3),
                   nest(c(NA, 6, NA), m_choice[[1]]),
                   nest(c(NA, 6, NA), m_choice[[2]]),
                   nest(c(NA, 6, NA), 1:3),
                   nest(c(NA, NA, 7), m_choice[[1]]),
                   nest(c(NA, NA, 7), m_choice[[2]]),
                   nest(c(NA, NA, 7), 1:3)
                 )
)

expect_identical(unname(nest(factor(5:7), list(mrset(m_choice), 1:3))), 
                 list(
                   nest(c(5, NA, NA), mrset(m_choice)),
                   nest(c(5, NA, NA), 1:3),
                   nest(c(NA, 6, NA), mrset(m_choice)),
                   nest(c(NA, 6, NA), 1:3),
                   nest(c(NA, NA, 7), mrset(m_choice)),
                   nest(c(NA, NA, 7), 1:3)
                   
                 )
)

expect_identical(unname(nest(factor(5:7), list(as.dichotomy(m_choice), 1:3))), 
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


expect_identical(unname(nest(factor(5:7), list(m_choice, posix_ct))), 
                 list(
                   nest(c(5, NA, NA), m_choice[[1]]),
                   nest(c(5, NA, NA), m_choice[[2]]),
                   nest(c(5, NA, NA), posix_ct),
                   nest(c(NA, 6, NA), m_choice[[1]]),
                   nest(c(NA, 6, NA), m_choice[[2]]),
                   nest(c(NA, 6, NA), posix_ct),
                   nest(c(NA, NA, 7), m_choice[[1]]),
                   nest(c(NA, NA, 7), m_choice[[2]]),
                   nest(c(NA, NA, 7), posix_ct)
                 )
)

expect_identical(unname(nest(factor(5:7), list(mrset(m_choice), posix_ct))), 
                 list(
                   nest(c(5, NA, NA), mrset(m_choice)),
                   nest(c(5, NA, NA), posix_ct),
                   nest(c(NA, 6, NA), mrset(m_choice)),
                   nest(c(NA, 6, NA), posix_ct),
                   nest(c(NA, NA, 7), mrset(m_choice)),
                   nest(c(NA, NA, 7), posix_ct)
                 )
)

expect_identical(nest(posix_ct, 1:3), 
structure(c(1, 5, 9), labels = structure(1:9, .Names = c("2017-01-01|1", 
"2017-01-01|2", "2017-01-01|3", "2017-01-02|1", "2017-01-02|2", 
"2017-01-02|3", "2017-01-03|1", "2017-01-03|2", "2017-01-03|3"
)), class = c("labelled", "numeric"), label = "")
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

expect_identical(nest(b), b)

expect_error(nest(b, 1:2))
expect_error(nest(1:2, b))

a = rep(NA, 5)
val_lab(a) = c(a = 1, b = 2)
b = rep(NA, 5)
val_lab(b) = c(d = 1, e = 2)
expect_identical(
nest(a, b),
structure(c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), 
labels = structure(1:4, .Names = c("a|d", 
                                 "a|e", "b|d", "b|e")), class = c("labelled", "numeric"), label = "")
)
# m_choice = sheet(a1 = c(1, NA, 1), a2 = c(2,2,NA))

single = 5:7
var_lab(single) = "Top level"
val_lab(single) = num_lab(
    "
        5 Five    
        6 Six
        7 Seven
    "
)
my_scales = sheet(
    a = 1:3,
    b = 2:4
)

var_lab(my_scales[[1]]) = "Scale 1" 
var_lab(my_scales[[2]]) = "Scale 2"
val_lab(my_scales[[1]]) = num_lab("
                        1 Sc1 1
                        2 Sc1 2
                        3 Sc1 3
                        4 Sc1 4
                                  ")

val_lab(my_scales[[2]]) = num_lab("
                        1 Sc2 1
                        2 Sc2 2
                        3 Sc2 3
                        4 Sc2 4
                                  ")

res = list(v5.a = structure(c(1, NA, NA), labels = structure(1:4, .Names = c("Top level|Five|Scale 1|Sc1 1", 
"Top level|Five|Scale 1|Sc1 2", "Top level|Five|Scale 1|Sc1 3", 
"Top level|Five|Scale 1|Sc1 4")), class = c("labelled", "numeric"
), label = ""), v5.b = structure(c(2, NA, NA), labels = structure(1:4, .Names = c("Top level|Five|Scale 2|Sc2 1", 
"Top level|Five|Scale 2|Sc2 2", "Top level|Five|Scale 2|Sc2 3", 
"Top level|Five|Scale 2|Sc2 4")), class = c("labelled", "numeric"
), label = ""), v6.a = structure(c(NA, 2, NA), labels = structure(1:4, .Names = c("Top level|Six|Scale 1|Sc1 1", 
"Top level|Six|Scale 1|Sc1 2", "Top level|Six|Scale 1|Sc1 3", 
"Top level|Six|Scale 1|Sc1 4")), class = c("labelled", "numeric"
), label = ""), v6.b = structure(c(NA, 3, NA), labels = structure(1:4, .Names = c("Top level|Six|Scale 2|Sc2 1", 
"Top level|Six|Scale 2|Sc2 2", "Top level|Six|Scale 2|Sc2 3", 
"Top level|Six|Scale 2|Sc2 4")), class = c("labelled", "numeric"
), label = ""), v7.a = structure(c(NA, NA, 3), labels = structure(1:4, .Names = c("Top level|Seven|Scale 1|Sc1 1", 
"Top level|Seven|Scale 1|Sc1 2", "Top level|Seven|Scale 1|Sc1 3", 
"Top level|Seven|Scale 1|Sc1 4")), class = c("labelled", "numeric"
), label = ""), v7.b = structure(c(NA, NA, 4), labels = structure(1:4, .Names = c("Top level|Seven|Scale 2|Sc2 1", 
"Top level|Seven|Scale 2|Sc2 2", "Top level|Seven|Scale 2|Sc2 3", 
"Top level|Seven|Scale 2|Sc2 4")), class = c("labelled", "numeric"
), label = ""))
expect_identical(nest(single, my_scales), res)

####### bug 

current = as.category(as.dichotomy(set_val_lab(rep(1:9, 2), setNames(1:9, letters[1:9]))))
rolling = as.category(as.dichotomy(set_val_lab(rep(1:3, 6), setNames(1:4, LETTERS[1:4]))))
nested0 = current %nest% rolling

rolling2 = rolling[,rev(seq_along(rolling))]
nested = current %nest% rolling2
expect_identical(nested0, nested)

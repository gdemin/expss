context("recode labelled recodings")


a = 1:5
expect_error(recode(a, 1 ~ 1:2))
expect_error(recode(a, all = 1 ~ 1:5))
expect_error(recode(a, missing = 1 ~ NA))
expect_error(recode(a, missing = 1 ~ sqrt))
expect_error(recode(a, wah = other ~ copy()))
expect_equal(recode(a, TRUE ~ 1, with_labels = TRUE), rep(1, 5))

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

a = c(1:7, 99)

var_lab(a) = "Liking"
labs  = num_lab("
                     1 Disgusting
                     2 Very Poor
                     3 Poor
                     4 So-so
                     5 Good
                     6 Very good
                     7 Excellent
                     99 Hard to say
                     ")

val_lab(a) = labs

res = rec(a, 99 ~ NA, other ~ copy, with_labels = FALSE)
expect_equal(res, c(1:7, NA))

res = rec(a, 99 ~ NA, other ~ sqrt)
expect_equal(res, structure(c(1, 1.4142135623731, 1.73205080756888, 2, 2.23606797749979, 
2.44948974278318, 2.64575131106459, NA), labels = c(Disgusting = 1, 
`Very Poor` = 1.4142135623731, Poor = 1.73205080756888, `So-so` = 2, 
Good = 2.23606797749979, `Very good` = 2.44948974278318, Excellent = 2.64575131106459
), class = c("labelled", "numeric"), label = "Liking"))

res = recode(a, 99 ~ NA, with_labels = TRUE)
expect_identical(res, 
structure(c(NA, NA, NA, NA, NA, NA, NA, NA), label = "Liking", class = c("labelled", 
"logical")))

b = a
recode(b, with_labels = TRUE) = 99 ~ NA
expect_identical(b, structure(c(1, 2, 3, 4, 5, 6, 7, NA), label = "Liking", labels = c(Disgusting = 1, 
`Very Poor` = 2, Poor = 3, `So-so` = 4, Good = 5, `Very good` = 6, 
Excellent = 7), class = c("labelled", "numeric"))
)


b = a
recode(b, with_labels = FALSE) = 99 ~ NA
expect_identical(b, structure(c(1, 2, 3, 4, 5, 6, 7, NA), label = "Liking", labels = c(Disgusting = 1, 
`Very Poor` = 2, Poor = 3, `So-so` = 4, Good = 5, `Very good` = 6, 
Excellent = 7, `Hard to say` = 99), class = c("labelled", "numeric"
)))


b = a
rec(b) = 99 ~ NA
expect_identical(b, structure(c(1, 2, 3, 4, 5, 6, 7, NA), label = "Liking", labels = c(Disgusting = 1, 
`Very Poor` = 2, Poor = 3, `So-so` = 4, Good = 5, `Very good` = 6, 
Excellent = 7), class = c("labelled", "numeric"))
)

res = recode(a, 1:3 ~ 1, 4 ~ 4, 5:7~ 7, 99 ~ NA, with_labels = TRUE)
expect_identical(res, 
structure(c(1, 1, 1, 4, 7, 7, 7, NA), labels = c(`Disgusting/Very Poor/Poor` = 1, 
`So-so` = 4, `Good/Very good/Excellent` = 7), class = c("labelled", 
"numeric"), label = "Liking"))


res = rec(a, 1:3 ~ 1, 4 ~ 4, 5:7~ 7, 99 ~ NA)
expect_identical(res, 
structure(c(1, 1, 1, 4, 7, 7, 7, NA), labels = c(`Disgusting/Very Poor/Poor` = 1, 
`So-so` = 4, `Good/Very good/Excellent` = 7), class = c("labelled", 
"numeric"), label = "Liking"))


res = recode(a, 1:3 ~ 1, 4 ~ 4, Top = 5:7 ~ 7, 99 ~ NA, with_labels = TRUE)
expect_identical(res, 
structure(c(1, 1, 1, 4, 7, 7, 7, NA), labels = c(`Disgusting/Very Poor/Poor` = 1, 
`So-so` = 4, Top = 7), class = c("labelled", "numeric"), label = "Liking"))

res = recode(a, 1:3 ~ 1, 4 ~ 4, 5:7 ~ 7, 99 ~ NA, with_labels = TRUE, new_label = function(x) {
    toupper(paste(x, collapse = ", "))
})

expect_identical(res, structure(c(1, 1, 1, 4, 7, 7, 7, NA), labels = c(`DISGUSTING, VERY POOR, POOR` = 1, 
`So-so` = 4, `GOOD, VERY GOOD, EXCELLENT` = 7), class = c("labelled", 
"numeric"), label = "Liking"))

res = recode(a, 1:3 ~ 1, 4 ~ 4, 5:7~ 7, 99 ~ NA, with_labels = TRUE, new_label = "range")
expect_identical(res, 
structure(c(1, 1, 1, 4, 7, 7, 7, NA), labels = c(`Disgusting - Poor` = 1, 
`So-so` = 4, `Good - Excellent` = 7), class = c("labelled", "numeric"
), label = "Liking"))

res = rec(a, 1:3 ~ 1, 4 ~ 4, 5:7~ 7, 99 ~ NA, new_label = "range")
expect_identical(res, 
structure(c(1, 1, 1, 4, 7, 7, 7, NA), labels = c(`Disgusting - Poor` = 1, 
`So-so` = 4, `Good - Excellent` = 7), class = c("labelled", "numeric"
), label = "Liking"))

res = recode(a, 1:3 ~ 1, 4 ~ 4, 5:7~ 7, 99 ~ NA, with_labels = TRUE, new_label = "first")
expect_identical(res,
structure(c(1, 1, 1, 4, 7, 7, 7, NA), labels = c(Disgusting = 1, 
`So-so` = 4, Good = 7), class = c("labelled", "numeric"), label = "Liking")
)

res = recode(a, 1:3 ~ 1, 4 ~ 4, 5:7~ 7, 99 ~ NA, with_labels = TRUE, new_label = "last")
expect_identical(res,
structure(c(1, 1, 1, 4, 7, 7, 7, NA), labels = c(Poor = 1, `So-so` = 4, 
Excellent = 7), class = c("labelled", "numeric"), label = "Liking")
)

b = a
recode(b, with_labels = TRUE, new_label = "range") = c(1:3 ~ 1, 5:7~ 7, 99 ~ NA)
expect_identical(b, 
structure(c(1, 1, 1, 4, 7, 7, 7, NA), label = "Liking", labels = c(`Disgusting - Poor` = 1, 
`So-so` = 4, `Good - Excellent` = 7), class = c("labelled", "numeric"
))
)

b = a
rec(b, new_label = "range") = c(1:3 ~ 1, 5:7~ 7, 99 ~ NA)
expect_identical(b, 
structure(c(1, 1, 1, 4, 7, 7, 7, NA), label = "Liking", labels = c(`Disgusting - Poor` = 1, 
`So-so` = 4, `Good - Excellent` = 7), class = c("labelled", "numeric"
))
)

expect_warning(recode(as.matrix(a), 99 ~ NA, TRUE ~ copy, with_labels = TRUE))
b = as.matrix(a)
expect_warning({
    recode(b, with_labels = TRUE) =c(99 ~ NA, TRUE ~ copy)
    })

### bug with lost labels (issue #73)

set.seed(123)
df1 = data.frame("Q1" = sample(c(1,7), 50, replace = T),
                  "Q2" = sample(c(1,4,5), 50, replace = T), 
                  "Q3" = sample(c(4,5), 50, replace = T), 
                  "Q4" = sample(c(1,4,5,6), 50, replace = T),
                  "Q5" = sample(c(1,4,6), 50, replace = T), 
                  "Q6" = sample(c(4,5,7,NA), 50, replace = T),
                  "Q7" = sample(c(5,NA), 50, replace = T), 
                  "Q8" = sample(c(1,4,5,6,7), 50, replace = T)
                  )

df2 = recode(df1[1:length(df1)], 
              "Always" = 1 ~ 5, "Often" = 4 ~ 4, "Sometimes" = 5 ~ 3, "Rarely" = 6 ~ 2, "Never" = 7 ~ 1, TRUE ~ copy)

res = lapply(df2, val_lab) %>% 
    lapply(identical, 
           rev(c("Always" = 5, "Often" = 4, "Sometimes" = 3, "Rarely" = 2, "Never" = 1))) %>% 
    unlist() %>% 
    all()


expect_true(res)


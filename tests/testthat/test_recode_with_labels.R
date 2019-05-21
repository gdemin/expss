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

res = recode(a, 1:3 ~ 1, 4 ~ 4, 5:7~ 7, 99 ~ NA, with_labels = TRUE)
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

expect_warning(recode(as.matrix(a), 99 ~ NA, TRUE ~ copy, with_labels = TRUE))
b = as.matrix(a)
expect_warning({
    recode(b, with_labels = TRUE) =c(99 ~ NA, TRUE ~ copy)
    })

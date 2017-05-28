context("fctr.labelled")

no_lab = rep(1:2,3)
vec_with_lab = no_lab
var_lab(vec_with_lab) = "Fruits"
val_lab(vec_with_lab) = c(Apple=1,Bananas=2)

expect_identical(fctr(unlab(vec_with_lab)),factor(no_lab))
expect_identical(fctr(unvr(vec_with_lab)),factor(no_lab,levels = 1:2,labels= c("Apple","Bananas")))
expect_identical(fctr(unvr(vec_with_lab)),fctr(vec_with_lab, prepend_var_lab = FALSE))

a = letters[1:3]
var_lab(a) = "letters"
expect_identical(fctr(a),
                 factor(unlab(a),levels = c("a", "b", "c"),labels= c("letters|a", "letters|b", "letters|c")))

a = letters[1:3]
var_lab(a) = "letters"
expect_identical(fctr(unvr(a)),
                 factor(unlab(a)))


expect_identical(fctr(vec_with_lab),
                 factor(no_lab,levels = 1:2,labels= c("Fruits|Apple","Fruits|Bananas")))

expect_identical(fctr(vec_with_lab, ordered = TRUE),
                 factor(no_lab,levels = 1:2,labels= c("Fruits|Apple","Fruits|Bananas"), ordered = TRUE))

expect_identical(fctr(no_lab,levels = 1:2,labels= c("Fruits|Apple","Fruits|Bananas"), ordered = TRUE),
                 factor(no_lab,levels = 1:2,labels= c("Fruits|Apple","Fruits|Bananas"), ordered = TRUE))

expect_identical(fctr(unvl(vec_with_lab)),factor(no_lab,levels = 1:2,labels= c("Fruits|1","Fruits|2")))

vec_with_lab2 = add_val_lab(vec_with_lab, c("Ananas" = 42))
expect_identical(fctr(vec_with_lab2),
                 factor(no_lab,levels = c(1:2, 42),
                        labels= c("Fruits|Apple","Fruits|Bananas","Fruits|Ananas")))

expect_identical(fctr(vec_with_lab2, drop_unused_labels = TRUE),
                 factor(no_lab,levels = c(1:2),
                        labels= c("Fruits|Apple","Fruits|Bananas")))

a = factor(c("a", "b", "c"), levels = rev(c("a", "b", "c", "d", "e")))

expect_identical(fctr(a, drop_unused_labels = TRUE), factor(a))
expect_identical(fctr(a, drop_unused_labels = FALSE), a)

var_lab(a) = "My 'a' with labels"

expect_identical(fctr(a, drop_unused_labels = FALSE, prepend_var_lab = FALSE), unvr(a))
b = a
levels(b) = paste0(var_lab(b),"|", levels(b))
expect_identical(fctr(a, drop_unused_labels = FALSE, prepend_var_lab = TRUE), unvr(b))
expect_identical(fctr(a, drop_unused_labels = TRUE, prepend_var_lab = FALSE), factor(a))

### Common usage ###

test_ds = data.frame(total = 1, s2b = sample(2:3,100,replace = TRUE))
test_ds = unlab(test_ds)
val_lab(test_ds$s2b) = c('18 - 26' = 2, '27 - 35' = 3)

# head(factor(test_ds$s2b))

expect_identical(levels(fctr(test_ds$s2b)), names(val_lab(test_ds$s2b)))


context("fctr.labelled - some values without labels")
no_lab = c(no_lab,5:6)
vec_with_lab = c(vec_with_lab,5:6)

expect_identical(fctr(unlab(vec_with_lab)),
                 factor(no_lab))
expect_identical(fctr(unvr(vec_with_lab)),
                 factor(no_lab,levels = c(1:2,5:6),labels= c("Apple","Bananas","5","6")))
expect_identical(fctr(vec_with_lab),
                 factor(no_lab,levels =  c(1:2,5:6),labels= c("Fruits|Apple","Fruits|Bananas","Fruits|5","Fruits|6")))
expect_identical(fctr(unvl(vec_with_lab)),
                factor(no_lab,levels =  c(1:2,5:6),labels= c("Fruits|1","Fruits|2","Fruits|5","Fruits|6")))


context( "fctr - errors and warnings")

a = 1
class(a) = "labelled"
attr(a, "labels") = c(a = 1, b = 1)
expect_error(fctr(a))
attr(a, "labels") = c(a = 1, a = 2)
expect_warning(fctr(a))
expect_identical(suppressWarnings(fctr(a)), factor(1, levels = 1:2, labels = c("a","a_1")))
attr(a, "labels") = c(a = 1, a = 2, a = 3)
expect_identical(suppressWarnings(fctr(a)), factor(1, levels = 1:3, labels = c("a","a_1","a_2")))


a = 1:3
val_lab(a) = c("1" = 3)
expect_warning(fctr(a))
suppressWarnings(expect_identical(fctr(a), factor(c("1", "2", "1_1"), levels = c("1", "2", "1_1"))))


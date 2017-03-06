context("to_fac.labelled")

no_lab = rep(1:2,3)
vec_with_lab = no_lab
var_lab(vec_with_lab) = "Fruits"
val_lab(vec_with_lab) = c(Apple=1,Bananas=2)

expect_identical(to_fac(unlab(vec_with_lab)),factor(no_lab))
expect_identical(to_fac(unvr(vec_with_lab)),factor(no_lab,levels = 1:2,labels= c("Apple","Bananas")))
expect_identical(to_fac(unvr(vec_with_lab)),to_fac(vec_with_lab, prepend_var_lab = FALSE))

a = letters[1:3]
var_lab(a) = "letters"
expect_identical(to_fac(a),
                 factor(unlab(a),levels = c("a", "b", "c"),labels= c("letters|a", "letters|b", "letters|c")))

a = letters[1:3]
var_lab(a) = "letters"
expect_identical(to_fac(unvr(a)),
                 factor(unlab(a)))


expect_identical(to_fac(vec_with_lab),
                 factor(no_lab,levels = 1:2,labels= c("Fruits|Apple","Fruits|Bananas")))

expect_identical(to_fac(vec_with_lab, ordered = TRUE),
                 factor(no_lab,levels = 1:2,labels= c("Fruits|Apple","Fruits|Bananas"), ordered = TRUE))

expect_identical(to_fac(no_lab,levels = 1:2,labels= c("Fruits|Apple","Fruits|Bananas"), ordered = TRUE),
                 factor(no_lab,levels = 1:2,labels= c("Fruits|Apple","Fruits|Bananas"), ordered = TRUE))

expect_identical(to_fac(unvl(vec_with_lab)),factor(no_lab,levels = 1:2,labels= c("Fruits|1","Fruits|2")))

vec_with_lab2 = add_val_lab(vec_with_lab, c("Ananas" = 42))
expect_identical(to_fac(vec_with_lab2),
                 factor(no_lab,levels = c(1:2, 42),
                        labels= c("Fruits|Apple","Fruits|Bananas","Fruits|Ananas")))

expect_identical(to_fac(vec_with_lab2, drop_unused = TRUE),
                 factor(no_lab,levels = c(1:2),
                        labels= c("Fruits|Apple","Fruits|Bananas")))

a = factor(c("a", "b", "c"), levels = rev(c("a", "b", "c", "d", "e")))

expect_identical(to_fac(a, drop_unused = TRUE), factor(a))
expect_identical(to_fac(a, drop_unused = FALSE), a)

var_lab(a) = "My 'a' with labels"

expect_identical(to_fac(a, drop_unused = FALSE, prepend_var_lab = FALSE), unvr(a))
b = a
levels(b) = paste0(var_lab(b),"|", levels(b))
expect_identical(to_fac(a, drop_unused = FALSE, prepend_var_lab = TRUE), unvr(b))
expect_identical(to_fac(a, drop_unused = TRUE, prepend_var_lab = FALSE), factor(a))

### Common usage ###

test_ds = data.frame(total = 1, s2b = sample(2:3,100,replace = TRUE))
test_ds = unlab(test_ds)
val_lab(test_ds$s2b) = c('18 - 26' = 2, '27 - 35' = 3)

# head(factor(test_ds$s2b))

expect_identical(levels(to_fac(test_ds$s2b)), names(val_lab(test_ds$s2b)))


context("to_fac.labelled - some values without labels")
no_lab = c(no_lab,5:6)
vec_with_lab = c(vec_with_lab,5:6)

expect_identical(to_fac(unlab(vec_with_lab)),
                 factor(no_lab))
expect_identical(to_fac(unvr(vec_with_lab)),
                 factor(no_lab,levels = c(1:2,5:6),labels= c("Apple","Bananas","5","6")))
expect_identical(to_fac(vec_with_lab),
                 factor(no_lab,levels =  c(1:2,5:6),labels= c("Fruits|Apple","Fruits|Bananas","Fruits|5","Fruits|6")))
expect_identical(to_fac(unvl(vec_with_lab)),
                factor(no_lab,levels =  c(1:2,5:6),labels= c("Fruits|1","Fruits|2","Fruits|5","Fruits|6")))


context( "to_fac - errors and warnings")

a = 1
class(a) = "labelled"
attr(a, "labels") = c(a = 1, b = 1)
expect_error(to_fac(a))
attr(a, "labels") = c(a = 1, a = 2)
expect_warning(to_fac(a))
expect_identical(suppressWarnings(to_fac(a)), factor(1, levels = 1:2, labels = c("a","a1")))
attr(a, "labels") = c(a = 1, a = 2, a = 3)
expect_identical(suppressWarnings(to_fac(a)), factor(1, levels = 1:3, labels = c("a","a1","a2")))

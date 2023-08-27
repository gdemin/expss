context("drop_unused_labels")

a = as.double(1:2)
expect_identical(drop_unused_labels(a), a)
val_lab(a) = autonum(
    "one
    two
        three
    "
)

b = a
val_lab(b) = autonum(
    "one
    two
    "
)

expect_identical(drop_unused_labels(a), b)

a = as.double(c(1:2, NA))
var_lab(a) = "my_a"
val_lab(a) = autonum(
    "one
    two
    three
    "
)

d = 1:3
val_lab(d) = autonum(
    "one
    two
    three
    "
)

b = a
val_lab(b) = autonum(
    "one
    two
    "
)

expect_identical(drop_unused_labels(sheet(a, d)), sheet(a = b, d))
expect_identical(drop_unused_labels(list(a, d)), list(b, d))

expect_identical(drop_unused_labels(mrset(a, d)), mrset(a, d))

new_mrset = mrset(a, d)
add_val_lab(new_mrset) = c(hs = 99L)             
expect_identical(drop_unused_labels(new_mrset), mrset(a, d))

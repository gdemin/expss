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
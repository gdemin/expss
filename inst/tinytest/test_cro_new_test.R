cat("\nCONTEXT:", "cro new tests", "\n")
a = set_val_lab(1:5, c(a = 1, b = 2, d = 3))
var_lab(a) = "My a"
expect_identical(cro(mrset(a)), cro(a))


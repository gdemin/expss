context("mrset")

data("product_test")

product_test = apply_labels(product_test,
                            a1_1 = "Likes. A",
                            a1_1 = num_lab("
                                     1 Liked everything
                                     2 Disliked everything
                                     3 Chocolate
                                     4 Appearance
                                     5 Taste
                                     6 Stuffing
                                     7 Nuts
                                     8 Consistency
                                     98 Other
                                     99 Hard to answer
                                     "),
                            b1_1 = "Likes. B",
                            b1_1 = num_lab("
                                     1 Liked everything
                                     2 Disliked everything
                                     3 Chocolate
                                     4 Appearance
                                     5 Taste
                                     6 Stuffing
                                     7 Nuts
                                     8 Consistency
                                     98 Other
                                     99 Hard to answer
                                     ")
)
expect_error(mrset())

a_res = calculate(product_test, a1_1 %to% a1_6)
b_res = calculate(product_test, b1_1 %to% b1_6)



class(a_res) = union("category", class(a_res))

big = cbind(a_res, b_res)
class(big) = union("category", class(big))

expect_identical(calculate(product_test, mrset(a1_1 %to% a1_6)), a_res)
expect_identical(calculate(product_test, mrset_f(a1_)), a_res)
expect_identical(calculate(product_test, mrset_p("a1_")), a_res)
expect_identical(calculate(product_test, mrset_t("a1_{1:6}")), a_res)
expect_identical(calculate(product_test, mrset(as.matrix(a1_1 %to% a1_6))), unlab(a_res))
expect_identical(calculate(product_test, mrset(a1_1 %to% a1_6, b1_1 %to% b1_6)), big)
expect_identical(calculate(product_test, mrset_f(a1_, b1_)), big)
expect_identical(calculate(product_test, mrset_p("^a1_\\d$", "^b1_\\d$")), big)
expect_identical(calculate(product_test, mrset_t("a1_{1:6}", "b1_{1:6}")), big)
big$b1_1 = unlab(big$b1_1)
expect_identical(calculate(product_test, mrset(a1_1 %to% a1_6, as.matrix(b1_1 %to% b1_6))), big)
expect_identical(calculate(product_test, mrset(a1_1)), setNames(a_res[,1, drop = FALSE], "a1_1"))
var_lab(a_res$a1_1) = "New label"
expect_identical(calculate(product_test, mrset(a1_1 %to% a1_6, label = "New label")), a_res)
expect_identical(calculate(product_test, mrset_f(a1_, label = "New label")), a_res)
expect_identical(calculate(product_test, mrset_p("a1_", label = "New label")), a_res)
expect_identical(calculate(product_test, mrset_t("a1_{1:6}", label = "New label")), a_res)
#########################
context("mdset")

a_res = calculate(product_test, a1_1 %to% a1_6)
b_res = calculate(product_test, b1_1 %to% b1_6)

class(a_res) = union("category", class(a_res))

big = cbind(as.dichotomy(a_res), as.dichotomy(b_res))
class(big) = union("dichotomy", class(big))
a_res_dich = as.dichotomy(a_res)
strange = a_res_dich
if_val(a_res_dich) = 1:150>100 ~ NA
if_val(strange) = 1:150>100 ~ 0
expect_identical(mdset(strange), strange)
expect_identical(calc(strange, mdset_f(v)), strange)
expect_identical(calc(strange, mdset_p("v")), strange)
expect_identical(calc(strange, mdset_t("v{2:8}")), strange)
a_res_dich = as.dichotomy(a_res)
strange = a_res_dich
if_val(strange) = c(0 ~ NA, 1 ~ 42)
expect_identical(mdset(strange),  a_res_dich)

expect_identical(mdset(as.dichotomy(a_res), as.dichotomy(b_res)), big)

a = as.double(1:0)
var_lab(a) = "My a"
res = sheet(a = a)
class(res) = c("dichotomy", class(res))
expect_identical(mdset(a), res)

a_res_dich = as.dichotomy(a_res)
for(each in seq_along(a_res_dich)){
    var_lab(a_res_dich[[each]]) = paste0("New label|", names(a_res_dich)[each]) 
}

# expect_identical(mdset(dummy(a_res), label = "New label"),  a_res_dich)

a_res = a_res_dich
for(each in seq_along(a_res_dich)){
    var_lab(a_res_dich[[each]]) = paste0("New label|", var_lab(a_res_dich[[each]])) 
    
}

expect_identical(mdset(a_res, label = "New label"),  a_res_dich)
expect_identical(calc(a_res, mdset_f(v, label = "New label")),  a_res_dich)
expect_identical(calc(a_res, mdset_p("v", label = "New label")),  a_res_dich)
expect_identical(calc(a_res, mdset_t("v{2:8}", label = "New label")),  a_res_dich)

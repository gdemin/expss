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
expect_identical(calculate(product_test, mrset(as.matrix(a1_1 %to% a1_6))), unlab(a_res))
expect_identical(calculate(product_test, mrset(a1_1 %to% a1_6, b1_1 %to% b1_6)), big)
big$b1_1 = unlab(big$b1_1)
expect_identical(calculate(product_test, mrset(a1_1 %to% a1_6, as.matrix(b1_1 %to% b1_6))), big)
expect_identical(calculate(product_test, mrset(a1_1)), setNames(a_res[,1, drop = FALSE], "x"))
var_lab(a_res$a1_1) = "New label"
expect_identical(calculate(product_test, mrset(a1_1 %to% a1_6, label = "New label")), a_res)
#########################
context("mdset")

a_res = calculate(product_test, a1_1 %to% a1_6)
b_res = calculate(product_test, b1_1 %to% b1_6)

class(a_res) = union("category", class(a_res))

big = cbind(dichotomy_df(a_res), dichotomy_df(b_res))
class(big) = union("dichotomy", class(big))
a_res_dich = dichotomy_df(a_res)
strange = a_res_dich
if_val(a_res_dich) = 1:150>100 ~ NA
if_val(strange) = 1:150>100 ~ 0
expect_identical(mdset(strange), a_res_dich)
a_res_dich = dichotomy_df(a_res)
strange = a_res_dich
if_val(strange) = c(0 ~ NA, 1 ~ 42)
expect_identical(mdset(strange),  a_res_dich)

expect_identical(mdset(dichotomy_df(a_res), dichotomy_df(b_res)), big)
expect_identical(mdset(dichotomy(a_res), dichotomy(b_res)), big)
expect_identical(mdset(dichotomy(a_res)[,1]), 
                 setNames(mdset(a_res_dich[,1, drop = FALSE]), "x"))

a_res_dich = dichotomy_df(a_res)
for(each in seq_along(a_res_dich)){
    var_lab(a_res_dich[[each]]) = paste0("New label|", names(a_res_dich)[each]) 
}

expect_identical(mdset(dichotomy(a_res), label = "New label"),  a_res_dich)

a_res = a_res_dich
for(each in seq_along(a_res_dich)){
    var_lab(a_res_dich[[each]]) = paste0("New label|", var_lab(a_res_dich[[each]])) 
    
}

expect_identical(mdset(a_res, label = "New label"),  a_res_dich)
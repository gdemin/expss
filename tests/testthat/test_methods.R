context("c")

vec_with_lab = rep(1:2,3)
var_lab(vec_with_lab) = "Fruits"
val_lab(vec_with_lab) = c(Apple=1,Bananas=2)

new_vec = c(vec_with_lab,vec_with_lab)

expect_identical(var_lab(new_vec),var_lab(vec_with_lab))
expect_identical(val_lab(new_vec),val_lab(vec_with_lab))

context("[")
expect_identical(var_lab(vec_with_lab[1]),var_lab(vec_with_lab))
expect_identical(val_lab(vec_with_lab[1]),val_lab(vec_with_lab))


context("data.frame[")
dfs = data.frame(a = vec_with_lab,b= vec_with_lab,stringsAsFactors = FALSE)

expect_identical(var_lab(dfs[1,1]),var_lab(vec_with_lab))
expect_identical(var_lab(dfs[1,'a']),var_lab(vec_with_lab))
expect_identical(var_lab(dfs[,'a']),var_lab(vec_with_lab))

expect_identical(val_lab(dfs[1,1]),val_lab(vec_with_lab))
expect_identical(val_lab(dfs[1,'a']),val_lab(vec_with_lab))
expect_identical(val_lab(dfs[,'a']),val_lab(vec_with_lab))

context("rep")

new_vec = rep(vec_with_lab,2)

expect_identical(var_lab(new_vec),var_lab(vec_with_lab))
expect_identical(val_lab(new_vec),val_lab(vec_with_lab))


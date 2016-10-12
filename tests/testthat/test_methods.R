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


context("as.data.frame")

a = 1:3

class(a) = "labelled"

expect_identical(as.data.frame(a), as.data.frame.vector(a))

expect_identical(as.data.frame(a, nm="xx"), as.data.frame.vector(a, nm="xx"))

b = 4:5
var_lab(b) = "lab"

expect_identical(as.data.frame(b), as.data.frame.vector(b))

expect_identical(as.data.frame(b, nm="xx"), as.data.frame.vector(b, nm="xx"))


# a = matrix(1:9, 3)
# var_lab(a) = "sdfsf"
# 
# as.data.frame(a)

context("utils")

expect_identical(expss:::check_conformance.list(list(1,2), 42), TRUE)

aaa = matrix(1:9, 3)
bbb = aaa
bbb[,1] = 1

'tested<-' = expss:::`column<-.matrix`
tested(aaa, 1) = 1
expect_identical(aaa, bbb)


'tested<-' = expss:::`column<-.list`
aaa = list(1,2)
expect_error({tested(aaa, 1) = 1})

'tested<-' = expss:::`column<-.factor`
fff = factor(c("a", "b", "c"))
f2 = fff
f2[] = "a"
tested(fff) = "a"
expect_identical(fff, f2)



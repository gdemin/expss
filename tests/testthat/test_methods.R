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

expect_identical(var_lab(dfs$a[1]),var_lab(vec_with_lab))
expect_identical(var_lab(dfs$a[[1]]),var_lab(vec_with_lab))

expect_identical(val_lab(dfs$a[1]),val_lab(vec_with_lab))
expect_identical(val_lab(dfs$a[[1]]),val_lab(vec_with_lab))

expect_identical(dfs$a[[2]],dfs$a[2])
expect_identical(dfs$a[[2]],dfs$a[2])

aa = dfs$a
aa[[1]] = "a"
dfs$a[1] = "a"

expect_identical(aa, dfs$a)

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


a = c("a", "b", "c")

var_lab(a) = "Characters"

expect_identical(as.data.frame(a), as.data.frame(a, stringsAsFactors = FALSE)) 

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

context("type conversion")
a = 1:0
a_str = as.character(a)
a_log = c(TRUE, FALSE)
var_lab(a) = "Lab"
var_lab(a_str) = "Lab"
var_lab(a_log) = "Lab"
val_lab(a) = c("Lab" = 1)
val_lab(a_str) = c("Lab" = 1)
val_lab(a_log) = c("Lab" = 1)

a_numeric = a + 0.5 - 0.5
class(a_numeric) = c("labelled", "numeric")
expect_identical(as.numeric(a_str), a_numeric)
expect_identical(as.integer(a_str), a)
expect_identical(as.character(a), a_str)
expect_identical(as.logical(a), a_log)
expect_identical(as.integer(a_log), a)


context("print.labelled")

x = c(letters, LETTERS)
x = as.labelled(x)

expect_identical(print(x), x)

expect_output_file(print(x), "rds/print_labelled1.txt")
var_lab(x) = "Letters"
expect_output_file(print(x), "rds/print_labelled2.txt")
expect_output_file(print(x, max = 100), "rds/print_labelled3.txt")
expect_output_file(print(x, max = 100, max_labels = 100), "rds/print_labelled4.txt")
expect_output_file(print(unvl(x)), "rds/print_labelled5.txt")
x_mat = matrix(x, ncol = 2)
var_lab(x_mat) = var_lab(x)
val_lab(x_mat) = val_lab(x)
expect_output_file(print(x_mat), "rds/print_labelled6.txt")

x_df = as.data.frame(x_mat)
var_lab(x_df) = var_lab(x)
val_lab(x_df) = val_lab(x)
class(x_df) = union("labelled", class(x_df))
expect_output_file(print(x_df), "rds/print_labelled2.txt")

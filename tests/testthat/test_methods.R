context("c")

vec_with_lab = rep(1:2,3)
var_lab(vec_with_lab) = "Fruits"
val_lab(vec_with_lab) = c(Apple=1,Bananas=2)

new_vec = c(vec_with_lab,vec_with_lab)

expect_identical(var_lab(new_vec),var_lab(vec_with_lab))
expect_identical(val_lab(new_vec),val_lab(vec_with_lab))
###
context("c on labelled factors")

a = factor(0:1, levels = 0:1, labels = c("Zero", "One"))
a = set_var_lab(a, "my labelled factor")
expect_identical(a, c(a))
####
b = factor(1:2, levels = 1:2, labels = c("One", "Two"))
b = set_var_lab(b, "my second labelled factor")

res = factor(c(1,2,2,3), levels = 1:3, labels = c("Zero", "One", "Two"))
res = set_var_lab(res, "my labelled factor")

expect_identical(c(a,b), res)
###
b = ordered(1:2, levels = 1:2, labels = c("One", "Two"))
b = set_var_lab(b, "my second labelled factor")

res = ordered(c(1,2,2,3), levels = 1:3, labels = c("Zero", "One", "Two"))
res = set_var_lab(res, "my labelled factor")

expect_identical(c(a,b), res)
###
b = c("One", "Two")
b = set_var_lab(b, "my second labelled factor")

res = factor(c(1,2,2,3), levels = 1:3, labels = c("Zero", "One", "Two"))
res = set_var_lab(res, "my labelled factor")

expect_identical(c(a,b), res)
###
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

a_integer = a
storage.mode(a_integer) = "integer"
class(a_integer) = c("labelled", "integer")

a_numeric = a + 0.5 - 0.5
class(a_numeric) = c("labelled", "numeric")
expect_identical(as.numeric(a_str), a_numeric)
expect_identical(as.integer(a_str), a_integer)

expect_identical(as.logical(a), unvl(a_log))
expect_identical(as.integer(a_log), a_integer)

options(expss.enable_value_labels_support = 0)
expect_identical(as.character(a), unvl(a_str))
options(expss.enable_value_labels_support = NULL)
expect_identical(as.character(a), c("Lab", "0"))
expect_identical(as.character(a, prepend_varlab = TRUE), c("Lab|Lab", "Lab|0"))


aa = 1:3
val_lab(aa) = c(c = 1, b = 2, a = 3)

expect_identical(factor(aa), factor(1:3, levels = 1:3, labels = c("c", "b", "a")))


# a = c(1, 2, 0)
# val_lab(a) = c(a= 1)
# attr(a, "labels") = c(a = 1, a=2 , b = 0)
# 
# expect_warning(as.character(a))
# suppressWarnings(expect_identical(as.character(a), c("a", "a_2", "b")))
# 
# attr(a, "labels") = c(a = 1, c=1 , b = 0)
# 
# expect_warning(as.character(a))
# suppressWarnings(expect_identical(as.character(a), c("a", "2", "b")))

context("unique.labelled")
a = c(1, 1, 0, NA)
var_lab(a) = "This is a"
val_lab(a) = c("a" = 1, b = 0, d = 2)

expect_identical(unique(a), a[-1])

options(expss.enable_value_labels_support = 0)
expect_identical(unique(a), c(1, 0, NA))

options(expss.enable_value_labels_support = 1)
expect_identical(unique(a), a[-1])
expss_enable_value_labels_support_extreme()
expect_identical(unique(a), c(a[-1], 2))
expss_enable_value_labels_support()
expect_identical(unique(a), a[-1])


context("print.labelled/str.labelled")

x = c(letters, LETTERS)
x = as.labelled(x)

expect_identical(print(x), x)

# x_df = as.data.frame(x_mat)
# var_lab(x_df) = var_lab(x)
# val_lab(x_df) = val_lab(x)
# class(x_df) = union("labelled", class(x_df))
# expect_output_file(print(x_df), "rds/print_labelled2.txt")



# my_vec = 1:3
# val_lab(my_vec) = autonum("Один
#                           Два
#                           Три")
# 
# var_lab(my_vec) = "Цифры"
# 
# all_dat = total(label = "Всего|Я")
# 
# cyrillic = cro(my_vec, all_dat)
# 
# options(expss.output = "")
# cyrillic


context("as.etable")

data(mtcars)
res = mtcars
expect_false(is.etable(res))
class(res) = union("etable", class(res))
expect_true(is.etable(res))

expect_identical(as.etable(mtcars, rownames_as_row_labels = FALSE), res)


res = sheet(row_labels = rownames(mtcars), mtcars)
class(res) = union("etable", class(res))
expect_identical(as.etable(mtcars, rownames_as_row_labels = TRUE), res)
expect_identical(as.etable(mtcars), res)
expect_identical(as.etable(1:3), as.etable(as.sheet(x = 1:3)))

res = as.sheet(matrix(1:9, 3))
colnames(res) = rep("", 3)
class(res) = union("etable", class(res))
expect_identical(as.etable(matrix(1:9, 3)), res)

mtcars = unlab(mtcars)

expect_equal_to_reference(
    with(mtcars, as.etable(table(am))),
    "rds/as.etable1.rds",  update = FALSE)
expect_equal_to_reference(
    with(mtcars, as.etable(table(am, vs))),
    "rds/as.etable2.rds",  update = FALSE)
expect_equal_to_reference(
    with(mtcars, as.etable(table(am, vs, cyl))),
    "rds/as.etable3.rds",  update = FALSE)
expect_equal_to_reference(
    with(mtcars, as.etable(table(am, vs, cyl, gear))),
    "rds/as.etable4.rds",  update = FALSE)

mtcars = apply_labels(mtcars, 
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      gear = "Number of forward gears",
                      carb = "Carbureuter",
                      hp = "Gross horsepower",
                      vs = "Engine",
                      vs = num_lab(" 
                                   0 V-engine
                                   1 Straight engine
                                   "),
                      
                      am = "Transmission",
                      am = num_lab(" 
                                   0 Automatic
                                   1 Manual
                                   ")
)

expect_identical(as.etable(cro(mtcars$am, mtcars$vs)), cro(mtcars$am, mtcars$vs))

expect_equal_to_reference(
    use_labels(mtcars, as.etable(table(am))),
    "rds/as.etable5.rds",  update = FALSE)
expect_equal_to_reference(
    use_labels(mtcars, as.etable(table(am, vs))),
    "rds/as.etable6.rds",  update = FALSE)
expect_equal_to_reference(
    use_labels(mtcars, as.etable(table(am, vs, cyl))),
    "rds/as.etable7.rds",  update = FALSE)
expect_equal_to_reference(
    use_labels(mtcars, as.etable(table(am, vs, cyl, gear))),
    "rds/as.etable8.rds",  update = FALSE)

context("method as.character.labelled and others")

a = 1:0
a_str = as.character(a)
a_log = c(TRUE, FALSE)
var_lab(a) = "Lab"
var_lab(a_str) = "Lab"
var_lab(a_log) = "Lab"
val_lab(a) = c("Lab" = 1)
val_lab(a_str) = c("Lab" = 1)
val_lab(a_log) = c("Lab" = 1)

a_integer = a
storage.mode(a_integer) = "integer"
class(a_integer) = c("labelled", "integer")

a_numeric = a + 0.5 - 0.5
class(a_numeric) = c("labelled", "numeric")
expect_identical(as.numeric(a_str), a_numeric)
expect_identical(as.integer(a_str), a_integer)

expect_identical(as.logical(a), unvl(a_log))
expect_identical(as.integer(a_log), a_integer)

# options(expss.enable_value_labels_support = 0)
expss_disable_value_labels_support()
expect_identical(as.character(a), unvl(a_str))
expss_enable_value_labels_support()
expect_identical(as.character(a), c("Lab", "0"))
expect_identical(as.character(a, prepend_varlab = TRUE), c("Lab|Lab", "Lab|0"))


aa = 1:3
val_lab(aa) = c(c = 1, b = 2, a = 3)

expect_identical(factor(aa), factor(1:3, levels = 1:3, labels = c("c", "b", "a")))

a = c(1, 1, 0, NA)
var_lab(a) = "This is a"
val_lab(a) = c("a" = 1, b = 0)

expect_identical(unique(a), a[-1])

# options(expss.enable_value_labels_support = 0)
expss_disable_value_labels_support()
expect_identical(unique(a), c(1, 0, NA))

expss_enable_value_labels_support()
expect_identical(unique(a), a[-1])
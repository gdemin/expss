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



# context("rbind/cbind etable")

# data(mtcars)
# mtcars = modify(mtcars,{
#     var_lab(mpg) = "Miles/(US) gallon"
#     var_lab(cyl) = "Number of cylinders"
#     var_lab(disp) = "Displacement (cu.in.)"
#     var_lab(hp) = "Gross horsepower"
#     var_lab(drat) = "Rear axle ratio"
#     var_lab(wt) = "Weight (lb/1000)"
#     var_lab(qsec) = "1/4 mile time"
#     
#     var_lab(vs) = "V/S"
#     val_lab(vs) = c("Straight" = 0, "V" = 1)
#     
#     var_lab(am) = "Transmission (0 = automatic, 1 = manual)"
#     val_lab(am) = c(" automatic" = 0, " manual" =  1)
#     
#     var_lab(gear) = "Number of forward gears"
#     var_lab(carb) = "Number of carburetors"
# })
# 
# tbl_df = table_summary_df(mtcars %>% except(vs, am), col_vars = mtcars$am, fun = function(x){
#     
#     sheet(res_num = seq_along(x), parameter = names(x), mean = colMeans(x))
# },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
# hide = "res_num",
# use_result_row_order = FALSE
# )
# 
# expect_equal_to_reference(cbind(tbl_df, tbl_df), "rds/cbind1.rds",  update = FALSE)
# expect_equal_to_reference(cbind(tbl_df, new_col = 42), "rds/cbind2.rds",  update = FALSE)
# expect_equal_to_reference(cbind(tbl_df, new_col = "new_col"), "rds/cbind3.rds",  update = FALSE)
# # expect_equal_to_reference(cbind(tbl_df, data.frame(new_col = "new_col")), "rds/cbind4.rds",  update = FALSE)
# 
# expect_equal_to_reference(rbind(tbl_df, tbl_df), "rds/cbind5.rds",  update = FALSE)
# expect_equal_to_reference(rbind(tbl_df, 42), "rds/cbind6.rds",  update = FALSE)
# expect_equal_to_reference(rbind(42, tbl_df), "rds/cbind7.rds",  update = FALSE)
# df = tbl_df
# class(df) = class(df) %d% c("etable",  "table_summary_df") 
# # expect_equal_to_reference(rbind(tbl_df, df), "rds/cbind8.rds",  update = FALSE)
# 
# simple_df = cro_mean(mtcars %>% except(vs, am), mtcars$am)
# 
# expect_equal_to_reference(cbind(simple_df, simple_df), "rds/cbind9.rds",  update = FALSE)
# expect_equal_to_reference(cbind(simple_df, new_col = 42), "rds/cbind10.rds",  update = FALSE)
# expect_equal_to_reference(cbind(simple_df, new_col = "new_col"), "rds/cbind11.rds",  update = FALSE)
# expect_equal_to_reference(cbind(tbl_df, data.frame(new_col = "new_col")), "rds/cbind12.rds",  update = FALSE)

# expect_equal_to_reference(rbind(simple_df, simple_df), "rds/cbind13.rds",  update = FALSE)
# expect_equal_to_reference(rbind(simple_df, 42), "rds/cbind14.rds",  update = FALSE)
# expect_equal_to_reference(rbind(42, simple_df), "rds/cbind15.rds",  update = FALSE)
# df = simple_df
# class(df) = class(df) %d% c("etable",  "table_summary_df") 
# expect_equal_to_reference(rbind(simple_df, df), "rds/cbind16.rds",  update = FALSE)


# expect_equal_to_reference(cbind(tbl_df, simple_df), "rds/cbind17.rds",  update = FALSE)
# expect_equal_to_reference(cbind(simple_df, tbl_df), "rds/cbind18.rds",  update = FALSE)

# expect_equal_to_reference(rbind(tbl_df, simple_df), "rds/cbind17.rds",  update = FALSE)
# expect_equal_to_reference(rbind(simple_df, tbl_df), "rds/cbind18.rds",  update = FALSE)


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
    calc(mtcars, as.etable(table(am))),
    "rds/as.etable1.rds",  update = FALSE)
expect_equal_to_reference(
    calc(mtcars, as.etable(table(am, vs))),
    "rds/as.etable2.rds",  update = FALSE)
expect_equal_to_reference(
    calc(mtcars, as.etable(table(am, vs, cyl))),
    "rds/as.etable3.rds",  update = FALSE)
expect_equal_to_reference(
    calc(mtcars, as.etable(table(am, vs, cyl, gear))),
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
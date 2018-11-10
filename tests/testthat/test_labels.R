context("var_lab")

test_ds = data.frame(total = 1, s2b = sample(2:3,100,replace = TRUE))
age_group = test_ds$s2b
var_lab(test_ds$s2b) = "Age group"
age_group = set_var_lab(age_group,"Age group")
expect_identical(age_group,test_ds$s2b) # should be TRUE

expect_identical(var_lab(age_group),attr(age_group,"label")) # should be TRUE


test_ds = unvr(test_ds)
test_test_ds = test_ds

for (each in seq_along(test_test_ds)) var_lab(test_test_ds[[each]]) = "Age group"

expect_identical(set_var_lab(test_ds, "Age group"), test_test_ds)


test_ds = as.list(unvr(test_ds))
test_test_ds = test_ds

for (each in seq_along(test_test_ds)) var_lab(test_test_ds[[each]]) = "Age group"

expect_identical(set_var_lab(test_ds, "Age group"), test_test_ds)
expect_identical(test_ds, unvr(test_test_ds))
expect_identical(test_ds, drop_var_labs(test_test_ds))

context("val_lab")

a = 1
expect_error({val_lab(a) = c(a = 1, b = 1)})
val_lab(a) = c(a = 1, a = 2)
b = set_val_lab(1, c(a = 1, a_1 = 2))
expect_identical(a, b)



## data.frame

dd = data.frame(a=1:3,b=3:1,d=3)

val_lab(dd$a) = c(a=1)
val_lab(dd$b) = c(b=2)
val_lab(dd$d) = c(d=3)

expect_identical(c(a=1,b=2,d=3),val_lab(dd))

dd = data.frame(a=1:3,b=3:1,d=3)

val_lab(dd$a) = c(a=1)
val_lab(dd$b) = c(b=2)
val_lab(dd$d) = c(d=1)

expect_identical(c(a=1,b=2),val_lab(dd))

### Add labels ###

expect_identical(drop_all_labels(test_ds), unlab(test_ds))
expect_identical(drop_val_labs(test_ds), unvl(test_ds))
test_ds = unlab(test_ds) # drop all labels

age_groups = c('18 - 26' = 2, '27 - 35' = 3)
add_val_lab(test_ds$s2b) = age_groups[1]
add_val_lab(test_ds$s2b) = age_groups[2]

expect_identical(test_ds$s2b, set_val_lab(unlab(test_ds$s2b),age_groups))

test_ds$s2b = unlab(test_ds$s2b)

## make labels from text copied from questionnaire

val_lab(test_ds$s2b) = make_labels("
 2. 18 - 26
 3. 27 - 35
")

expect_equal(test_ds$s2b, set_val_lab(test_ds$s2b,age_groups))

context("make_labels")

labs1 = c('18 - 26' = 2, '27 - 35' = 3)

labs2 = c(Brand1 =1 , Brand2 =2 ,Brand3=3, Brand4=4)
labs2_1 = c("Brand 1" =1 , "Brand 2" =2 ,"Brand 3"=3, "Brand 4"=4)

labs3 = c(Bad = -1 , Normal= 0, Good=1)
labs4 = c('Very bad' = -1 ,'Bad' = -0.5 ,Normal= 0,Good = 0.5 ,'Very good'=1)
    
expect_identical(labs1,make_labels("
 2. 18 - 26
 3. 27 - 35
"))

expect_identical(labs2,make_labels("
    1\tBrand1

\t\t2.    Brand2    

3.\t\t    Brand3\t\t


4    Brand4

"))

expect_identical(labs2,num_lab("
    1\tBrand1

\t\t2.    Brand2    

3.\t\t    Brand3\t\t


4    Brand4

"))



expect_identical(labs2,lab_num("
    Brand1\t      1

\t\t    Brand2   2 

\t\t    Brand3\t\t3


Brand4              4

"))



expect_identical(labs2_1,make_labels("
    1\tBrand 1

\t\t2.    Brand 2    

3.\t\t    Brand 3\t\t


4    Brand 4

"))

expect_identical(labs2_1,make_labels("
    Brand 1\t      1

\t\t    Brand 2   2 

\t\t    Brand 3\t\t3


Brand 4              4

",code_position="right"))


expect_identical(labs3,make_labels(
    "Bad  -1 
    \t\tNormal\t\t 0 
    Good    1
    
    
    
    ",code_position="right"))


expect_identical(labs3,make_labels(
    "   -1. \t   Bad 
    0\t\tNormal\t\t  
    1 Good
    
    
    
    ",code_position="left"))


expect_identical(labs4,make_labels("
    Very bad -1
    Bad \t -0.5 
    Normal    0 
    \t\tGood 0.5 
    Very good  1
    
    
    ",code_position="right"))



expect_identical(labs4,make_labels("
      \t-1 Very bad
       \t-0.5\tBad \t  
    0 Normal
     0.5\t\tGood 
    1 Very good
    
    
    ",code_position="left"))


expect_identical(labs4,make_labels(c("
                            \t-1 Very bad",
                            "\t-0.5\tBad \t",  
                            "0 Normal
                            0.5\t\tGood", 
                            "1 Very good
                            
                            
                            ","","      \t\t"),code_position="left"))


expect_identical(make_labels("
    1
    2
    3
    4
    5
    6
    7
    8
    9
    10
"), NULL)

expect_identical(make_labels("
    1  hi
                             2
                             3
                             4
                             5
                             6
                             7
                             8
                             9
                             10 lo
                             "), c(hi=1, lo = 10))

expect_error(make_labels("
    1
                         2
                         3
                         wdwddde
                         5
                         6
                         7
                         8
                         9
                         10
                         "))

context("add_val_lab")
vec = 1:5
dfs = data.frame(a = vec, b = vec)

dfs1 = set_val_lab(dfs, c(a=1, b=2, c=3, d=4, e=5))

add_val_lab(dfs) = c(a=1)
add_val_lab(dfs) = c(b=2)
add_val_lab(dfs) = c(c=3)
add_val_lab(dfs) = c(d=4)
add_val_lab(dfs) = c(e=5)

expect_identical(dfs, dfs1)


context("labels NULL")
a = 1:3
b = a
val_lab(b) = c(a=1)
# expect_identical(set_val_lab(b, NULL), as.double(a))
# a = as.double(a)
var_lab(b) = "bbb"
expect_identical(set_val_lab(b, NULL), set_var_lab(a, "bbb"))
expect_identical(set_var_lab(b, NULL), set_val_lab(a, c(a=1)))
expect_identical(set_val_lab(set_var_lab(b, NULL), NULL), a)
val_lab(b) = NULL
expect_identical(b, set_var_lab(a, "bbb"))
var_lab(b) = NULL
expect_identical(b, a)

context("make_labels autonum")

expect_identical(
 make_labels(
"
male


female
             ", code_position = "autonum"),
c(male = 1L, female = 2L)
)

expect_identical(
    make_labels(
        "male
        female      ", code_position = "autonum"),
    c(male = 1L, female = 2L)
    )

expect_identical(
    make_labels(
        "

        female      ", code_position = "autonum"),
    c(female = 1L)
    )

expect_identical(
    make_labels(
        "
        
              ", code_position = "autonum"),
    NULL
    )

expect_identical(
    autonum(
        "
        male
        
        
        female
        "),
    c(male = 1L, female = 2L)
    )



context("as.labelled")



character_vector = c("one", "two",  "two", "three")
res = c(1L, 3L, 3L, 2L)

expect_error(set_val_lab(res, character_vector))

val_lab(res) = c("one" = 1L, "three" = 2L, "two" = 3L)
var_lab(res) = "Numbers"
expect_identical(
    as.labelled(character_vector, label = "Numbers"),
    res
)

data(iris)
species = rep(1:3, each = 50) * 1.0
val_lab(species) = c("setosa" = 1L, "versicolor" = 2L, "virginica" = 3L) 

expect_identical(as.labelled(iris$Species), species)


dat = as.POSIXct(c("2016-09-25", "2016-09-26"))

res = 1:2
val_lab(res) = setNames(1:2, as.character(dat))
expect_identical(as.labelled(dat), res)

a = 1:2
val_lab(a) = c("a"=1, "b" = 2)
expect_identical(as.labelled(a), a)
var_lab(a) = "ssdds"
expect_identical(as.labelled(a), a)
expect_identical(as.labelled(a, "new"), set_var_lab(a, "new"))
a = 1:2
var_lab(a) = "ssdds"
expect_equal(as.labelled(a), set_val_lab(a, c("1" = 1L, "2" = 2L)))

context("as.labelled labelled factor")
a = factor(c("a", "b", "c"), levels = rev(c("a", "b", "c", "d", "e")))
b = as.double(5:3)
val_lab(b) = setNames(5:1, letters[1:5])
expect_identical(as.labelled(a), b)

var_lab(a) = "My 'a' with labels"
var_lab(b) = "My 'a' with labels"
expect_identical(as.labelled(a), b)
var_lab(b) = "New label"
expect_identical(as.labelled(a, "New label"), b)

context("as.labelled labelled character")

a = letters
a_res = set_val_lab(1:26, setNames(1:26, letters))
expect_identical(as.labelled(a), a_res)

var_lab(a) = "Letters"
var_lab(a_res) = "Letters"
expect_identical(as.labelled(a), a_res)



context("is.labelled")

a = 1:5
expect_identical(is.labelled(a), FALSE)
var_lab(a) = "aaa"
expect_identical(is.labelled(a), TRUE)
a = unlab(a)
expect_identical(is.labelled(a), FALSE)
val_lab(a) = c(a = 1)
expect_identical(is.labelled(a), TRUE)
class(a) = union("new_class", class(a))
expect_identical(is.labelled(a), TRUE)



aaa = matrix(1:9, 3)

expect_error(as.labelled(as.list(aaa), NULL))
expect_error(as.labelled(as.data.frame(aaa), NULL))

#### with strange lists ####
context("labels on lists")

data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (lb/1000)",
                      qsec = "1/4 mile time",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)

model = use_labels(mtcars, lm(mpg ~ vs + am + hp + wt))

expect_equal_to_reference(unlab(model), "rds/unlab_list.rds",  update = FALSE)
expect_equal_to_reference(unvr(model), "rds/unvr_list.rds",  update = FALSE)
expect_equal_to_reference(unvl(model), "rds/unvl_list.rds",  update = FALSE)


context("value labels on factor")

gender = c(1, 1, 1, 0, 0, 0, 1, 0, 0, 0,
                     1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1,
                     0, 0, 1, 0, 1, 1, 1, 1, 1, 0)
group = c(rep(1, 10), rep(2, 11), rep(3, 10))
sample.df = data.frame(group = factor(group), gender = factor(gender))

sample.df = apply_labels(sample.df,
                         gender = "Gender",
                         group = "Group")
expect_warning({
val_lab(sample.df$group) = num_lab("1 A
                               2 B
                               3 C")
val_lab(sample.df$gender) = num_lab("0 Women
                                1 Men")
})

res = cro_cases(sample.df$group, sample.df$gender)


var_lab(gender) = "Gender"
var_lab(group) = "Group"

val_lab(group) = num_lab("1 A
                                   2 B
                                   3 C")
val_lab(gender) = num_lab("0 Women
                                    1 Men")

test = cro_cases(group, gender)

expect_identical(res, test)


context("add_labelled_class")

a = 1:3
attr(a, "label") = "My label"

aa = 1:3
var_lab(aa) = "My label"
expect_identical(add_labelled_class(a), aa)

df = sheet(a, b = 5:7, d = set_val_lab(5:7, c("hs" = 99)))
fixed_df = sheet(a = aa, b = 5:7, d = set_val_lab(5:7, c("hs" = 99)))
expect_identical(add_labelled_class(df), fixed_df)


df = as.list(df)
fixed_df = as.list(fixed_df)
expect_identical(add_labelled_class(df), fixed_df)

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

context("val_lab")

a = 1
expect_error({val_lab(a) = c(a = 1, b = 1)})
expect_warning({val_lab(a) = c(a = 1, a = 2, b = 3, b = 4)})


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

expect_identical(labs2,ml_left("
    1\tBrand1

\t\t2.    Brand2    

3.\t\t    Brand3\t\t


4    Brand4

"))

expect_identical(labs2,ml_right("
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


expect_error(make_labels("
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




context("if_val simple vector")

expect_identical(if_val(1:5, 1~-1), c(-1, 2, 3, 4, 5))
expect_identical(if_val(1:5, 1~-1, 2 ~ NA), c(-1, NA, 3, 4, 5))
expect_identical(if_val(1:5, gt(2)~99), c(1, 2, 99, 99, 99))
expect_identical(if_val(1:5, gt(2)~99, . ~ 0), c(0, 0, 99, 99, 99))
expect_identical(if_val(1:5, 1:3 ~ 1, . ~ NA), c(1, 1, 1, NA, NA))
expect_identical(if_val(1:5, 1:3 ~ 1, 2:5 ~ 2), c(1, 1, 1, 2, 2))
expect_identical(if_val(1:5, lt(2) ~ 10, lt(3) ~ 11, lt(4) ~ 12, . ~ NA), c(10, 11, 12, NA, NA))

expect_identical(if_val(1:5, 4 ~ "four", (function(x) (x-1)<2) ~-1), c(-1, -1, 3, "four", 5) )

x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)

expect_identical(if_val(x, gt(2)~y), c(1, 8, 1, 9, NA))
expect_identical(if_val(x, list(gt(2)~y)), c(1, 8, 1, 9, NA))
expect_identical(if_val(x, gt(2)~y, lte(2) ~ z), c(4, 8, 4, 9, NA))
expect_identical(if_val(x, gt(2)~y, lte(2) ~ z, .~99), c(4, 8, 4, 9, 99))
expect_identical(if_val(x, list(gt(2)~y, lte(2) ~ z, .~99)), c(4, 8, 4, 9, 99))

expect_identical(if_val(x, (z>4)~y), c(1, 3, 1, 9, 9))

context("if_val dplyr")
if(FALSE & suppressWarnings(require(dplyr, quietly = TRUE))){
    
    
    
    x = c(1,3,1,3,NA)
    y = c(8,8,8,9,9)
    z = c(4,4,4,5,5)
    
    dfs = data.frame(
        x = c(2,4,2,4,NA),
        y = c(18,18,18,19,19),
        z = c(14,14,14,15,15)
        
    )  %>% tbl_df()
    
    dfs  = dfs %>% mutate(
        w = if_val(x, gt(2) ~ y)
        #w = ifelse(x>2, y , x)
    )
    
    expect_identical(dfs$w, c(2, 18, 2, 19, NA))
    
    dfs$x = NULL
    dfs$w = NULL
    dfs  = dfs %>% mutate(
        w = if_val(x, gt(2)~y)
    )
    expect_identical(dfs$w, c(1, 18, 1, 19, NA))
} else {
    cat("dplyr not found\n")
}

context("modify")
x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)

dfs = data.frame(
    x = c(2,4,2,4,NA),
    y = c(18,18,18,19,19),
    z = c(14,14,14,15,15)
    
) 

dfs  =  modify(dfs, {
    w = if_val(x, gt(2) ~ y)
})

expect_identical(dfs$w, c(2, 18, 2, 19, NA))

dfs$x = NULL
dfs$w = NULL
dfs  =  modify(dfs, { 
    w = if_val(x, gt(2)~y)
})
expect_identical(dfs$w, c(1, 18, 1, 19, NA))

dfs$x = NULL
dfs$y = NULL
dfs$w = NULL
dfs  = modify(dfs, { 
    w = if_val(x, gt(2)~y)
})
expect_identical(dfs$w, c(1, 8, 1, 9, NA))
##########################

context("if_val 'from, to' notation simple vector")

expect_identical(if_val(1:5, from = 1, to = -1), c(-1, 2, 3, 4, 5))
expect_identical(if_val(1:5, from = 1:2, to =c(-1, NA)), c(-1, NA, 3, 4, 5))
expect_identical(if_val(1:5, from = list(gt(2)), to=99), c(1, 2, 99, 99, 99))
expect_identical(if_val(1:5, from = c(gt(2),"."), to = c(99,0)), c(0, 0, 99, 99, 99))
expect_identical(if_val(1:5, from = list(1:3,"."), to = c(1, NA)), c(1, 1, 1, NA, NA))
expect_equal(if_val(1:5, from = list(1:3, 2:5),  to =1:2), c(1, 1, 1, 2, 2))
expect_identical(if_val(1:5, from = list(lt(2), lt(3), lt(4),"."), to =c(10,  11, 12, NA)), c(10, 11, 12, NA, NA))

expect_identical(if_val(1:5, from = list(4,function(x) (x-1)<2),  to = c("four", -1)), c(-1, -1, 3, "four", 5) )

x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)

expect_identical(if_val(x, from = list(gt(2)), to =list(y)), c(1, 8, 1, 9, NA))
expect_identical(if_val(x, from = list(gt(2)), to = list(y)), c(1, 8, 1, 9, NA))
expect_identical(if_val(x, from = list(gt(2), lte(2)), to = list(y,z)), c(4, 8, 4, 9, NA))
expect_identical(if_val(x, from = list(gt(2), lte(2),"."), to = list(y,z,99)), c(4, 8, 4, 9, 99))

expect_identical(if_val(x, from = list(z>4), to = list(y)), c(1, 3, 1, 9, 9))

context("if_val 'from, to' notation dplyr")

if(suppressWarnings(require(dplyr, quietly = TRUE))){
    
    x = c(1,3,1,3,NA)
    y = c(8,8,8,9,9)
    z = c(4,4,4,5,5)
    
    dfs = data.frame(
        x = c(2,4,2,4,NA),
        y = c(18,18,18,19,19),
        z = c(14,14,14,15,15)
        
    )
    
    dfs  = dfs %>% mutate(
        w = if_val(x, from = list(gt(2)), to = list(y))
    )
    
    expect_identical(dfs$w, c(2, 18, 2, 19, NA))
    
    dfs$x = NULL
    dfs$w = NULL
    dfs  = dfs %>% mutate(
        w = if_val(x, from = list(gt(2)), to = list(y))
    )
    expect_identical(dfs$w, c(1, 18, 1, 19, NA))
} else {
    cat("dplyr not found\n")
}

context("if_val examples")
set.seed(123)
v1  = sample(c(0:3,9,10), 20, replace = TRUE)
# RECODE V1 TO V3 (0=1) (1=0) (2,3=-1) (9=9) (ELSE=SYSMIS)
v_test = v1
v_test[] = NA
v_test[v1 == 0] = 1
v_test[v1 == 1] = 0
v_test[v1 %in% 2:3] = -1
v_test[v1 == 9 ] = 9
if_val(v1) = c(0 ~ 1, 1 ~ 0, 2:3 ~ -1, 9 ~ 9, . ~ NA)
expect_identical(
    v1
    , v_test)

set.seed(123)
qvar = sample((-5):20, 50, replace = TRUE)
# RECODE QVAR(1 THRU 5=1)(6 THRU 10=2)(11 THRU HI=3)(ELSE=0).
qvar_test = qvar
qvar_test[] = 0
qvar_test[qvar %in% 1:5] = 1
qvar_test[qvar %in% 6:10] = 2
qvar_test[qvar >= 11] = 3
expect_identical(
    if_val(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, gte(11) ~ 3, . ~ 0),
    qvar_test
)
if_val(qvar) = c(1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, 11 %thru% Inf ~ 3, . ~ 0)
expect_identical(
    qvar,
    qvar_test
)


strngvar = LETTERS
# RECODE STRNGVAR ('A','B','C'='A')('D','E','F'='B')(ELSE=' ').
letters_test = strngvar
letters_test[] = " "
letters_test[LETTERS %in% c('A','B','C')] = 'A'
letters_test[LETTERS %in% c('D','E','F')] = 'B'
expect_identical(
    if_val(strngvar, c('A','B','C') ~ 'A', c('D','E','F') ~ 'B', . ~ ' '),
    letters_test
)

set.seed(123)
age = sample(c(sample(5:30, 40, replace = TRUE), rep(9, 10)))
# RECODE AGE (MISSING=9) (18 THRU HI=1) (0 THRU 18=0) INTO VOTER.
voter_test = age
voter_test[age == 9 ] = NA
voter_test[voter_test %in% 0:17 ] = 0
voter_test[voter_test %in% 18:100 ] = 1
expect_identical(
    if_val(age, 9 ~ NA, 18 %thru% Inf ~ 1, 0 %thru% 18 ~ 0),
    voter_test
)

context("if_val examples from/to")
set.seed(123)
v1  = sample(c(0:3,9,10), 20, replace = TRUE)
# RECODE V1 TO V3 (0=1) (1=0) (2,3=-1) (9=9) (ELSE=SYSMIS)
v_test = v1
v_test[] = NA
v_test[v1 == 0] = 1
v_test[v1 == 1] = 0
v_test[v1 %in% 2:3] = -1
v_test[v1 == 9 ] = 9
fr = list(0, 1, 2:3, 9, ".")
to = list(1, 0, -1, 9, NA)
if_val(v1, from = fr) = to
expect_identical(
    v1
    , v_test)

set.seed(123)
qvar = sample((-5):20, 50, replace = TRUE)
# RECODE QVAR(1 THRU 5=1)(6 THRU 10=2)(11 THRU HI=3)(ELSE=0).
qvar_test = qvar
qvar_test[] = 0
qvar_test[qvar %in% 1:5] = 1
qvar_test[qvar %in% 6:10] = 2
qvar_test[qvar >= 11] = 3

fr = list(1 %thru% 5, 6 %thru% 10, gte(11), ".")
to = list(1, 2, 3, 0)
expect_identical(
    if_val(qvar, from = fr, to = to),
    qvar_test
)


strngvar = LETTERS
# RECODE STRNGVAR ('A','B','C'='A')('D','E','F'='B')(ELSE=' ').
letters_test = strngvar
letters_test[] = " "
letters_test[LETTERS %in% c('A','B','C')] = 'A'
letters_test[LETTERS %in% c('D','E','F')] = 'B'
fr = list(c('A','B','C'), c('D','E','F') , ".")
to = list("A", "B", " ")
expect_identical(
    if_val(strngvar, from = fr, to = to),
    letters_test
)

set.seed(123)
age = sample(c(sample(5:30, 40, replace = TRUE), rep(9, 10)))
# RECODE AGE (MISSING=9) (18 THRU HI=1) (0 THRU 18=0) INTO VOTER.
voter_test = age
voter_test[age == 9 ] = NA
voter_test[voter_test %in% 0:17 ] = 0
voter_test[voter_test %in% 18:100 ] = 1
fr = list(9, 18 %thru% Inf, 0 %thru% 18)
to = list(NA, 1, 0)
expect_identical(
    if_val(age, from = fr, to = to),
    voter_test
)

context("ifs")
a = 1:5
b = 5:1
expect_identical(
    ifs(b>3 ~ 1), 
    c(1,1,NA, NA, NA)
)
expect_identical(
    ifs(b>3 ~ 1, default = 3),
    c(1,1,3, 3, 3)
)
expect_identical(
    ifs(c(b, NA)>3 ~ 1, default = 3),
    c(1,1,3, 3, 3, 3)
)
expect_identical(
    ifs(b>3 ~ 1, a>4 ~ 7, default = 3),
    c(1,1,3, 3, 7)
    
)

expect_identical(
    ifs(b>3 ~ a),
    as.integer(c(1,2,NA,NA, NA))
)
expect_identical(
    ifs(b>3 ~ a, default = "wah"),
    c("1","2","wah","wah", "wah")
)

expect_error(
    ifs(b>3 ~ cbind(a,b))
)

expect_error(
    ifs(b>3 ~ t(a))
)

expect_error(
    ifs(45 ~ 1)
)

expect_error(
    ifs(cbind(c(T,T), c(F,F)) ~ 1)
)


context("if_val type")

a = 1:3
var_lab(a) = "aadad"
if_val(a) = 1 ~ "bah"
expect_identical(class(a), c("labelled", "character"))

a = factor(letters[1:4])
if_val(a) = "a" ~ "z"
res = factor(c("z", "b", "c", "d"), levels = c("a", "b", "c", "d", "z"))
expect_identical(a, res)

a = factor(letters[1:4])
var_lab(a) = "factor"
if_val(a) = "a" ~ "z"
var_lab(res) = "factor"
expect_identical(a, res)



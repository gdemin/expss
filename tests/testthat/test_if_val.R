context("recode simple vector")


expect_error(recode(1, 42))
expect_error(recode(1))
expect_error(recode(1, ~ 42))


expect_identical(recode(1:5, 1~-1), c(-1, NA, NA, NA, NA))
expect_identical(recode(1:5, 1~-1, other ~ copy), c(-1, 2, 3, 4, 5))
expect_identical(recode(1:5, 1~-1, other() ~ copy()), c(-1, 2, 3, 4, 5))
b = 1:5
recode(b) = 1~-1
expect_identical(b, c(-1, 2, 3, 4, 5))

expect_identical(recode(1:5, 1~-1, 2 ~ NA), c(-1, NA, NA, NA, NA))
expect_identical(recode(1:5, 1~-1, 2 ~ NA, other ~ copy), c(-1, NA, 3, 4, 5))

b = 1:5
recode(b) = c(1~-1, 2 ~ NA)
expect_identical(b, c(-1, NA, 3, 4, 5))

expect_identical(recode(1:5, gt(2)~99, other ~ copy), c(1, 2, 99, 99, 99))
expect_identical(recode(1:5, gt(2)~99), c(NA, NA, 99, 99, 99))

b = 1:5
recode(b) = gt(2)~99
expect_identical(b, c(1, 2, 99, 99, 99))

expect_identical(recode(1:5, gt(2)~99, other ~ 0), c(0, 0, 99, 99, 99))

expect_identical(recode(1:5, 1:3 ~ 1, other ~ NA), c(1, 1, 1, NA, NA))

expect_identical(recode(1:5, 1:3 ~ 1, 2:5 ~ 2), c(1, 1, 1, 2, 2))

expect_identical(recode(1:5, lt(2) ~ 10, lt(3) ~ 11, lt(4) ~ 12, other ~ NA), c(10, 11, 12, NA, NA))

expect_identical(recode(1:5, 4 ~ "four", (function(x) (x-1)<2) ~-1, other ~ copy), c(-1, -1, 3, "four", 5) )
expect_identical(recode(1:5, 4 ~ "four", (function(x) (x-1)<2) ~-1), c(-1, -1, NA, "four", NA) )

b = 1:5
recode(b) = c(4 ~ "four", (function(x) (x-1)<2) ~-1)
expect_identical(b, c(-1, -1, 3, "four", 5) )

x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)

expect_identical(recode(x, gt(2)~y, other ~ copy), c(1, 8, 1, 9, NA))
expect_identical(recode(x, gt(2) ~ y), c(NA, 8, NA, 9, NA))
recode(x) = gt(2)~y
expect_identical(x, c(1, 8, 1, 9, NA))



x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)

expect_identical(recode(x, list(gt(2)~y), other ~ copy), c(1, 8, 1, 9, NA))
expect_identical(recode(x, list(gt(2)~y, other ~ copy)), c(1, 8, 1, 9, NA))

expect_identical(recode(x, list(gt(2)~y)), c(NA, 8, NA, 9, NA))
recode(x) = list(gt(2)~y)
expect_identical(x, c(1, 8, 1, 9, NA))


x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)


expect_identical(recode(x, gt(2)~y, le(2) ~ z, other ~ copy), c(4, 8, 4, 9, NA))
expect_identical(recode(x, gt(2)~y, le(2) ~ z), c(4, 8, 4, 9, NA))

recode(x) = c(gt(2)~y, le(2) ~ z)
expect_identical(x, c(4, 8, 4, 9, NA))



x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)

expect_identical(recode(x, gt(2)~y, le(2) ~ z, other ~ 99), c(4, 8, 4, 9, 99))

recode(x) = list(gt(2)~y, le(2) ~ z, other ~ 99)
expect_identical(x, c(4, 8, 4, 9, 99))

x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)

expect_identical(recode(x, list(gt(2)~y, le(2) ~ z, other ~99)), c(4, 8, 4, 9, 99))
recode(x) = list(gt(2)~y, le(2) ~ z, other ~99)
expect_identical(x, c(4, 8, 4, 9, 99))

x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)
expect_identical(recode(x, (z>4)~y, other ~ copy), c(1, 3, 1, 9, 9))
expect_identical(recode(x, (z>4)~y), c(NA, NA, NA, 9, 9))

recode(x) = (z>4)~y
expect_identical(x, c(1, 3, 1, 9, 9))



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
    w = recode(x, gt(2) ~ y, other ~ copy)
})

expect_identical(dfs$w, c(2, 18, 2, 19, NA))

dfs$x = NULL
dfs$w = NULL
dfs  =  modify(dfs, { 
    w = recode(x, gt(2)~y, other ~ copy)
})
expect_identical(dfs$w, c(1, 18, 1, 19, NA))

dfs$x = NULL
dfs$y = NULL
dfs$w = NULL
dfs  = modify(dfs, { 
    w = recode(x, gt(2)~y, other ~ copy)
})
expect_identical(dfs$w, c(1, 8, 1, 9, NA))
##########################

context("recode 'from, to' notation simple vector")

expect_identical(recode(1:5, from = list(1, other), to = list(-1, copy)), c(-1, 2, 3, 4, 5))
expect_identical(recode(1:5, from = 1:2, to =c(-1, NA)), c(-1, NA, NA, NA, NA))

expect_identical(recode(1:5, from = list(1, 2, other), to =list(-1, NA, copy)), c(-1, NA, 3, 4, 5))

expect_identical(recode(1:5, from = list(gt(2), other), to=list(99, copy)), c(1, 2, 99, 99, 99))

expect_identical(recode(1:5, from = list(gt(2)), to=99), c(NA, NA, 99, 99, 99))

b = 1:5
recode(b, list(gt(2))) = 99 
expect_identical(b, c(1, 2, 99, 99, 99))

b = 1:5
recode(b, gt(2)) = 99 
expect_identical(b, c(1, 2, 99, 99, 99))



expect_identical(recode(1:5, from = c(gt(2),other), to = c(99,0)), c(0, 0, 99, 99, 99))

b = 1:5
recode(b, c(gt(2),copy)) = c(99,0)
expect_identical(b, c(0, 0, 99, 99, 99))



expect_identical(recode(1:5, from = list(1:3,other), to = c(1, NA)), c(1, 1, 1, NA, NA))

expect_equal(recode(1:5, from = list(1:3, 2:5),  to =1:2), c(1, 1, 1, 2, 2))

expect_identical(recode(1:5, from = list(lt(2), lt(3), lt(4),other), to =c(10,  11, 12, NA)), c(10, 11, 12, NA, NA))

expect_identical(recode(1:5, from = list(4,function(x) (x-1)<2, other),  to = c("four", -1, copy)), c(-1, -1, 3, "four", 5) )
expect_identical(recode(1:5, from = list(4,function(x) (x-1)<2),  to = c("four", -1)), c(-1, -1, NA, "four", NA) )

x = c(1,3,1,3,NA)
y = c(8,8,8,9,9)
z = c(4,4,4,5,5)

expect_identical(recode(x, from = list(gt(2)), to =list(y)), c(NA, 8, NA, 9, NA))
expect_identical(recode(x, from = list(gt(2), other), to =list(y, copy)), c(1, 8, 1, 9, NA))


expect_identical(recode(x, from = list(gt(2), le(2)), to = list(y,z)), c(4, 8, 4, 9, NA))
expect_identical(recode(x, from = list(gt(2), le(2), other), to = list(y,z, copy)), c(4, 8, 4, 9, NA))

expect_identical(recode(x, from = list(gt(2), le(2),other), to = list(y,z,99)), c(4, 8, 4, 9, 99))

expect_identical(recode(x, from = list(z>4, other), to = list(y, copy)), c(1, 3, 1, 9, 9))

expect_identical(recode(x, from = list(z>4), to = list(y)), c(NA, NA, NA, 9, 9))



context("recode examples")
set.seed(123)
v1  = sample(c(0:3,9,10), 20, replace = TRUE)
# RECODE V1 TO V3 (0=1) (1=0) (2,3=-1) (9=9) (ELSE=SYSMIS)
v_test = v1
v_test[] = NA
v_test[v1 == 0] = 1
v_test[v1 == 1] = 0
v_test[v1 %in% 2:3] = -1
v_test[v1 == 9 ] = 9
recode(v1) = c(0 ~ 1, 1 ~ 0, 2:3 ~ -1, 9 ~ 9, other ~ NA)
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
    recode(qvar, 1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, ge(11) ~ 3, other ~ 0),
    qvar_test
)
recode(qvar) = c(1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, 11 %thru% Inf ~ 3, other ~ 0)
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
    recode(strngvar, c('A','B','C') ~ 'A', c('D','E','F') ~ 'B', other ~ ' '),
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
    recode(age, 9 ~ NA, 18 %thru% Inf ~ 1, 0 %thru% 18 ~ 0),
    voter_test
)

context("recode examples from/to")
set.seed(123)
v1  = sample(c(0:3,9,10), 20, replace = TRUE)
# RECODE V1 TO V3 (0=1) (1=0) (2,3=-1) (9=9) (ELSE=SYSMIS)
v_test = v1
v_test[] = NA
v_test[v1 == 0] = 1
v_test[v1 == 1] = 0
v_test[v1 %in% 2:3] = -1
v_test[v1 == 9 ] = 9
fr = list(0, 1, 2:3, 9, other)
to = list(1, 0, -1, 9, NA)
recode(v1, from = fr) = to
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

fr = list(1 %thru% 5, 6 %thru% 10, ge(11), other)
to = list(1, 2, 3, 0)
expect_identical(
    recode(qvar, from = fr, to = to),
    qvar_test
)


strngvar = LETTERS
# RECODE STRNGVAR ('A','B','C'='A')('D','E','F'='B')(ELSE=' ').
letters_test = strngvar
letters_test[] = " "
letters_test[LETTERS %in% c('A','B','C')] = 'A'
letters_test[LETTERS %in% c('D','E','F')] = 'B'
fr = list(c('A','B','C'), c('D','E','F') , other)
to = list("A", "B", " ")
expect_identical(
    recode(strngvar, from = fr, to = to),
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
    recode(age, from = fr, to = to),
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
    ifs(b>3 ~ 1, TRUE ~ 3),
    c(1,1,3, 3, 3)
)
expect_identical(
    ifs(c(b, NA)>3 ~ 1, TRUE ~ 3),
    c(1,1,3, 3, 3, 3)
)
expect_identical(
    ifs(b>3 ~ 1, a>4 ~ 7, TRUE ~ 3),
    c(1,1,3, 3, 7)
    
)

expect_identical(
    ifs(b>3 ~ a),
    as.integer(c(1,2,NA,NA, NA))
)
expect_identical(
    ifs(b>3 ~ a, TRUE ~ "wah"),
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


context("recode type")

a = 1:3
var_lab(a) = "aadad"
recode(a) = 1 ~ "bah"
expect_identical(class(a), c("labelled", "character"))

a = factor(letters[1:4])

res = factor(c("a", "z", "c", "d"), levels = c("z", "a", "c", "d"))
b = recode(a, "b" ~ "z", other ~ copy)
expect_identical(b, res)

res = factor(c("z", "b", "c", "d"), levels = c("a", "b", "c", "d", "z"))
b = recode(a, "a" ~ factor("z"))
res2 = res
res2[2:4] = NA
expect_identical(b, factor(res2)) 

recode(a) = "a" ~ "z"
expect_identical(a, res)

a = factor(letters[1:4])
var_lab(a) = "factor"
recode(a) = "a" ~ "z"
var_lab(res) = "factor"
expect_identical(a, res)


a = as.POSIXct("2016-10-01")

b = recode(a, "2016-10-01" ~ as.POSIXct("2016-10-02"))

expect_equal(b, as.POSIXct("2016-10-02"))

b = recode(a, "2016-10-01" ~ "2016-10-02")

expect_equal(b, "2016-10-02")

recode(a) = c("2016-10-01" ~ "2016-10-02")

expect_identical(a, as.POSIXct("2016-10-02"))

a = as.POSIXct(c("2016-10-01", "2017-05-10"))
res = as.POSIXct(c("2016-10-02", "2017-05-11"))
b = recode(a, "2016-10-01" ~ as.POSIXct("2016-10-02"), "2017-05-10" ~ "2017-05-11")
expect_equal(b, res)

b = recode(a, "2016-10-01" ~ "2016-10-02", "2017-05-10" ~ "2017-05-11")
expect_equal(b, as.character(res))

recode(a) = "2016-10-01" ~ "2016-10-02"
expect_identical(a, as.POSIXct(c("2016-10-02", "2017-05-10")))

context("dot notation")

a = 1:5

expect_identical(recode(a, 1:4 ~ NA, 5 ~ copy), c(NA, NA, NA, NA, 5L))

context("type conversion")

expect_identical(recode("a", "a" ~ 1), 1)

context("recode to function")
expect_identical(recode(letters, other ~ toupper), LETTERS)

letters2 = letters
recode(letters2) = c(other ~ toupper)
expect_identical(letters2, LETTERS)

context("ifs incorrect from to")

expect_error(ifs(letters, from= c("a", "b")))
expect_error(ifs(letters, from= c("a", "b"), to = c("aa")))


context("recode list")

mk_emtpy_obj = expss:::make_empty_object
data(iris)
a = 1:4
b = 4:1
ab = list(a,b, iris)
names(ab) = c("a", "b", "c")
empty_iris = iris

empty_iris[[1]] = NA
empty_iris[[2]] = NA
empty_iris[[3]] = NA
empty_iris[[4]] = NA
empty_iris[[5]] = NA
rownames(empty_iris) = as.character(1:150)
res = list(rep(NA, 4), rep(NA, 4), empty_iris)
names(res) = c("a", "b", "c")

expect_identical(mk_emtpy_obj(ab), res)


context("%into%")

remove_if_exists = function(...){
    for(each in unlist(list(...))){
        if(exists(each, envir = parent.frame(), inherits = FALSE)){
            rm(list = each, pos = parent.frame())
        }
    }
    invisible(NULL)
}
remove_if_exists("x")
1 %into% x
expect_identical(x, 1)

remove_if_exists("y")
x = "y"

1 %into% ((x))
expect_identical(x, "y")
expect_identical(y, 1)

remove_if_exists("x", "y", "z")
many_vars = function() c("x", "y", "z")
list(1,2,3) %into% many_vars()
expect_identical(x, 1)
expect_identical(y, 2)
expect_identical(z, 3)

remove_if_exists("x", "y", "z")
1:3 %into% many_vars()
expect_identical(x, 1:3)
expect_identical(y, 1:3)
expect_identical(z, 1:3)

remove_if_exists("x", "y", "z")

..[x] = 1
expect_identical(x, 1)

remove_if_exists("y")
x = "y"

..[(x)] = 1 
expect_identical(x, "y")
expect_identical(y, 1)


remove_if_exists("x", "y", "z")
many_vars = function() c("x", "y", "z")
..[many_vars()] = list(1,2,3) 
expect_identical(x, 1)
expect_identical(y, 2)
expect_identical(z, 3)

remove_if_exists("x", "y", "z")
..[many_vars()] = 1:3 
expect_identical(x, 1:3)
expect_identical(y, 1:3)
expect_identical(z, 1:3)

remove_if_exists("x", "y", "z")
remove_if_exists("x1", "x2", "x3")

..[x1 %to% x3] = list(1:2, letters, TRUE)
expect_identical(x1, 1:2)
expect_identical(x2, letters)
expect_identical(x3, TRUE)

b = 1:5
b2 = rev(b)
recode(b, 1~-1, other ~ copy) %into%  b_recoded

expect_identical(b_recoded, c(-1, 2, 3, 4, 5))

recode(b, 1~-1, other ~ copy) %into%  "b_recoded_chr"
expect_identical(b_recoded_chr, c(-1, 2, 3, 4, 5))

recode(list(sheet(b, b2)), 1~-1, other ~ copy) %into%  qc(b_recoded)
rownames(b_recoded) = NULL
expect_identical(b_recoded, sheet(b = c(-1, 2, 3, 4, 5), b2 = rev( c(-1, 2, 3, 4, 5))))


recode(sheet(b, b2), 1~10, other ~ copy) %into%  qc(b1_recoded, b2_recoded)
expect_identical(b1_recoded, c(10, 2, 3, 4, 5))
expect_identical(b2_recoded, rev( c(10, 2, 3, 4, 5)))

rm(b1_recoded, b2_recoded)
recode(sheet(b, b2), 1~10, other ~ copy) %into%  c(b1_recoded, b2_recoded)
expect_identical(b1_recoded, c(10, 2, 3, 4, 5))
expect_identical(b2_recoded, rev( c(10, 2, 3, 4, 5)))



recode(as.matrix(cbind(b, b2)), 1~11, other ~ copy) %into%  qc(b1_recoded_mat, b2_recoded_mat)
expect_identical(b1_recoded_mat, c(11, 2, 3, 4, 5))
expect_identical(b2_recoded_mat, rev( c(11, 2, 3, 4, 5)))

recode(sheet(b, b2), 1~10, other ~ copy) %into%  c(b1_recoded, b2_recoded)
expect_identical(b1_recoded, c(10, 2, 3, 4, 5))
expect_identical(b2_recoded, rev( c(10, 2, 3, 4, 5)))

recode(list(b, b2), 1~10, other ~ copy) %into%  list(b3_recoded, b4_recoded)
expect_identical(b3_recoded, c(10, 2, 3, 4, 5))
expect_identical(b4_recoded, rev( c(10, 2, 3, 4, 5)))


recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  subst('v`1:3`')
expect_identical(v1, c(10, 2, 3, 4, 5))
expect_identical(v2, c(10, 2, 3, 4, 5))
expect_identical(v3, rev( c(10, 2, 3, 4, 5)))
i = 1:3
recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  subst('ww`i`')
expect_identical(ww1, c(10, 2, 3, 4, 5))
expect_identical(ww2, c(10, 2, 3, 4, 5))
expect_identical(ww3, rev( c(10, 2, 3, 4, 5)))

i = 1:2
j = 3
recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  subst(c('xx`i`', 'y`j`'))
expect_identical(xx1, c(10, 2, 3, 4, 5))
expect_identical(xx2, c(10, 2, 3, 4, 5))
expect_identical(y3, rev( c(10, 2, 3, 4, 5)))


i = 1:3
recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  subst('wah`i`')
expect_identical(wah1, c(10, 2, 3, 4, 5))
expect_identical(wah2, c(10, 2, 3, 4, 5))
expect_identical(wah3, rev( c(10, 2, 3, 4, 5)))

i = 1:2
j = 3
recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  c(subst('xax`i`'), subst('xy`j`'))
expect_identical(xax1, c(10, 2, 3, 4, 5))
expect_identical(xax2, c(10, 2, 3, 4, 5))
expect_identical(xy3, rev( c(10, 2, 3, 4, 5)))

my_fun = function() c("x", "y", "z") 
recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  my_fun()
expect_identical(x, c(10, 2, 3, 4, 5))
expect_identical(y, c(10, 2, 3, 4, 5))
expect_identical(z, rev( c(10, 2, 3, 4, 5)))

recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  (va1 %to% va3)
expect_identical(va1, c(10, 2, 3, 4, 5))
expect_identical(va2, c(10, 2, 3, 4, 5))
expect_identical(va3, rev( c(10, 2, 3, 4, 5)))

recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  (vb0 %to% vb2)
expect_identical(vb0, c(10, 2, 3, 4, 5))
expect_identical(vb1, c(10, 2, 3, 4, 5))
expect_identical(vb2, rev( c(10, 2, 3, 4, 5)))

expect_error(recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  (vb2 %to% vb4))

expect_error(recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  (v0 %to% v2))
expect_error(recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  (v01 %to% v002))
recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  (v01 %to% v03)
expect_identical(v01, c(10, 2, 3, 4, 5))
expect_identical(v02, c(10, 2, 3, 4, 5))
expect_identical(v03, rev( c(10, 2, 3, 4, 5)))

recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  (v001 %to% v003)
expect_identical(v001, c(10, 2, 3, 4, 5))
expect_identical(v002, c(10, 2, 3, 4, 5))
expect_identical(v003, rev( c(10, 2, 3, 4, 5)))

recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  c(vax, f002 %to% f003)
expect_identical(vax, c(10, 2, 3, 4, 5))
expect_identical(f002, c(10, 2, 3, 4, 5))
expect_identical(f003, rev( c(10, 2, 3, 4, 5)))

recode(sheet(b, b1=b, b2), 1~42, other ~ copy) %into%  c(vax, f002 %to% f003)
expect_identical(vax, c(42, 2, 3, 4, 5))
expect_identical(f002, c(42, 2, 3, 4, 5))
expect_identical(f003, rev( c(42, 2, 3, 4, 5)))

expect_error(
    recode(sheet(b, b1=b, b2), 1~42, other ~ copy) %into%  c(g001 %to% gg003)
)

expect_error(
    recode(sheet(b, b1=b, b2), 1~42, other ~ copy) %into%  c(g003 %to% g001)
)

data(iris)
expect_error(
    modify(iris, {
        recode (Sepal.Length %to% Petal.Width, gt(1) ~ 1, other ~ 0) %into% ( Petal.Width %to% Sepal.Length)
        
    })
)
expect_error(recode(sheet(b, b1=b, b2), 1~10, other ~ copy) %into%  (v0001 %to% v0002))

recode(b, 1~10, other ~ copy) %into%  (v0001 %to% v0003)
expect_identical(v0001, c(10, 2, 3, 4, 5))
expect_identical(v0002, c(10, 2, 3, 4, 5))
expect_identical(v0003, c(10, 2, 3, 4, 5))



data(iris)
new_iris = modify(iris, {
    recode (Sepal.Length %to% Petal.Width, gt(1) ~ 1, other ~ 0) %into% (v01 %to% v04)
    
})

res_iris = iris
res_iris[, c("v01", "v02", "v03", "v04")] = 
    calc(iris, recode (Sepal.Length %to% Petal.Width, gt(1) ~ 1, other ~ 0))

expect_identical(new_iris, res_iris)

new_iris = modify(new_iris, {
    recode (v01 %to% v04, 1 ~ 1, 0 ~ NA) %into% (Sepal.Length %to% Petal.Width)
    
})

res_iris[, 1:4] = 
    calc(res_iris, recode (v01 %to% v04, 1 ~ 1, 0 ~ NA))

expect_identical(new_iris, res_iris)


# context("recode factor")
# 
# df = data.frame(id = c(1,2,3,4,5),
#                 Did_you_use_tv=factor(c("tv","","","tv","tv")),
#                 Did_you_use_internet=factor(c("","","","int","int")))
# 
# new_df = df
# 
# recode(df[,-1], "" ~ 0, other ~ 1)
# recode(new_df[,-1]) = c("" ~ 0, other ~ 1)

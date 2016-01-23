context("if_na.vector")

a = 1:4
b = a
expect_identical(if_na(a, 2), b)
a[1] = NA
b[1] = 2
expect_identical(if_na(a, 2), b)

a[3] = NA
b[3] = 2
expect_identical(if_na(a, 2), b)

b[1] = 4
b[3] = 2
expect_identical(if_na(a, 4:1), as.integer(b))

expect_error(if_na(a, 1:2))
expect_error(if_na(a, t(1:2)))
expect_identical(if_na(numeric(0), 1),numeric(0))

context("if_na.data.frame")

a = data.frame(a = 1:4, b = 5:8, d = 10:13)
val_lab(a$a) = c('aaa' = 1)
b = a
expect_identical(if_na(a, 2), b)

a[1,1] = NA
b[1,1] = 2

expect_identical(if_na(a, 2), b)

a[4,1] = NA 
b[4,1] = 2
expect_identical(if_na(a, 2), b)

b[1,1] = 4
b[4,1] = 1

expect_equal(if_na(a, 4:1), b)

a[1,3] = NA
b[1,3] = 4
expect_equal(if_na(a, 4:1), b)

b[1,1] = 3
b[4,1] = 3
b[1,3] = 1
expect_equal(if_na(a, t(3:1)), b)
expect_error(if_na(a, t(3:2)))
expect_error(if_na(a, 3:2))


b[1,1] = 4
b[4,1] = 1
b[1,3] = -1
expect_equal(if_na(a, cbind(4:1,2,-(1:4))), b)
expect_equal(if_na(a, as.data.frame(cbind(4:1,2,-(1:4)))), b)


context("if_na.matrix")

a = as.matrix(data.frame(a = 1:4, b = 5:8, d = 10:13))

b = a
expect_identical(if_na(a, 2), b)

a[1,1] = NA
b[1,1] = 2

expect_identical(if_na(a, 2), b)

a[4,1] = NA 
b[4,1] = 2
expect_identical(if_na(a, 2), b)

b[1,1] = 4
b[4,1] = 1

expect_equal(if_na(a, 4:1), b)

a[1,3] = NA
b[1,3] = 4
expect_equal(if_na(a, 4:1), b)

b[1,1] = 3
b[4,1] = 3
b[1,3] = 1
expect_equal(if_na(a, t(3:1)), b)
expect_error(if_na(a, t(3:2)))
expect_error(if_na(a, 3:2))


b[1,1] = 4
b[4,1] = 1
b[1,3] = -1
expect_equal(if_na(a, cbind(4:1,2,-(1:4))), b)
expect_equal(if_na(a, as.data.frame(cbind(4:1,2,-(1:4)))), b)

context("tbl_df")

library(dplyr)
a = tbl_df(data.frame(a = 1:4, b = 5:8, d = 10:13))

b = a
expect_identical(if_na(a, 2), b)

a[1,1] = NA
b[1,1] = 2

expect_identical(if_na(a, 2), b)

a[4,1] = NA 
b[4,1] = 2
expect_identical(if_na(a, 2), b)

b[1,1] = 4L
b[4,1] = 1L
b$a = as.integer(b$a)
expect_equal(if_na(a, 4:1), b)

a[1,3] = NA
b[1,3] = 4L
b$d = as.integer(b$d)
expect_equal(if_na(a, 4:1), b)

b[1,1] = 3
b[4,1] = 3
b[1,3] = 1
b$a = as.integer(b$a)
b$d = as.integer(b$d)
expect_equal(if_na(a, t(3:1)), b)
expect_error(if_na(a, t(3:2)))
expect_error(if_na(a, 3:2))


b[1,1] = 4
b[4,1] = 1
b[1,3] = -1

expect_equal(if_na(a, cbind(4:1,2,-(1:4))), b)
expect_equal(if_na(a, as.data.frame(cbind(4:1,2,-(1:4)))), b)

context("list")

a = 1:4
b = 4:1
ab = list(a,b)
val_lab(ab) = c("a"=1, "b" = 2)

expect_identical(if_na(ab, 42), ab)
expect_error(if_na(ab, list(42)))

ab[[1]][1] = NA
ab[[2]][4] = NA

ab_no_na = ab

ab_no_na[[1]][1] = 42
ab_no_na[[2]][4] = 42
expect_identical(if_na(ab, 42), ab_no_na)

ab_no_na[[1]][1] = 42
ab_no_na[[2]][4] = 43
expect_identical(if_na(ab, list(42,43)), ab_no_na)
expect_equal(if_na(ab, list(42:39,40:43)), ab_no_na)
expect_error(if_na(ab, data.frame(42:39,40:43)))
expect_equal(if_na(ab, list(data.frame(42:39),40:43)),ab_no_na)
expect_equal(if_na(ab, list(as.matrix(42:39),40:43)),ab_no_na)

context("help")

# simple case
a = c(NA, 2, 3, 4, NA)
if_na(a) = 1
expect_identical(a, c(1, 2, 3, 4, 1))

# replacement with values from other variable
a = c(NA, 2, 3, 4, NA)
if_na(a) = 1:5
expect_equal(a, 1:5)

# replacement with group means

# make data.frame 
set.seed(123)
group = sample(1:3, 30, replace = TRUE)
param = runif(30)
param[sample(30, 10)] = NA # place 10 NA's
df = data.frame(group, param)

# replace NA's with group means
df_clean = df %>% group_by(group) %>% 
    mutate(
        param = if_na(param, mean(param, na.rm = TRUE))
        )

df = within(df, {
    param[group==1 & is.na(param)] = mean(param[group==1], na.rm = TRUE)
    param[group==2 & is.na(param)] = mean(param[group==2], na.rm = TRUE)
    param[group==3 & is.na(param)] = mean(param[group==3], na.rm = TRUE)
})

expect_identical(as.data.frame(df_clean), df)

# replacement with column means

# make data.frame
set.seed(123)
x1 = runif(30)
x2 = runif(30)
x3 = runif(30)
x1[sample(30, 10)] = NA # place 10 NA's
x2[sample(30, 10)] = NA # place 10 NA's
x3[sample(30, 10)] = NA # place 10 NA's

df = data.frame(x1, x2, x3)
df_test = df
df_test2 = df
if_na(df) = t(colMeans(df, na.rm = TRUE))

df_test = within(df_test, {
    x1[is.na(x1)] = mean(x1, na.rm = TRUE)
    x2[is.na(x2)] = mean(x2, na.rm = TRUE)
    x3[is.na(x3)] = mean(x3, na.rm = TRUE)
})

expect_identical(df, df_test)

# just for curiosity - assignemnet form doesn't work inside mutate
df_test2 = within(df_test2, {
    if_na(x1) = mean(x1, na.rm = TRUE)
    if_na(x2) = mean(x2, na.rm = TRUE)
    if_na(x3) = mean(x3, na.rm = TRUE)
    
})

expect_identical(df, df_test2)

context("if_na.vector")
suppressWarnings(RNGversion("3.5.0"))


a = 1:4
b = a
expect_identical(if_na(a, 2), b)
a[1] = NA
b[1] = 2
expect_identical(if_na(a, 2), b)

a[1] = NA
val_lab(a) = c(one = 1)
b = a
b[1] = 2
expect_identical(if_na(a, c(two = 2)), b)
expect_identical(if_na(a, 2, label = "Hard to say"), add_val_lab(b, c("Hard to say" = 2)))
a2 = a
if_na(a2, label = "Hard to say") = 2
expect_identical(a2, add_val_lab(b, c("Hard to say" = 2)))
expect_error(if_na(a, 1:4, label = "Error"))

a = 1:4
b = a
a[1] = NA
b[1] = 2

a[3] = NA
b[3] = 2
expect_identical(if_na(a, 2), b)

expect_identical(a %if_na% 2, b)

b[1] = 4
b[3] = 2
expect_identical(if_na(a, 4:1), as.integer(b))
expect_identical(a %if_na% 4:1, as.integer(b))

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

expect_equal(if_na(a, 2), b)

a[4,1] = NA 
b[4,1] = 2
expect_equal(if_na(a, 2), b)

b[1,1] = 4
b[4,1] = 1

expect_equal(if_na(a, c(4:2,1.0)), b)

a[1,3] = NA
b[1,3] = 4
expect_equal(if_na(a, c(4:2,1.0)), b)

b[1,1] = 3
b[4,1] = 3
b[1,3] = 1




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

expect_equal(if_na(a, rep(4:1, 3)), b)

a[1,3] = NA
b[1,3] = 4
expect_equal(if_na(a, rep(4:1, 3)), b)


context("if_na list")

a = 1:4
b = 4:1
ab = list(a,b)
val_lab(ab) = c("a"=1, "b" = 2)

expect_identical(if_na(ab, 42), ab)

ab[[1]][1] = NA
ab[[2]][4] = NA

ab_no_na = ab

ab_no_na[[1]][1] = 42
ab_no_na[[2]][4] = 42
expect_identical(if_na(ab, 42), ab_no_na)

ab_no_na[[1]][1] = 42
ab_no_na[[2]][4] = 42
expect_error(if_na(ab, list(42,43)))

context("if_na help")

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


# replacement with column means


context("if_na add_val_lab")


set.seed(123)
x1 = runif(30)
x2 = runif(30)
x3 = runif(30)
x1[sample(30, 10)] = NA # place 10 NA's
x2[sample(30, 10)] = NA # place 10 NA's
x3[sample(30, 10)] = NA # place 10 NA's

df = data.frame(x1, x2, x3)
df_test = df

if_na(df) = c("h/s" = 99)

df_test = within(df_test, {
    x1[is.na(x1)] = 99
    x2[is.na(x2)] = 99
    x3[is.na(x3)] = 99
})



expect_identical(df, df_test)

context("if_na factor")

fac = factor(c("a","b",NA))

expect_identical(if_na(fac, "c"), factor(c("a","b","c")))
expect_identical(if_na(fac, "a"), factor(c("a","b","a")))

context("if_na POSIXct")

ct = c(as.POSIXct("2016-09-24"), NA)
expect_equal(if_na(ct, "2016-09-25"), as.POSIXct(c("2016-09-24", "2016-09-25")))


ct = c(as.Date("2016-09-24"), NA)
expect_equal(if_na(ct, "2016-09-25"), as.Date(c("2016-09-24", "2016-09-25")))


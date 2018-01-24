context("if_val with NA.vector")

a = 1:4
b = a

expect_equal(if_val(a, NA ~ 2, other ~ copy), b)
a[1] = NA
b[1] = 2
expect_equal(if_val(a, NA ~ 2, other ~ copy), b)

a[3] = NA
b[3] = 2
expect_identical(if_val(a, NA ~ 2, other ~ copy), b)

b[1] = 4
b[3] = 2
expect_identical(if_val(a, NA ~ 4:1, other ~ copy), as.integer(b))

expect_error(if_val(a, NA ~ 1:2, other ~ copy))
expect_error(if_val(a, NA ~ t(1:2), other ~ copy))
expect_identical(if_val(numeric(0), NA ~ 1, other ~ copy),logical(0))

context("if_val with NA.data.frame")

a = data.frame(a = as.double(1:4), b = as.double(5:8), d = as.double(10:13))
b = a
rownames(b) = rownames(a)
val_lab(a$a) = c('aaa' = 1)

expect_identical(if_val(a, NA ~ 2, other ~ copy), b)

a[1,1] = NA
b[1,1] = 2

expect_equal(if_val(a, NA ~ 2, other ~ copy), b)

a[4,1] = NA 
b[4,1] = 2
expect_equal(if_val(a, NA ~ 2, other ~ copy), b)

b[1,1] = 4
b[4,1] = 1

expect_equal(if_val(a, NA ~ (4:1)*1.0,  other ~ copy), b)

a[1,3] = NA
b[1,3] = 4
expect_equal(if_val(a, NA ~ (4:1)*1.0, other ~ copy), b)

b[1,1] = 3
b[4,1] = 3
b[1,3] = 1
expect_equal(if_val(a, NA ~ t(3:1)*1.0, other ~ copy), b)
expect_error(if_val(a, NA ~ t(3:2), other ~ copy))
expect_error(if_val(a, NA ~ 3:2, other ~ copy))


b[1,1] = 4
b[4,1] = 1
b[1,3] = -1
expect_equal(if_val(a, NA ~ cbind(4:1,2,-(1:4)), other ~ copy), b)
expect_equal(if_val(a, NA ~ as.data.frame(cbind(4:1,2,-(1:4))), other ~ copy), b)


context("if_val with NA.matrix")

a = as.matrix(data.frame(a = 1:4, b = 5:8, d = 10:13))

b = a
expect_equal(if_val(a, NA ~ 2, other ~ copy), b)

a[1,1] = NA
b[1,1] = 2

expect_identical(if_val(a, NA ~ 2, other ~ copy), b)

a[4,1] = NA 
b[4,1] = 2
expect_identical(if_val(a, NA ~ 2, other ~ copy), b)

b[1,1] = 4
b[4,1] = 1

expect_equal(if_val(a, NA ~ 4:1, other ~ copy), b)

a[1,3] = NA
b[1,3] = 4
expect_equal(if_val(a, NA ~ 4:1, other ~ copy), b)

b[1,1] = 3
b[4,1] = 3
b[1,3] = 1
expect_equal(if_val(a, NA ~ t(3:1)*1.0, other ~ copy), b)
expect_error(if_val(a, NA ~ t(3:2)))
expect_error(if_val(a, NA ~ 3:2))


b[1,1] = 4
b[4,1] = 1
b[1,3] = -1
expect_equal(if_val(a, NA ~ cbind(4:1,2,-(1:4)), other ~ copy), b)
expect_equal(if_val(a, NA ~ as.data.frame(cbind(4:1,2,-(1:4))), other ~ copy), b)



context("if_val with NA list")

a = 1:4
b = 4:1
ab = list(a,b)
val_lab(ab) = c("a"=1, "b" = 2)

expect_identical(if_val(ab, NA ~ 42,  other ~ copy), unlab(ab))
expect_identical(if_val(ab, NA ~ list(42), other ~ copy), unlab(ab))
expect_error(if_val(ab, NA ~ list(42,43,44), other ~ copy))

ab[[1]][1] = NA
ab[[2]][4] = NA

ab_no_na = ab

ab_no_na[[1]][1] = 42
ab_no_na[[2]][4] = 42
expect_identical(if_val(ab, NA ~ 42, other ~ copy), unlab(ab_no_na))

ab_no_na[[1]][1] = 42
ab_no_na[[2]][4] = 42
expect_error(if_val(ab, NA ~ list(42,43)))
expect_identical(if_val(ab, NA ~ list(42), other ~ copy), unlab(ab_no_na))
expect_error(if_val(ab, NA ~ list(42:39,40:43)))
expect_error(if_val(ab, NA ~ data.frame(42:39,40:43)))

context("if_val with NA help")

# simple case
a = c(NA, 2, 3, 4, NA)
if_val(a) = NA ~ 1
expect_identical(a, c(1, 2, 3, 4, 1))

# replacement with values from other variable
a = c(NA, 2, 3, 4, NA)
if_val(a) = NA ~ 1:5
expect_equal(a, 1:5)

# replacement with group means

# make data.frame 
set.seed(123)
group = sample(1:3, 30, replace = TRUE)
param = runif(30)
param[sample(30, 10)] = NA # place 10 NA's
df = data.frame(group, param)

df_clean =  modify(df, {
        param = if_val(param, NA ~ ave(param, group, FUN = mean_col), other ~ copy)
    })

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

df_res1 = df

mins = sapply(df, min, na.rm = TRUE)
maxs = sapply(df, max, na.rm = TRUE)
means = sapply(df, mean, na.rm = TRUE)

for(i in seq_along(df_res1)){
    df_res1[df_res1[,i]<0.25 & !is.na(df_res1[,i]),i] = mins[i]
    df_res1[df_res1[,i]>0.75 & !is.na(df_res1[,i]),i] = maxs[i]
    df_res1[is.na(df_res1[,i]),i] = means[i]
}

rownames(df_res1) = rownames(df)
expect_identical(
    if_val(df, lt(0.25) ~ t(min_col(df)), gt(0.75) ~ t(max_col(df)), NA ~ t(mean_col(df)), other ~ copy), 
    df_res1
)

df_res2 = df
rownames(df_res2) = rownames(df)
means = rowMeans(df, na.rm = TRUE)
for(i in seq_along(df_res2)){
    df_res2[is.na(df_res2[,i]),i] = means[is.na(df_res2[,i])]
}
expect_identical(
    if_val(df, NA ~ mean_row(df), other ~ copy), df_res2
)








df_test = df
df_test2 = df
if_val(df) = NA ~ t(colMeans(df, na.rm = TRUE))

df_test = within(df_test, {
    x1[is.na(x1)] = mean(x1, na.rm = TRUE)
    x2[is.na(x2)] = mean(x2, na.rm = TRUE)
    x3[is.na(x3)] = mean(x3, na.rm = TRUE)
})

expect_identical(df, df_test)

df = data.frame(x1, x2, x3)

if_val(df, from = NA) = list(as.list(colMeans(df, na.rm = TRUE)))
expect_identical(df, df_test)

# just for curiosity - assignment form doesn't work inside mutate
df_test2 = modify(df_test2, {
    if_val(x1, NA) = mean(x1, na.rm = TRUE)
    if_val(x2) = NA ~ mean(x2, na.rm = TRUE)
    if_val(x3) = NA ~ mean(x3, na.rm = TRUE)
    
})

expect_identical(df, df_test2)

context("if_val with NA add_val_lab")

a = 1:4
b = a
a[1] = NA
b[1] = 2
expect_identical(if_val(a, NA ~ c("Hard to say" = 2), other ~ copy), unlab(b))

set.seed(123)
x1 = runif(30)
x2 = runif(30)
x3 = runif(30)
x1[sample(30, 10)] = NA # place 10 NA's
x2[sample(30, 10)] = NA # place 10 NA's
x3[sample(30, 10)] = NA # place 10 NA's

df = data.frame(x1, x2, x3)
df_test = df

if_val(df) = NA ~ c("h/s" = 99)

df_test = within(df_test, {
    x1[is.na(x1)] = 99
    x2[is.na(x2)] = 99
    x3[is.na(x3)] = 99
})



expect_identical(df, df_test)


context("sum_if")
suppressWarnings(RNGversion("3.5.0"))


df1 = data.frame(
    a=c("apples",   "oranges",     "peaches",     "apples"),
    b = c(32, 54, 75, 86)
)

expect_equal(sum_if("apples", df1$a, data = df1$b), 118L)


expect_equal(sum_if(gt(55),df1$b),161L)

expect_equal(sum_if(function(x) x>55 | x==32,df1$b),193L)
expect_equal(sum_if(gt(55)|32,df1$b),193L)

expect_equal(sum_if(function(x) x>55 | x==54,df1$b),215L)
expect_equal(sum_if(gt(55)|54,df1$b),215L)

expect_equal(sum_if(ne(75),df1$b),172L)

expect_equal(sum_if(ge(32),df1$b),247L)


expect_equal(sum_if(gt(32) & lt(86),df1$b),54L + 75L)

expect_equal(sum_if(33:85,df1$b),54L + 75L)





context("sum_if complex criteria")
# more complex criteria
# values with letters
expect_equal(sum_if(function(x) grepl("^[A-z]+$",x),df1$a, data = df1$b), 247L)
expect_error(sum_if(function(x) grepl("^[A-z]+$",x),df1, data = df1))

# values that started on 'a'
expect_equal(sum_if(function(x) grepl("^a",x), df1$a, data = df1$b),118L)
expect_error(sum_if(eq("apples"), df1, data = df1))

context("sum_row_if")


expect_equal(sum_row_if(function(x) grepl("^a",x),df1$a, data = df1$b),c(32,0,0,86))




context("sum_col_if")




set.seed(123)
df2 = as.data.frame(
    matrix(sample(c(1:10,NA),30,replace = TRUE),10)
)
result = data.frame(
    exact=c(0,8,16,0,8,8,0,0,0,0),
    greater=c(10,9,0,10,0,10,0,10,0,0),
    range=c(0,13,21,7,8,8,12,7,7,6),
    na=c(0,0,0,0,0,0,0,0,0,0),
    not_na=c(14,22,21,17,10,19,15,18,15,8)
)

t_df2 = as.data.frame(t(df2))
expect_equal(
    with(t_df2, unname(sum_col_if(8,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$exact
)

expect_equal(
    with(t_df2, unname(sum_col_if(gt(8),V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$greater
)

expect_equal(
    with(t_df2, unname(sum_col_if(5:8,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$range
)

expect_equal(
    with(t_df2, unname(sum_col_if(is.na,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$na
)

expect_equal(
    with(t_df2, unname(sum_col_if(NA,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$na
)

expect_equal(
    with(t_df2, unname(sum_col_if(not_na,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$not_na
)


#########################

expect_equal(
    with(df2, unname(sum_col_if(8,V1,V2,V3))),
    c(0, 8, 32)
)


expect_equal(
    with(df2, unname(sum_col_if(gt(8),V1,V2,V3))),
    c(29, 10, 10)
)

expect_equal(
    with(df2, unname(sum_col_if(5:8,V1,V2,V3))),
    c(24, 20, 45)
)

expect_equal(
    with(df2, unname(sum_col_if(is.na,V1,V2,V3))),
    c(0, 0, 0)
)

expect_equal(
    with(df2, unname(sum_col_if(NA,V1,V2,V3))),
    c(0, 0, 0)
)


expect_equal(
    with(df2, unname(sum_col_if(not_na,V1,V2,V3))),
    unname(colSums(df2, na.rm = TRUE))
)

    

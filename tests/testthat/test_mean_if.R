context("mean_if")
df1 = data.frame(
    a=c("apples",   "oranges",     "peaches",     "apples"),
    b = c(32, 54, 75, 86)
)

expect_equal(mean_if("apples", df1$a, data = df1$b), 59)


expect_equal(mean_if(gt(55),df1$b),161/2)

expect_equal(mean_if(function(x) x>55 | x==32,df1$b),193/3)
expect_equal(mean_if(gt(55)|32,df1$b),193/3)

expect_equal(mean_if(function(x) x>55 | x==54,df1$b),215/3)
expect_equal(mean_if(gt(55)|54,df1$b),215/3)

expect_equal(mean_if(neq(75),df1$b),172/3)

expect_equal(mean_if(gte(32),df1$b),247/4)


expect_equal(mean_if(gt(32) & lt(86),df1$b),(54L + 75L)/2)

expect_equal(mean_if(33:85,df1$b), (54L + 75L)/2)





context("mean_if complex criteria")
# more complex criteria
# values with letters
expect_equal(mean_if(function(x) grepl("^[A-z]+$",x),df1$a, data = df1$b), 247/4)
expect_error(mean_if(function(x) grepl("^[A-z]+$",x),df1, data = df1))

# values that started on 'a'
expect_equal(mean_if(function(x) grepl("^a",x), df1$a, data = df1$b),118/2)
expect_error(mean_if(eq("apples"), df1, data = df1))

context("mean_row_if")


expect_equal(mean_row_if(function(x) grepl("^a",x),df1$a, data = df1$b),c(32,NaN,NaN,86))




# example with dplyr
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    set.seed(123)
    df2 = as.data.frame(
        matrix(sample(c(1:10,NA),30,replace = TRUE),10)
    )
    result = data.frame(df2,
                        exact=c(0,8,16,0,8,8,0,0,0,0)/c(0,1,2,0,1,1,0,0,0,0),
                        greater=c(10,9,0,10,0,10,0,10,0,0)/c(1,1,0,1,0,1,0,1,0,0),
                        range=c(0,13,21,7,8,8,12,7,7,6)/c(0,2,3,1,1,1,2,1,1,1),
                        na=c(0,0,0,0,0,0,0,0,0,0)/c(0,0,0,0,0,0,0,0,0,0),
                        not_na=c(14,22,21,17,10,19,15,18,15,8)/c(2,3,3,2,2,3,3,3,3,2)
                        )
    expect_equal(df2  %>% mutate(exact = mean_row_if(8,V1,V2,V3),
                    greater = mean_row_if(gt(8),V1,V2,V3),
                    range = mean_row_if(5:8,V1,V2,V3),
                    na = mean_row_if(is.na,V1,V2,V3),
                    not_na = mean_row_if(,V1,V2,V3)),
                     result)
} else {
	cat("dplyr not found\n")
}


context("mean_col_if")




set.seed(123)
df2 = as.data.frame(
    matrix(sample(c(1:10,NA),30,replace = TRUE),10)
)
result = data.frame(
    exact=c(0,8,16,0,8,8,0,0,0,0)/c(0,1,2,0,1,1,0,0,0,0),
    greater=c(10,9,0,10,0,10,0,10,0,0)/c(1,1,0,1,0,1,0,1,0,0),
    range=c(0,13,21,7,8,8,12,7,7,6)/c(0,2,3,1,1,1,2,1,1,1),
    na=c(0,0,0,0,0,0,0,0,0,0)/c(0,0,0,0,0,0,0,0,0,0),
    not_na=c(14,22,21,17,10,19,15,18,15,8)/c(2,3,3,2,2,3,3,3,3,2)
)

t_df2 = as.data.frame(t(df2))
expect_equal(
    with(t_df2, unname(mean_col_if(8,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$exact
)

expect_equal(
    with(t_df2, unname(mean_col_if(gt(8),V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$greater
)

expect_equal(
    with(t_df2, unname(mean_col_if(5:8,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$range
)

expect_equal(
    with(t_df2, unname(mean_col_if(is.na,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$na
)

expect_equal(
    with(t_df2, unname(mean_col_if(NA,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$na
)

expect_equal(
    with(t_df2, unname(mean_col_if(,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$not_na
)


#########################

expect_equal(
    with(df2, unname(mean_col_if(8,V1,V2,V3))),
    c(NaN, 8, 8)
)


expect_equal(
    with(df2, unname(mean_col_if(gt(8),V1,V2,V3))),
    c(29, 10, 10)/c(3, 1, 1)
)

expect_equal(
    with(df2, unname(mean_col_if(5:8,V1,V2,V3))),
    c(24, 20, 45)/c(4, 3, 6)
)

expect_equal(
    with(df2, unname(mean_col_if(is.na,V1,V2,V3))),
    c(NaN, NaN, NaN)
)

expect_equal(
    with(df2, unname(mean_col_if(NA,V1,V2,V3))),
    c(NaN, NaN, NaN)
)


expect_equal(
    with(df2, unname(mean_col_if(,V1,V2,V3))),
    unname(colMeans(df2, na.rm = TRUE))
)

    

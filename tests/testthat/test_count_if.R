context("count_if")
df1 = data.frame(
    a=c("apples",   "oranges",     "peaches",     "apples"),
    b = c(32, 54, 75, 86)
)

expect_equal(count_if("apples",df1$a),2L)

expect_equal(count_if("apples",df1),2L)

expect_equal(with(df1,count_if("apples",a,b)),2L)

expect_equal(with(df1,count_if(fixed("app"),a,b)),2L)
expect_equal(with(df1,count_if(regex("app"),a,b)),2L)
expect_equal(with(df1,count_if(perl("app"),a,b)),2L)
expect_equal(with(df1,count_if(perl("^app"),a,b)),2L)
expect_equal(with(df1,count_if(perl("app$"),a,b)),0L)

expect_equal(with(df1,count_if(!perl("app"),a,b)),6L)
expect_equal(with(df1,count_if(perl("app") | regex("oranges"),a,b)),3L)

expect_equal(count_if(gt(55),df1$b),2L)

expect_equal(count_if(function(x) x>55 | x==32,df1$b),3L)
expect_equal(count_if(gt(55)|32,df1$b),3L)

expect_equal(count_if(function(x) x>55 | x==54,df1$b),3L)
expect_equal(count_if(gt(55)|54,df1$b),3L)

expect_equal(count_if(neq(75),df1$b),3L)

expect_equal(count_if(gte(32),df1$b),4L)

expect_equal(count_if(list(gt(32), lt(86)),df1$b),2L)
expect_equal(count_if(gt(32) & lt(86),df1$b),2L)

expect_equal(count_if(33:85,df1$b),2L)

context("count_if character")

expect_equal(count_if("a", letters),1L)
expect_equal(count_if(gt("a"), letters),25L)
expect_equal(count_if(gte("b"), letters),25L)
expect_equal(count_if(neq("b"), letters),25L)
expect_equal(count_if(eq("b"), letters),1L)
# expect_warning(count_if(c(eq(b),"d"), letters))
expect_equal(count_if(c("a","b"), letters),2L)

context("count_ifs")

df3 = data.frame( 
    "Sales Person" = c("Davidoski", "Burke", "Sundaram", "Levitan"),
    "Exceeded Widgets Quota" = c("Yes", "Yes", "Yes", "No"),
    "Exceeded Gadgets Quota" = c("No", "Yes", "Yes", "Yes"),
    "Exceeded Doodads Quota" = c("No", "No", "Yes", "Yes")
)

expect_equal(count_if("Yes", df3[1,2:4]), 1) # Counts how many times Davidoski exceeded a sales quota for Widgets, Gadgets, and Doodads.



context("count_if complex criteria")
# more complex criteria
# values with letters
expect_equal(count_if(function(x) grepl("^[A-z]+$",x),df1),4L)
expect_equal(count_if(perl("^[A-z]+$"),df1),4L)
expect_equal(count_if(regex("[:alpha:]"),df1),4L)

# values that started on 'a'
expect_equal(count_if(function(x) grepl("^a",x),df1),2L)
expect_equal(count_if(perl("^a"),df1),2L)

context("count_row_if")


expect_equal(count_row_if(function(x) grepl("^a",x),df1),c(1,0,0,1))

expect_equal(df1 %has% 'apples',c(TRUE,FALSE,FALSE,TRUE))

# example with dplyr
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    set.seed(123)
    df2 = as.data.frame(
        matrix(sample(c(1:10,NA),30,replace = TRUE),10)
    )
    result = data.frame(df2,
                        exact=c(0,1,2,0,1,1,0,0,0,0),
                        greater=c(1,1,0,1,0,1,0,1,0,0),
                        range=c(0,2,3,1,1,1,2,1,1,1),
                        na=c(1,0,0,1,1,0,0,0,0,1),
                        not_na=c(2,3,3,2,2,3,3,3,3,2)
                        )
    expect_equal(df2  %>% mutate(exact = count_row_if(8,V1,V2,V3),
                    greater = count_row_if(gt(8),V1,V2,V3),
                    range = count_row_if(5:8,V1,V2,V3),
                    na = count_row_if(is.na,V1,V2,V3),
                    not_na = count_row_if(,V1,V2,V3)),
                     result)
}


context("count_col_if")


expect_equal(count_col_if(function(x) grepl("^a",x),t(df1)),c(X1 = 1, X2 = 0, X3 = 0, X4 = 1))
expect_equal(count_col_if(perl("^a"),t(df1)),c(X1 = 1, X2 = 0, X3 = 0, X4 = 1))
expect_equal(count_col_if(function(x) grepl("^a",x),df1),c(a = 2, b = 0))
expect_equal(count_col_if(perl("^a"),df1),c(a = 2, b = 0))

set.seed(123)
df2 = as.data.frame(
    matrix(sample(c(1:10,NA),30,replace = TRUE),10)
)
result = data.frame(exact=c(0,1,2,0,1,1,0,0,0,0),
                    greater=c(1,1,0,1,0,1,0,1,0,0),
                    range=c(0,2,3,1,1,1,2,1,1,1),
                    na=c(1,0,0,1,1,0,0,0,0,1),
                    not_na=c(2,3,3,2,2,3,3,3,3,2)
)

t_df2 = as.data.frame(t(df2))
expect_equal(
    with(t_df2, unname(count_col_if(8,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$exact
)

expect_equal(
    with(t_df2, unname(count_col_if(gt(8),V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$greater
)

expect_equal(
    with(t_df2, unname(count_col_if(5:8,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$range
)

expect_equal(
    with(t_df2, unname(count_col_if(is.na,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$na
)

expect_equal(
    with(t_df2, unname(count_col_if(NA,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$na
)

expect_equal(
    with(t_df2, unname(count_col_if(,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
    result$not_na
)


#########################

expect_equal(
    with(df2, unname(count_col_if(8,V1,V2,V3))),
    c(0, 1, 4)
)


expect_equal(
    with(df2, unname(count_col_if(gt(8),V1,V2,V3))),
    c(3, 1, 1)
)

expect_equal(
    with(df2, unname(count_col_if(5:8,V1,V2,V3))),
    c(4, 3, 6)
)

expect_equal(
    with(df2, unname(count_col_if(is.na,V1,V2,V3))),
    c(1, 2, 1)
)

expect_equal(
    with(df2, unname(count_col_if(NA,V1,V2,V3))),
    c(1, 2, 1)
)


expect_equal(
    with(df2, unname(count_col_if(,V1,V2,V3))),
    c(9, 8, 9)
)

    

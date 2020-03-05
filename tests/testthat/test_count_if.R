context("count_if")
suppressWarnings(RNGversion("3.5.0"))
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

expect_equal(count_if(ne(75),df1$b),3L)

expect_equal(count_if(ge(32),df1$b),4L)


expect_equal(count_if(gt(32) & lt(86), df1$b),2L)
expect_equal(count_if(thru(35, 80), df1$b),2L)
expect_equal(count_if(35 %thru% 80, df1$b),2L)

expect_equal(count_if(33:85,df1$b),2L)

context("count_if character")

expect_equal(count_if("a", letters),1L)
expect_equal(count_if(gt("a"), letters),25L)
expect_equal(count_if(gte("b"), letters),25L)
expect_equal(count_if(ge("b"), letters),25L)
expect_equal(count_if(neq("b"), letters),25L)
expect_equal(count_if(ne("b"), letters),25L)
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


expect_equal(df1 %row_in% 'apples', c(TRUE,FALSE,FALSE,TRUE))
expect_equal(df1 %has% 'apples', c(TRUE,FALSE,FALSE,TRUE))
expect_equal(df1 %col_in% 'apples', c(a = TRUE, b = FALSE))



context("count_col_if")


expect_equal(count_col_if(function(x) grepl("^a",x),t(df1)),
             c(1, 0, 0, 1))
expect_equal(count_col_if(perl("^a"),t(df1)),
             c(1, 0, 0, 1))
expect_equal(count_col_if(function(x) grepl("^a",x),df1),
             c(a = 2, b = 0))
expect_equal(count_col_if(perl("^a"),df1),
             c(a = 2, b = 0))

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
    with(t_df2, unname(count_col_if(not_na,V1,V2,V3, V4, V5, V6, V7, V8, V9, V10))),
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
    with(df2, unname(count_col_if(not_na,V1,V2,V3))),
    c(9, 8, 9)
)

context("count_if new examples")
set.seed(123)
dfs = as.data.frame(
       matrix(sample(c(1:10,NA),30,replace = TRUE),10)
)

result  = modify(dfs, {
                   exact = count_row_if(8, V1, V2, V3)
                   greater = count_row_if(gt(8), V1, V2, V3)
                   range = count_row_if(5:8, V1, V2, V3)
                   na = count_row_if(is.na, V1, V2, V3)
                   not_na = count_row_if(not_na, V1, V2, V3)
                })  

expect_equal(
    mean_row_if(6, dfs$V1, data = dfs),
    ifelse(dfs$V1==6, apply(dfs, 1, mean, na.rm = TRUE), NaN)
)
expect_identical(
    median_row_if(gt(2), dfs$V1, dfs$V2, dfs$V3),
    apply(ifelse(as.matrix(dfs)>2, as.matrix(dfs), NA) , 1, median, na.rm = TRUE)
)
expect_identical(
    sd_row_if(5 %thru% 8, dfs$V1, dfs$V2, dfs$V3),
    apply(ifelse((5 %thru% 8)(as.matrix(dfs)), as.matrix(dfs), NA) , 1, sd, na.rm = TRUE)
)

if_na(dfs) = 5 # replace NA 

# custom apply
expect_identical(
    apply_col_if(prod, gt(2), dfs$V1, data = dfs),
    apply(na_if(dfs, when(dfs$V1<3)), 2, prod, na.rm = TRUE)
)
expect_identical(
    apply_row_if(prod, gt(2), dfs$V1, data = dfs),
    ifelse(dfs$V1>2, apply(dfs, 1, prod, na.rm = TRUE), 1)
)
# apply_row_if(prod, gt(2), dfs$V1, data = dfs) # product of all elements




### issue #47

expect_equal(count_if(TRUE, c(TRUE, FALSE), TRUE), 3)






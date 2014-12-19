context("countif")
df1 = data.frame(
    a=c("apples",   "oranges",     "peaches",     "apples"),
    b = c(32, 54, 75, 86)
)

expect_equal(countif("apples",df1$a),2L)

expect_equal(countif("apples",df1),2L)

expect_equal(with(df1,countif("apples",a,b)),2L)

expect_equal(countif(crit(">",55),df1$b),2L)
expect_equal(countif(crit(">",55) | 32,df1$b),3L)
expect_equal(countif(54 | crit(">",55),df1$b),3L)

expect_equal(countif(crit("!=",75),df1$b),3L)

expect_equal(countif(crit(">=",32),df1$b),4L)

expect_equal(countif(crit(">",32) & crit("<",86),df1$b),2L)

expect_equal(countif(33:85,df1$b),2L)

# more complex criteria
# values with letters
expect_equal(countif(function(x) grepl("^[A-z]+$",x),df1),4L)

# values that started on 'a'
expect_equal(countif(function(x) grepl("^a",x),df1),2L)

context("row_countif")


expect_equal(row_countif(function(x) grepl("^a",x),df1),c(1,0,0,1))

expect_equal(df1 %has% 'apples',c(TRUE,FALSE,FALSE,TRUE))

# example with dplyr
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
expect_equal(df2  %>% mutate(exact = row_countif(8,V1,V2,V3),
                greater = row_countif(crit(">",8),V1,V2,V3),
                range = row_countif(5:8,V1,V2,V3),
                na = row_countif(is.na,V1,V2,V3),
                not_na = row_countif(,V1,V2,V3))
                , result)

context("count_if")
df1 = data.frame(
    a=c("apples",   "oranges",     "peaches",     "apples"),
    b = c(32, 54, 75, 86)
)

expect_equal(count_if("apples",df1$a),2L)

expect_equal(count_if("apples",df1),2L)

expect_equal(with(df1,count_if("apples",a,b)),2L)

expect_equal(count_if(">55",df1$b),2L)
expect_equal(count_if(function(x) x>55 | x==32,df1$b),3L)
expect_equal(count_if(function(x) x>55 | x==54,df1$b),3L)

expect_equal(count_if("!=75",df1$b),3L)

expect_equal(count_if(">=32",df1$b),4L)

expect_equal(count_if(list(">32", "<86"),df1$b),2L)

expect_equal(count_if(33:85,df1$b),2L)

context("count_ifs")

df3 = data.frame( 
    "Sales Person" = c("Davidoski", "Burke", "Sundaram", "Levitan"),
    "Exceeded Widgets Quota" = c("Yes", "Yes", "Yes", "No"),
    "Exceeded Gadgets Quota" = c("No", "Yes", "Yes", "Yes"),
    "Exceeded Doodads Quota" = c("No", "No", "Yes", "Yes")
)

expect_equal(count_if("Yes", df3[1,2:4]), 1) # Counts how many times Davidoski exceeded a sales quota for Widgets, Gadgets, and Doodads.



context("complex criteria")
# more complex criteria
# values with letters
expect_equal(count_if(function(x) grepl("^[A-z]+$",x),df1),4L)

# values that started on 'a'
expect_equal(count_if(function(x) grepl("^a",x),df1),2L)

context("row_count_if")


expect_equal(row_count_if(function(x) grepl("^a",x),df1),c(1,0,0,1))

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
expect_equal(df2  %>% mutate(exact = row_count_if(8,V1,V2,V3),
                greater = row_count_if(">8",V1,V2,V3),
                range = row_count_if(5:8,V1,V2,V3),
                na = row_count_if(is.na,V1,V2,V3),
                not_na = row_count_if(,V1,V2,V3))
                , result)

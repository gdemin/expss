context("sngl")

library(dplyr)
data(ProductTestRaw)

## Example 1 ##

# 4 errors: 2 missing, 2 invalid codes
expect_equal_to_reference(ProductTestRaw  %>% sngl(s2b)(2,3), "sngl1.rds")

expect_equal_to_reference(ProductTestRaw  %>% check_subset(cell %in% 2) %>% sngl(s2b)(2,3), "sngl2.rds") 


## Example 2 ##

data(codeframe)
valid_a1 = make_labels(codeframe$likes)

# Exclusive values
# 1 Liked everything
# 2 Disliked everything
# 99 Hard to say

expect_equal_to_reference(ProductTestRaw  %>% mult(a1_1:a1_6)(valid_a1,exclusive=c(1,2,99)),
                          "mult1.rds") 
expect_equal_to_reference(ProductTestRaw %>% check_subset(cell %in% 2)  %>% mult(a1_1:a1_6)(valid_a1,exclusive=c(1,2,99)),
                          "mult2.rds")

# 5 errors: 1 missing value, 1 invalid code, 1 code duplication, 2 non-exclusive values
expect_equal_to_reference(ProductTestRaw %>%  mult(a1_1:a1_6)(valid_a1,exclusive=c(1,2,99)),
                          "mult3.rds")            

## Example 3 ##

# question a4 was asked only if codes 1-4 marked in a3
# 3 errors: 1 missing value, 1 invalid code, 1 code in case of a3 in 5-7.
valid_a4 = make_labels(codeframe$dislikes_in_appearance)
expect_equal_to_reference(ProductTestRaw  %>% check_if(a3 %in% 1:4) %>% mult(a4_1:a4_6)(valid_a4,exclusive=99),
                          "mult4.rds")
expect_equal_to_reference(ProductTestRaw %>% check_subset(cell %in% 2) %>% check_if(a3 %in% 1:4) %>% mult(a4_1:a4_6)(valid_a4,exclusive=99), 
                          "mult5.rds")



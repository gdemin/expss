context("check_internal")

library(dplyr)
data(ProductTestRaw)

## Example 1 ##

# 4 errors: 2 missing, 2 invalid codes
expect_equal_to_reference(check_internal(ProductTestRaw$s2b,values=2:3),
                          "check1.rds")

## Example 2 ##

data(codeframe)
valid_a1 = make_labels(codeframe$likes)

# Exclusive values
# 1 Liked everything
# 2 Disliked everything
# 99 Hard to say

# 5 errors: 1 missing value, 1 invalid code, 1 code duplication, 2 non-exclusive values
expect_equal_to_reference(check_internal(select(ProductTestRaw,a1_1:a1_6),values=valid_a1,mult = TRUE, exclusive=c(1,2,99)),
                          "check2.rds")   

# 5 errors: 1 missing value, 1 invalid code, 1 code duplication, 2 non-exclusive values
# test for including exclusive in valid values
expect_equal_to_reference(check_internal(select(ProductTestRaw,a1_1:a1_6),values=valid_a1[-(1:2)],mult = TRUE, exclusive=c(1,2,99)),
                          "check2.rds")  

## Example 3 ##

valid_a4 = make_labels(codeframe$dislikes_in_appearance)
# question a4 was asked only if codes 1-4 marked in a3
# 3 errors: 1 missing value, 1 invalid code, 1 code in case of a3 in 5-7.
expect_equal_to_reference(check_internal(select(ProductTestRaw,a4_1:a4_6),values=valid_a4,mult = TRUE, exclusive=99, cond = ProductTestRaw$a3 %in%  1:4),
                          "check3.rds")

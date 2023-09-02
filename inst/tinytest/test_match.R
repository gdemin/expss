context("match")

a = as.data.frame(matrix(1:9, ncol = 3))

expect_equal(match_row(1, a), c(1, NA, NA))
expect_equal(match_col(1, a), c(V1 = 1, V2 = NA, V3 = NA))


expect_equal(match_row(c(1, 5, 9), a), c(1, 2, 3))
expect_equal(match_col(c(1, 5, 9), a), c(V1 = 1, V2 = 2, V3 = 3))

expect_equal(match_row(gt(2), a), c(2, 2, 1))
expect_equal(match_col(gt(2), a), c(V1 = 3, V2 = 1, V3 = 1))

V1 = 1:3
V2 = 3:1
V3 = 9:7

expect_equal(match_row(1, V1, V2, V3), c(1, NA, 2))
expect_equal(match_col(1, V1, V2, V3), c(1, 3, NA))


expect_equal(match_row(c(3, 2, 1), V1, V2, V3), c(1, 1, 1))
expect_equal(match_row(eq(c(3, 2, 1)), V1, V2, V3), c(2, 1, 2))

expect_equal(match_col(c(2, 9, 8), V1, V2, V3), c(2, 2, 1))

expect_equal(match_row(gt(2), V1, V2, V3), c(2, 3, 1))
expect_equal(match_col(gt(2), V1, V2, V3), c(3, 1, 1))


context("index")

a = as.data.frame(matrix(1:9, ncol = 3))

expect_equal(index_row(1, a), c(1, 2, 3))
expect_equal(index_col(1, a), c(V1=1, V2 = 4, V3 = 7))

expect_equal(index_row(NA, a), 1*c(NA, NA, NA))
expect_equal(index_col(NA, a), 1*c(V1 = NA, V2 = NA, V3 = NA))


expect_equal(index_row(c(1, NA, 2), a), c(1, NA, 6))
expect_equal(index_col(c(1, 3, NA), a), c(V1 = 1, V2 = 6, V3 = NA))

V1 = 1:3
V2 = 4:6
V3 = 7:9

expect_equal(index_row(1,  V1, V2, V3), c(1, 2, 3))
expect_equal(index_col(1,  V1, V2, V3), c(1, 4, 7))

expect_equal(index_row(NA, V1, V2, V3), 1*c(NA, NA, NA))
expect_equal(index_col(NA, V1, V2, V3), 1*c(NA, NA, NA))


expect_equal(index_row(c(1, NA, 2),  V1, V2, V3), c(1, NA, 6))
expect_equal(index_col(c(1, 3, NA),  V1, V2, V3), c(1, 6, NA))

expect_error(index_row(1:2,  V1, V2, V3))
expect_error(index_col(1:2,  V1, V2, V3))

expect_error(index_row(1:5,  V1, V2, V3))
expect_error(index_col(1:5,  V1, V2, V3))

expect_error(index_row(NULL, V1, V2, V3))
expect_error(index_col(NULL, V1, V2, V3))

expect_error(index_row(numeric(0), V1, V2, V3))
expect_error(index_col(numeric(0), V1, V2, V3))

context("match_examples")

v1 = 1:3
v2 = 2:4
v3 = 7:5

# postions of 1,3,5 in rows
expect_equal(match_row(eq(c(1, 3, 5)), v1, v2, v3),1:3)
# postions of 1,3,5 in columnss
expect_equal(match_col(1, v1, v2, v3), c(1, NA, NA))

# postion of first value greater than 2
ix = match_row(gt(2), v1, v2, v3) 
expect_equal(ix, c(3,2,1))
# return values by result of previous 
expect_equal(index_row(ix, v1, v2, v3), c(7,3,3))

# the same actions with data.frame
dfs = data.frame(v1, v2, v3)

# postions of 1,3,5 in rows
expect_equal(match_row(eq(c(1, 3, 5)), dfs), 1:3)
# postions of 1,3,5 in columnss
expect_equal(match_col(1, dfs), c(v1 = 1, v2 = NA, v3 = NA))

# postion of first value greater than 2
ix = match_row(gt(2), dfs) 
expect_equal(ix, c(3,2,1))
# return values by result of previous 
expect_equal(index_row(ix, dfs), c(7,3,3))


df = sheet(v1 = c(11, 12, 11, 12), v2 = c(12, 11, 12, 11), v3 = NA)

expect_equal(match_row(is_max, df), c(2, 1, 2, 1))
expect_equal(match_row(is_min, df), c(1, 2, 1, 2))

expect_equal(index_row(match_row(is_max, df), df), c(12, 12, 12, 12))
expect_equal(index_row(match_row(is_min, df), df), c(11, 11, 11, 11))

expect_equal(value_row_if(is_max, df), c(12, 12, 12, 12))
expect_equal(value_row_if(is_min, df), c(11, 11, 11, 11))


expect_equal(value_col_if(is_max, df), c(v1 = 12, v2 = 12, v3 = NA))
expect_equal(value_col_if(is_min, df), c(v1 = 11, v2 = 11, v3 = NA))


df = sheet(v1 = c(11, 12, 11, 12), v2 = c(11, 11, 12, 11), v3 = NA)

expect_equal(match_row(is_max, df), c(NA, 1, 2, 1))
expect_equal(match_row(eq(max_row(df$v1, df$v2, df$v3)), df), c(1, 1, 2, 1))
expect_equal(match_row(eq(min_row(df$v1, df$v2, df$v3)), df), c(1, 2, 1, 2))

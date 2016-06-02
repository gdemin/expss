context("set_na vector")

a = 1:5

a[a>3] = NA

expect_equal(set_na(1:5, gt(3)),a)
expect_equal(set_na(1:5, 4:5),a)
expect_equal(set_na(1:5, c(FALSE,FALSE,FALSE,TRUE,TRUE)),a)


context("set_na data.frame")
a = data.frame(a=1:5,b=5:1)

b = a
b[a$a>3,"a"] = NA
b[a$b>3,"b"] = NA
expect_equal(set_na(a, gt(3)),b)
expect_equal(set_na(a, 4:5),b)

cond = cbind(a$a>3, a$b>3)
expect_equal(set_na(a, cond),b)

b = a
b[3,"b"] = NA
b[1:5,"a"] = NA
expect_equal(set_na(a, eq(a$a)),b)

set.seed(123)

aa = matrix(rnorm(50), ncol = 2)
bb = aa
bb[bb[,1] == max(bb[,1]),1] = NA
bb[bb[,2] == max(bb[,2]),2] = NA
# we set to NA maximum values in each column
expect_identical(set_na(aa, as.list(max_col(aa))), bb)
expect_error(set_na(aa, aa))
expect_error(set_na(aa, as.data.frame(aa)))

b = a
b[1:2,] = NA

expect_equal(set_na(a, c(TRUE, TRUE, FALSE,FALSE,FALSE)),b)
expect_equal(set_na(a, as.data.frame(c(TRUE, TRUE, FALSE,FALSE,FALSE))),b)
expect_equal(set_na(list(a,1:5), c(TRUE, TRUE, FALSE,FALSE,FALSE)),list(b,c(NA,NA,3,4,5)))

b = a
b[,1] = NA
b$a = as.numeric(b$a)

expect_equal(set_na(a, t(c(TRUE, FALSE))),b)
# expect_equal(set_na(a, list(TRUE, FALSE)),b)
expect_equal(set_na(a, as.data.frame(t(c(TRUE, FALSE)))),b)

context("set_na tbl_df")
if(suppressWarnings(require(dplyr, quietly = TRUE))){

    
    a = as.tbl(data.frame(a=1:5,b=5:1))
    
    b = a
    b[a$a>3,"a"] = NA
    b[a$b>3,"b"] = NA
    expect_equal(set_na(a, gt(3)),b)
    expect_equal(set_na(a, 4:5),b)
    
    cond = cbind(a$a>3, a$b>3)
    expect_equal(set_na(a, cond),b)
    
    b = a
    b[1:2,] = NA
    
    expect_equal(set_na(a, c(TRUE, TRUE, FALSE,FALSE,FALSE)),b)
    
    
    b = a
    b[,1] = NA
    b$a = as.integer(b$a)
    
    expect_equal(set_na(a, t(c(TRUE, FALSE))),b)
} else {
	cat("dplyr not found\n")
}
context("set_na matrix")

a = cbind(a=1:5,b=5:1)

b = a
b[a[,1]>3,"a"] = NA
b[a[,2]>3,"b"] = NA
expect_equal(set_na(a, gt(3)),b)
expect_equal(set_na(a, 4:5),b)

cond = cbind(a[,1]>3, a[,2]>3)
expect_equal(set_na(a, cond),b)

b = a
b[1:2,] = NA

expect_equal(set_na(a, c(TRUE, TRUE, FALSE,FALSE,FALSE)),b)

b = a
b[,1] = NA


expect_equal(set_na(a, t(c(TRUE, FALSE))),b)


a = c(1:5,99)
a1 = a
a1[a1==99] = NA
expect_identical(set_na(a, 99), a1)
expect_identical(set_na(a, gt(5)), a1)


set.seed(123)

dfs = data.frame(
      a = c("bad value", "bad value", "good value", "good value", "good value"),
      b = runif(5)
)

dfs1 = dfs
dfs1[1:2, ] = NA

expect_identical(set_na(dfs, dfs$a=="bad value"), dfs1)

a = rnorm(50)
a1 = a
a1[a1< -1 | a1>1] = NA

expect_identical(set_na(a, lt(-1) | gt(1)), a1)

dfs = data.frame(
    a = rnorm(50),
    b = rnorm(50)
)





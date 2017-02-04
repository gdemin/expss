context("modify")
aa = 10 %r% 5
a_ = 20 %r% 5
a_1 = 1 %r% 5
a_2 = 2 %r% 5
a_4 = 4 %r% 5
a_5 = 5 %r% 5

b_1 = -1 %r% 5
b_2 = -2 %r% 5
b_4 = -4 %r% 5
b_5 = -5 %r% 5 

dfs = data.frame(
    zz = 42,
    b_3 = 44,
    aa = 10 %r% 5,
    b_ = 20 %r% 5,
    b_1 = 11 %r% 5,
    b_2 = 12 %r% 5,
    b_4 = 14 %r% 5,
    b_5 = 15 %r% 5 
)

expect_error(modify(dfs, {.n = 42}))
expect_error(modify(dfs, {.N = 42}))
expect_error(modify(dfs, {set = 42}))



result_dfs = dfs
result_dfs$b_total = with(dfs, sum_row(b_1, b_2, b_4, b_5))
set.seed(1)
result_dfs$b_total = with(dfs, sum_row(b_1, b_2, b_4, b_5))
result_dfs$random_numer = runif(nrow(dfs))

dfs2 = dfs
dfs2$test = 1:5

result_dfs2 = dfs2
# result_dfs2$a_total = ifelse(dfs2$test %in% 2:4, sum_row(a_1, a_2, a_4, a_5), NA)
result_dfs2$b_total = ifelse(dfs2$test %in% 2:4, with(dfs, sum_row(b_1, b_2, b_4, b_5)), NA)
result_dfs2$aa = ifelse(dfs2$test %in% 2:4, result_dfs2$aa+1, result_dfs2$aa)

expect_identical(calc(dfs, sum_row(b_1 %to% b_5)), result_dfs$b_total)

default_dataset(dfs)
expect_identical(.calc(sum_row(b_1 %to% b_5)), result_dfs$b_total)

expect_identical(dfs %calc% sum_row(b_1 %to% b_5), result_dfs$b_total)
set.seed(1)
expect_identical(calc(dfs, runif(.N)), result_dfs$random_numer)

set.seed(1)
expect_identical(
    modify(dfs, {
        b_total = sum_row(vars_range("b_1", "b_5"))
        random_numer = runif(.n)
    }), 
    result_dfs
)

set.seed(1)
expect_identical(
    compute(dfs, {
        b_total = sum_row(vars_range("b_1", "b_5"))
        random_numer = runif(.n)
    }), 
    result_dfs
)

set.seed(1)
expect_identical(
    modify(dfs, {
        b_total = sum_row(vars_range("b_1", "b_5"))
        random_numer = runif(.N)
    }), 
    result_dfs
)

# Bad number of rows: 'random_numer' has 2 rows instead of 5 rows. 
expect_error(
    modify(dfs, {
        random_numer = runif(2)
    })
)


set.seed(1)
expect_identical(
    dfs %modify% {
        b_total = sum_row(vars_range("b_1", "b_5"))
        random_numer = runif(.n)
    }, 
    result_dfs
)


set.seed(1)
expect_identical(
    dfs %compute% {
        b_total = sum_row(vars_range("b_1", "b_5"))
        random_numer = runif(.n)
    }, 
    result_dfs
)


set.seed(1)
expect_identical(
    dfs %modify% {
        b_total = sum_row(vars_range("b_1", "b_5"))
        random_numer = runif(.N)
    }, 
    result_dfs
)

result_dfs$random_numer = NULL
expect_identical(
    modify(dfs, {
        b_total = sum_row(b_1 %to% b_5)
    }), 
    result_dfs
)

result_dfs$random_numer = NULL
expect_identical(
    dfs %modify% {
        b_total = sum_row(b_1 %to% b_5)
    }, 
    result_dfs
)

context("modify magrittr")
if(suppressWarnings(require(magrittr, quietly = TRUE))){
    expect_identical(
        dfs %>% modify( {
            b_total = sum_row(vars_range("b_1", "b_5"))
        }), 
        result_dfs
    )
    
    expect_identical(
        dfs %>% modify( {
            b_total = sum_row(b_1 %to% b_5)
        }), 
        result_dfs
    )
    
} else {
    cat("magrittr not found\n")
}

# example with dplyr
context("modify dplyr")
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    
    expect_identical(
        tbl_df(dfs) %>% modify( {
            b_total = rowSums(vars_range("b_1", "b_5"))
        }), 
        tbl_df(result_dfs)
    )
    
    expect_identical(
        tbl_df(dfs) %>% modify( {
            b_total = sum_row(b_1 %to% b_5)
        }), 
        tbl_df(result_dfs)
    )
    
    
} else {
    cat("dplyr not found\n")
}

context("modify_if")
set.seed(1)

result_dfs2[result_dfs2$test %in% 2:4, "random_numer"] = runif(3) 
set.seed(1)
expect_identical(
    modify_if(dfs2, test %in% 2:4,
              {
                  b_total = sum_row(b_1 %to% b_5)
                  aa = aa + 1
                  random_numer = runif(.n)
              }), 
    result_dfs2
)


set.seed(1)
expect_identical(
    modify_if(dfs2, 2:4,
              {
                  b_total = sum_row(b_1 %to% b_5)
                  aa = aa + 1
                  random_numer = runif(.n)
              }), 
    result_dfs2
)


set.seed(1)
expect_identical(
    modify_if(dfs2, test %in% 2:4,
              {
                  b_total = sum_row(b_1 %to% b_5)
                  aa = aa + 1
                  random_numer = runif(.N)
              }), 
    result_dfs2
)


set.seed(1)
expect_identical(
    do_if(dfs2, test %in% 2:4,
              {
                  b_total = sum_row(b_1 %to% b_5)
                  aa = aa + 1
                  random_numer = runif(.N)
              }), 
    result_dfs2
)


expect_error(
    modify_if(dfs2, test %in% 2:4,
              {
                  a_total = sum_row(a_1 %to% a_5)
                  b_total = sum_row(b_1 %to% b_5)
                  aa = aa + 1
              })
)
expect_error(modify_if(dfs2,test %in% 2:4,  {.n = 42}))
expect_error(modify_if(dfs2,test %in% 2:4,  {.N = 42}))
expect_error(modify_if(dfs2,test %in% 2:4,  {set = 42}))


result_dfs2$random_numer = NULL
context("modify_if dplyr")
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    expect_identical(
        modify_if(tbl_df(dfs2), test %in% 2:4,
                  {
                      b_total = sum_row(b_1 %to% b_5)
                      aa = aa + 1
                  }), 
        tbl_df(result_dfs2)
    )
    
    expect_error(
        modify_if(tbl_df(dfs2), test %in% 2:4,
                  {
                      a_total = sum_row(a_1 %to% a_5)
                      b_total = sum_row(b_1 %to% b_5)
                      aa = aa + 1
                  })
    )
    
} else {
    cat("dplyr not found\n")
}


result_dfs2$b_total = NULL
expect_identical(
    modify_if(dfs2, test %in% 2:4,
              {
                  b_total = NULL
                  aa = aa + 1
              }), 
    result_dfs2
)


expect_identical(
    modify_if(dfs2, 2:4,
              {
                  b_total = NULL
                  aa = aa + 1
              }), 
    result_dfs2
)

context("modify labels")

result_dfs3 = dfs
result_dfs3$b_total = with(dfs, sum_row(b_1, b_2, b_4, b_5))
var_lab(result_dfs3$aa) = "my label"
val_lab(result_dfs3$aa) = c(one = 1, two = 2)

expect_identical(
    modify(dfs, {
        b_total = sum_row(b_1, b_2, b_4, b_5)
        var_lab(aa) = "my label"
        val_lab(aa) = c(one = 1, two = 2)
        
        
    }),
    result_dfs3
)

expect_identical(
    dfs %modify% {
        b_total = sum_row(b_1, b_2, b_4, b_5)
        var_lab(aa) = "my label"
        val_lab(aa) = c(one = 1, two = 2)
        
        
    },
    result_dfs3
)


result_dfs3$b_total = NULL
result_dfs3$aa = 1
expect_identical(
    modify(dfs, {
        b_total = NULL
        aa = 1
        
        
    }),
    result_dfs3
)

# doesn't work
# result_dfs3$b_total = NULL
# expect_identical(
#     modify_if(dfs, 1:5 %in% 2:4, {
#         
#         var_lab(aa) = "my label"
#         val_lab(aa) = c(one = 1, two = 2)
# 
#     }),
#     result_dfs3
# )

data(iris)

iris2 = iris

iris2 = modify(iris2, {Species = NULL})

expect_identical(iris2, iris[,-5])

iris2 = iris

iris2 = iris2 %modify% {Species = NULL}

expect_identical(iris2, iris[,-5])

iris2 = iris

iris2 = modify_if(iris2, Sepal.Length<5, {Species = NULL})

expect_identical(iris2, iris[,-5])


#### 
context("modify.list/modify_if.list")

data(iris)
iris_list = split(iris, iris$Species)


res = lapply(iris_list, function(dfs) {dfs$aggr = sum(dfs$Sepal.Length); dfs})

res0 = modify(iris_list, {aggr = sum(Sepal.Length)})
expect_identical(res0, res)

res_calc = calc(iris_list, sum(Sepal.Length))
expect_identical(res_calc, lapply(iris_list, function(dfs){sum(dfs$Sepal.Length)}))



res = lapply(iris_list, function(dfs) {
    dfs$aggr = ifelse(dfs$Sepal.Width>mean(dfs$Sepal.Width), 1, NA)
    dfs
    })

res0 = modify_if(iris_list, Sepal.Width>mean(Sepal.Width), {aggr = 1})
expect_identical(res0, res)
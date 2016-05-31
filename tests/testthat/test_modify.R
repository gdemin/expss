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
    aa = 10 %r% 5,
    b_ = 20 %r% 5,
    b_1 = 11 %r% 5,
    b_2 = 12 %r% 5,
    b_4 = 14 %r% 5,
    b_5 = 15 %r% 5 
)

result_dfs = dfs
result_dfs$a_total = sum_row(a_1, a_2, a_4, a_5)
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

set.seed(1)
expect_identical(
    modify(dfs, {
        a_total = sum_row(get_var_range("a_1", "a_5"))
        b_total = sum_row(get_var_range("b_1", "b_5"))
        random_numer = runif(.n)
    }), 
    result_dfs
)


result_dfs$random_numer = NULL
expect_identical(
    modify(dfs, {
        a_total = sum_row(a_1 %to% a_5)
        b_total = sum_row(b_1 %to% b_5)
    }), 
    result_dfs
)

context("modify magrittr")
if(suppressWarnings(require(magrittr, quietly = TRUE))){
    expect_identical(
        dfs %>% modify( {
            a_total = sum_row(get_var_range("a_1", "a_5"))
            b_total = sum_row(get_var_range("b_1", "b_5"))
        }), 
        result_dfs
    )
    
    expect_identical(
        dfs %>% modify( {
            a_total = sum_row(a_1 %to% a_5)
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
            a_total = sum_row(get_var_range("a_1", "a_5"))
            b_total = sum_row(get_var_range("b_1", "b_5"))
        }), 
        tbl_df(result_dfs)
    )
    
    expect_identical(
        tbl_df(dfs) %>% modify( {
            a_total = sum_row(a_1 %to% a_5)
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

expect_error(
    modify_if(dfs2, test %in% 2:4,
              {
                  a_total = sum_row(a_1 %to% a_5)
                  b_total = sum_row(b_1 %to% b_5)
                  aa = aa + 1
              })
)


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








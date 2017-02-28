
context("vars with '%to%'")
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
    zz = 10,
    # b_3 = 42,
    aa = 10 %r% 5,
    b_ = 20 %r% 5,
    b_1 = 11 %r% 5,
    b_2 = 12 %r% 5,
    b_4 = 14 %r% 5,
    b_5 = 15 %r% 5 
)

result_dfs = dfs
result_dfs$b_total = with(dfs, sum_row(b_1, b_2, b_4, b_5))
# result_dfs$a_total = sum_row(a_1, a_2, a_4, a_5)


expect_warning(vars_range("a_1", "a_5"))

expect_warning(vars_range_list("a_1", "a_5"))

expect_identical(
    with(dfs, vars(b_1 %to% b_5)), 
    with(dfs, data.frame(b_1 = b_1, b_2 = b_2, b_4 = b_4, b_5 = b_5))
)


expect_identical(
    within(dfs, {
           b_total = sum_row(vars(b_1 %to% b_5))
           }), 
    result_dfs
)

expect_identical(
    transform(dfs,
        b_total = sum_row(vars(b_1 %to% b_5))
    ), 
    result_dfs
)





context("%to%")
expect_identical(a_1 %to% a_5, data.frame(a_1 = a_1, a_2 = a_2, a_4 = a_4, a_5 = a_5))
expect_identical(a_1 %to_list% a_5, list(a_1 = a_1, a_2 = a_2, a_4 = a_4, a_5 = a_5))
expect_error(
    with(dfs, a_1 %to% a_5))

expect_error(
    with(dfs, a_1 %to_list% a_5))

expect_identical(
    with(dfs, b_1 %to% b_5), 
    with(dfs, data.frame(b_1 = b_1, b_2 = b_2, b_4 = b_4, b_5 = b_5))
)
expect_identical(
    within(dfs, {
        b_total = sum_row(b_1 %to% b_5)
    }), 
    result_dfs
)


expect_identical(
    transform(dfs,
              b_total = sum_row(b_1 %to% b_5)

              
    ), 
    result_dfs
)


expect_error(b_1 %to% a_5)
expect_error(a_5 %to% a_1)
expect_error(a_1a %to% a_5)
expect_error(rep("a_1",2) %to% rep("a_5",2))
expect_error(d_1 %to% d_5)

# context("dplyr")
# if(suppressWarnings(require(dplyr, quietly = TRUE))){
    # expect_identical(
    #     dfs %>% mutate(
    #               b_total = sum_row(vars_range("b_1", "b_5")),
    #               a_total = sum_row(vars_range("a_1", "a_5"))
    #               
    #     ), 
    #     result_dfs
    # )
    # 
    # mutate(dfs, 
    #     b_total = sum_row(vars_range("b_1", "b_5")),
    #     a_total = sum_row(vars_range("a_1", "a_5"))
    #     
    # )
    
# } else {
    # cat("dplyr not found\n")
# }

context("magrittr")
if(suppressWarnings(require(magrittr, quietly = TRUE))){
    expect_identical(
        dfs %>% with(vars(b_1 %to% b_5)), 
        with(dfs, data.frame(b_1 = b_1, b_2 = b_2, b_4 = b_4, b_5 = b_5))
    )

    expect_identical(
        dfs %>% within( {
            b_total = sum_row(vars(b_1 %to% b_5))
        }), 
        result_dfs
    )
    
    expect_identical(
        dfs %>% transform(
            b_total = sum_row(vars(b_1 %to% b_5))

            
        ), 
        result_dfs
    )

    expect_identical(
        dfs %$% {
            sum_row(vars(b_1 %to% b_5))
        }, 
        result_dfs$b_total
    )
    
    context("magrittr %to% ")
    
    expect_identical(
        dfs %>% with(b_1 %to% b_5), 
        with(dfs, data.frame(b_1 = b_1, b_2 = b_2, b_4 = b_4, b_5 = b_5))
    )

    expect_identical(
        dfs %>% within( {
            b_total = sum_row(b_1 %to% b_5)
        }), 
        result_dfs
    )
    
    expect_identical(
        dfs %>% transform(
            b_total = sum_row(b_1 %to% b_5)
            
        ), 
        result_dfs
    )
    expect_identical(
        dfs %$% {
            sum_row(b_1 %to% b_5)
        }, 
        result_dfs$b_total
    )
   
    
} else {
    cat("magrittr not found\n")
}

# 
# library(data.table)
# 
# dts = as.data.table(dfs)
# result_dts = as.data.table(result_dfs)
# dts[,new := sum_row(vars_range("b_1", "b_5"))]
# dts


context("vars with perl")

expect_warning(vars_pattern("a_[0-9]"))
expect_warning(vars_pattern_list("a_[0-9]"))

expect_identical(vars(perl("a_[0-9]")), data.frame(a_1 = a_1, a_2 = a_2, a_4 = a_4, a_5 = a_5))
expect_identical(vars_list(perl("a_[0-9]")), list(a_1 = a_1, a_2 = a_2, a_4 = a_4, a_5 = a_5))

expect_identical(
    with(dfs, vars(perl("b_[0-9]"))), 
    with(dfs, data.frame(b_1 = b_1, b_2 = b_2, b_4 = b_4, b_5 = b_5))
)


expect_identical(
    within(dfs, {
        b_total = sum_row(vars(perl("b_[0-9]")))
    }), 
    result_dfs
)

expect_identical(
    transform(dfs,
              b_total = sum_row(vars(perl("b_[0-9]")))
              
    ), 
    result_dfs
)



context("vars")
expect_identical(vars("a_`c(1:2,4:5)`"), data.frame(a_1 = a_1, a_2 = a_2, a_4 = a_4, a_5 = a_5))
expect_identical(vars_list("a_`c(1:2,4:5)`"), list(a_1 = a_1, a_2 = a_2, a_4 = a_4, a_5 = a_5))


expect_identical(
    with(dfs, vars("b_`c(1:2,4:5)`")), 
    with(dfs, data.frame(b_1 = b_1, b_2 = b_2, b_4 = b_4, b_5 = b_5))
)
expect_identical(
    within(dfs, {
        b_total = sum_row(vars("b_`c(1:2,4:5)`"))
    }), 
    result_dfs
)


expect_identical(
    transform(dfs,
              b_total = sum_row(vars("b_`c(1:2,4:5)`"))
    ), 
    result_dfs
)




expect_error(vars("z`1:5`"))
expect_error(vars("a_`c(1:2,4:5)`", "z`1:5`"))
# expect_error(a_5 %to% a_1)
# expect_error(a_1a %to% a_5)
# expect_error(rep("a_1",2) %to% rep("a_5",2))
# expect_error(d_1 %to% d_5)
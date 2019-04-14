context("data.table modify")
suppressWarnings(RNGversion("3.5.0"))


dt = data.table(
    zz = 42,
    b_3 = 44,
    aa = 10 %r% 5,
    b_ = 20 %r% 5,
    b_1 = 11 %r% 5,
    b_2 = 12 %r% 5,
    b_4 = 14 %r% 5,
    b_5 = 15 %r% 5 
)

result_dt = data.table::copy(dt)
result_dt$b_total = with(dt, sum_row(b_1, b_2, b_4, b_5))
set.seed(1)
result_dt$b_total = with(dt, sum_row(b_1, b_2, b_4, b_5))
result_dt$random_numer = runif(nrow(dt))


set.seed(1) 
# inplace modification 
modify(dt, {
    b_total = sum_row(vars(b_1 %to% b_5))
    random_numer = runif(.n)
})
expect_identical(
    dt, 
    result_dt
)

expect_error(
    modify(dt, {
        new_v = 1:2
    })
)

expect_error(
    modify_if(dt, (1:.N)<3,{
        new_v = 1:4
    })
)

result_dt$random_numer = NULL
result_dt$b_total = NULL

modify(dt, {
    random_numer = NULL
    b_total = NULL
})
expect_identical(
    dt, 
    result_dt
)

dt_iris = data.table(iris)
test_iris = data.table(iris)
test_iris$new_var = NA
test_iris$new_logi = FALSE
test_iris$new_num = 0
test_iris$new_char = ""

dt_iris2 = modify(dt_iris, {
    new_var = .new_var()
    new_logi = .new_logical()
    new_num = .new_numeric()
    new_char = .new_character()
})

expect_identical(dt_iris, test_iris)
expect_identical(dt_iris2, test_iris)

############

# set.seed(1)
# dt2 = data.table(
#     zz = 42,
#     b_3 = 44,
#     aa = 10 %r% 5,
#     b_ = 20 %r% 5,
#     b_1 = 11 %r% 5,
#     b_2 = 12 %r% 5,
#     b_4 = 14 %r% 5,
#     b_5 = 15 %r% 5 
# )
# dt2$test = 1:5
# 
# result_dt2 = data.table::copy(dt2)
# # result_dt2$a_total = ifelse(dt2$test %in% 2:4, sum_row(a_1, a_2, a_4, a_5), NA)
# result_dt2$b_total = ifelse(dt2$test %in% 2:4, with(dt2, sum_row(b_1, b_2, b_4, b_5)), NA)
# result_dt2$aa = ifelse(dt2$test %in% 2:4, result_dt2$aa+1, result_dt2$aa)
# 
# result_dt2[result_dt2$test %in% 2:4, "random_numer"] = runif(3) 
# set.seed(1)
# expect_identical(
#     modify_if(dt2, test %in% 2:4,
#               {
#                   b_total = sum_row(b_1 %to% b_5)
#                   aa = aa + 1
#                   random_numer = runif(.n)
#               }), 
#     result_dt2
# )
# 
# 
# set.seed(1)
# expect_identical(
#     modify_if(dt2, 2:4,
#               {
#                   b_total = sum_row(b_1 %to% b_5)
#                   aa = aa + 1
#                   random_numer = runif(.n)
#               }), 
#     result_dt2
# )
# 
# 
# set.seed(1)
# expect_identical(
#     modify_if(dt2, test %in% 2:4,
#               {
#                   b_total = sum_row(b_1 %to% b_5)
#                   aa = aa + 1
#                   random_numer = runif(.N)
#               }), 
#     result_dt2
# )

context("data.table vec ops")

dt2 = data.table(
    zz = 42,
    b_3 = NA,
    aa = 10 %r% 5,
    b_ = 20 %r% 5,
    b_1 = 11 %r% 5,
    b_2 = 12 %r% 5,
    b_4 = 14 %r% 5,
    b_5 = 15 %r% 5
)

expect_identical(dt2 %n_i% "b_", dt2[,"b_"])
expect_identical(dt2 %n_d% "b_", dt2[,-"b_"])


expect_identical(dt2 %i% anyNA, dt2[,"b_3"])
expect_identical(dt2 %d% anyNA, dt2[,-"b_3"])
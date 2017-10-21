context("data.table modify")

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
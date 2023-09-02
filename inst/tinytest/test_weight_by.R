context("weight_by")

data(state)

expect_identical(
    weight_by(state.x77, 1), state.x77
)

expect_identical(
    weight_by(state.x77), state.x77
)

expect_identical(
    weight_by(state.x77, .3), state.x77[FALSE, , drop = FALSE]
)

expect_identical(
    weight_by(state.x77, c(1, rep(NA, nrow(state.x77) - 1))), state.x77[1, , drop = FALSE]
)
expect_identical(
    weight_by(state.x77, .3), state.x77[FALSE, , drop = FALSE]
)

df_x77 = as.data.frame(state.x77)

expect_error(weight_by(df_x77, 1:2))

ex_weight = rep(seq_len(nrow(df_x77)), each = 2)

expect_identical(
    weight_by(df_x77, 2), df_x77[ex_weight,]
)

ex_weight2 = rep(2, nrow(df_x77))
expect_identical(
    weight_by(df_x77, ex_weight2), df_x77[ex_weight,]
)

dt = data.table(var = 1:5, ww = c(-1, NA, .4, 1.5, 0.5))
expect_identical(
    weight_by(dt, ww), dt[c(4, 4, 5), ] 
)





#################################

context("cro_fun_df")

data(mtcars)
mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"

    var_lab(vs) = "V/S"
    val_lab(vs) = c("Straight" = 0, "V" = 1)

    var_lab(am) = "Transmission (0 = automatic, 1 = manual)"
    val_lab(am) = c(" automatic" = 0, " manual" =  1)

    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
})

expect_equal_to_reference(
    mtcars %calc% cro_fun_df(mpg, 
                             col_vars = vs, 
                             fun = w_mean, 
                             row_vars = am
    )
    ,"rds/table_summary_df0.rds"
)

expect_error(
    mtcars %calc% cro_fun_df(mpg, 
                                   col_vars = vs[1:2], 
                                   fun = w_mean, 
                                   row_vars = am
    )
)

expect_error(
    mtcars %calc% cro_fun_df(mpg[1:2], 
                                   col_vars = vs, 
                                   fun = w_mean, 
                                   row_vars = am
    )
)

expect_error(
    mtcars %calc% cro_fun_df(mpg, 
                                   col_vars = vs, 
                                   fun = w_mean, 
                                   row_vars = am[1:2]
    )
)

expect_error(
    mtcars %calc% cro_fun_df(mpg, 
                                   col_vars = vs, 
                                   fun = w_mean, 
                                   row_vars = am,
                                   weight = 1:2
    )
)



expect_equal_to_reference(
    mtcars %calc% cro_fun_df(mpg, 
                                   col_vars = vs, 
                                   fun = colMeans, 
                                   row_vars = am
    )
    ,"rds/table_summary_df0_colMeans.rds"
)

expect_equal_to_reference(
    mtcars %calc% cro_fun_df(mpg, 
                                   col_vars = vs, 
                                   fun = function(x) numeric(0), 
                                   row_vars = am
    )
    ,"rds/table_summary_df0empty.rds"
)

expect_equal_to_reference(
    mtcars %calc% table_summary_df(mpg, 
                                   col_vars = vs, 
                                   fun = function(x, weight = NULL) numeric(0), 
                                   row_vars = am, weight = 1
    )
    ,"rds/table_summary_df0empty.rds"
)

expect_equal_to_reference(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, fun = function(x){

        colMeans(x)
    })
    ,"rds/table_summary_df1.rds"
)

expect_equal_to_reference(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, fun = function(x){

        dtfrm(parameter = names(x), mean = colMeans(x))
    })
    ,"rds/table_summary_df2.rds"
)

expect_equal_to_reference(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, fun = function(x){

        dtfrm(res_num = seq_along(x), parameter = names(x), mean = colMeans(x))
    },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
    hide = "res_num",
    use_result_row_order = FALSE
    )
    ,"rds/table_summary_df3.rds"
)

expect_equal_to_reference(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, fun = function(x){

        dtfrm(res_num = seq_along(x), parameter = names(n2l(x)), mean = colMeans(x))
    },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
    hide = "res_num",
    use_result_row_order = FALSE)
    ,"rds/table_summary_df4.rds"
)

expect_equal_to_reference(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, row_vars = mtcars$vs,
                     fun = function(x){
                         dtfrm(res_num = seq_along(x), parameter = names(n2l(x)), mean = colMeans(x))
                     },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                     hide = "res_num",
                     use_result_row_order = FALSE
                     )
    ,"rds/table_summary_df5.rds"
)


expect_equal_to_reference(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = dtfrm("Total", mtcars$am), 
                     row_vars = dtfrm("Total",mtcars$vs),
                     fun = function(x){
                         dtfrm(res_num = seq_along(x), parameter = names(n2l(x)), mean = colMeans(x))
                     },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                     hide = "res_num",
                     use_result_row_order = FALSE
    )
    ,"rds/table_summary_df5c.rds"
)


expect_equal_to_reference(
    mtcars %where% FALSE %calc% table_summary_df(vars(!fixed("vs") & !fixed("am")), col_vars = am, row_vars = vs,
                     fun = function(x){
                         dtfrm(res_num = seq_along(x), parameter = names(n2l(x)), mean = colMeans(x))
                     },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                     hide = "res_num",
                     use_result_row_order = FALSE
    )
    ,"rds/table_summary_df5a.rds"
)


expect_equal_to_reference(
    mtcars %where% FALSE %calc% table_summary_df(vars(!fixed("vs") & !fixed("am")), col_vars = am, 
                                                 fun = function(x){
                                                     dtfrm(res_num = seq_along(x), parameter = names(n2l(x)), mean = colMeans(x))
                                                 },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                                                 hide = "res_num",
                                                 use_result_row_order = FALSE
    )
    ,"rds/table_summary_df5b.rds"
)

expect_identical(
    mtcars %where% FALSE %calc% table_summary_df(vars(!fixed("vs") & !fixed("am")), col_vars = am, 
                                                 fun = function(x){
                                                     dtfrm(res_num = seq_along(x), parameter = names(n2l(x)), mean = colMeans(x))
                                                 },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                                                 hide = "res_num",
                                                 use_result_row_order = FALSE
    )
    ,
    mtcars %calc% table_summary_df(vars(!fixed("vs") & !fixed("am")), col_vars = am, 
                                                 fun = function(x){
                                                     dtfrm(res_num = seq_along(x), parameter = names(n2l(x)), mean = colMeans(x))
                                                 }, 
                                   subset = FALSE,
                                    row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                                                 hide = "res_num",
                                                 use_result_row_order = FALSE
    )
)

expect_identical(
    mtcars %where% (cyl==8) %calc% table_summary_df(vars(!fixed("vs") & !fixed("am")), col_vars = am, 
                                                 fun = function(x){
                                                     dtfrm(res_num = seq_along(x), parameter = names(n2l(x)), mean = colMeans(x))
                                                 },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                                                 hide = "res_num",
                                                 use_result_row_order = FALSE
    )
    ,
    mtcars %calc% table_summary_df(vars(!fixed("vs") & !fixed("am")), col_vars = am, 
                                   fun = function(x){
                                       dtfrm(res_num = seq_along(x), parameter = names(n2l(x)), mean = colMeans(x))
                                   }, 
                                   subset = cyl==8,
                                   row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                                   hide = "res_num",
                                   use_result_row_order = FALSE
    )
)





expect_equal_to_reference(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, row_vars = mtcars$vs,
                     fun = function(x, weight = NULL){
                         res = w_cor(x, weight = weight)
                         dtfrm(res_num = seq_along(x), parameter = rownames(res), cor = res[,1])
                     },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                     hide = "res_num",
                     use_result_row_order = FALSE
                     )
    ,"rds/table_summary_df6.rds"
)

expect_equal_to_reference(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, row_vars = mtcars$vs,
                     fun = function(x, weight = NULL){
                         res = w_cor(x, weight = weight)
                         means = unlist(lapply(x, w_mean, weight = weight))
                         dtfrm(res_num = seq_along(x), parameter = rownames(res), cor = res[,1], mean = means)
                     },   row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                     hide = "res_num",
                     use_result_row_order = FALSE
                     )
    ,"rds/table_summary_df7.rds"
)

expect_equal_to_reference(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = mtcars$am,
                     fun = function(x, weight = NULL){
                         res = w_cor(x, weight = weight)
                         means = unlist(lapply(x, w_mean, weight = weight))
                         dtfrm(res_num = seq_along(nrow(res)), parameter = rownames(res), rbind(
                             cbind(stat = "cor", value = res[,1]),
                             cbind(stat = "mean", value = means)
                         ))
                     },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                     col_labels = c("col_vars", "col_vars_values", "stat"),
                     hide = "res_num",
                     use_result_row_order = FALSE

                     )
    ,"rds/table_summary_df8.rds"
)


expect_equal_to_reference(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = mtcars$am,
                     fun = function(x, weight = NULL){
                         if(is.null(weight)) weight = 1
                         dtfrm(res_num = seq_along(x), parameter = names(x), mean = colSums(x*weight))
                     },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                     hide = "res_num",
                     use_result_row_order = FALSE
                     )
    ,"rds/table_summary_df9.rds"
)

expect_equal_to_reference(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = mtcars$am,
                     fun = function(x, weight = NULL){
                         if(is.null(weight)) weight = 1
                         dtfrm(res_num = seq_along(x), parameter = names(x), mean = colSums(x*weight))
                     },  row_labels = c("row_vars", "row_vars_values", "res_num", "parameter"),
                     hide = "res_num",
                     use_result_row_order = FALSE, weight = 2)
    ,"rds/table_summary_df10.rds"
)


expect_equal_to_reference(
    table_summary_df(mtcars %except% c("cyl", "am"), col_vars = list("Total", mtcars$am), fun = colMeans),
    "rds/table_summary_df11.rds"
)

expect_equal_to_reference(
    table_summary_df(mtcars %except% c("cyl", "am"), col_vars = dtfrm("Total", mtcars$am), fun = colMeans),
    "rds/table_summary_df11.rds"
)

expect_equal_to_reference(
    table_summary_df(mtcars %except% c("cyl", "am"),
                     col_vars = list("Total", mtcars$am),
                     fun = colMeans,
                     hide = "col_vars"
                     ),
    "rds/table_summary_df12.rds"
)


expect_error(
    table_summary_df(mtcars %except% c("cyl", "am"), col_vars = mtcars$am, fun = colMeans, weight = 2)
)





# add_val_lab(mtcars$am) = c(HardToSay = 3)
# expect_equal_to_reference(
#     table_summary_df(mtcars %except% c("cyl", "am"), col_vars = list("Total", mtcars$am), fun = colMeans)
#     ,"rds/table_summary_df12.rds"
# )



context("table correlations")
val_lab(mtcars$am) = val_lab(mtcars$am)[1:2] 
expect_equal_to_reference(
    table_pearson(mtcars %except% qc(vs, am), col_vars = mtcars$am)
    ,"rds/table_cor_1.rds"
)
expect_equal_to_reference(
    table_spearman(mtcars %except% qc(vs, am), col_vars = mtcars$am)
    ,"rds/table_cor_2.rds"
)

expect_equal_to_reference(
    mtcars %where% FALSE %calc% table_pearson(vars(!perl("vs|am")), col_vars = am)
    ,"rds/table_cor_1a.rds"
)


expect_equal_to_reference(
    mtcars %where% FALSE %calc% table_pearson(vars(!perl("vs|am")), col_vars = am, weight = numeric(0))
    ,"rds/table_cor_1a.rds"
)

expect_equal_to_reference(
    table_spearman(mtcars %except% qc(vs, am), col_vars = mtcars$am)
    ,"rds/table_cor_2.rds"
)

expect_equal_to_reference(
    table_spearman(mtcars %except% qc(vs, am), col_vars = mtcars$am, weight = 1)
    ,"rds/table_cor_2.rds"
)

expect_identical(
    mtcars %where% FALSE %calc% table_pearson(vars(!perl("vs|am")), col_vars = am)
    ,
    mtcars  %calc% table_pearson(vars(!perl("vs|am")), col_vars = am, subset = FALSE)
)

expect_identical(
    mtcars %where% FALSE %calc% table_spearman(vars(!perl("vs|am")), col_vars = am)
    ,
    mtcars  %calc% table_spearman(vars(!perl("vs|am")), col_vars = am, subset = FALSE)
)

expect_identical(
    mtcars %where% (cyl == 8) %calc% table_pearson(vars(!perl("vs|am")), col_vars = am)
    ,
    mtcars  %calc% table_pearson(vars(!perl("vs|am")), col_vars = am, subset = (cyl == 8))
)

expect_identical(
    mtcars %where% (cyl == 8) %calc% table_spearman(vars(!perl("vs|am")), col_vars = am)
    ,
    mtcars  %calc% table_spearman(vars(!perl("vs|am")), col_vars = am, subset = (cyl == 8))
)

set.seed(1)
weight = runif(nrow(mtcars), 1,2)
expect_equal_to_reference(
    table_pearson(mtcars %except% qc(vs, am), col_vars = mtcars$am, weight = weight)
    ,"rds/table_cor_3.rds"
)
expect_equal_to_reference(
    table_spearman(mtcars %except% qc(vs, am), col_vars = mtcars$am, weight = weight)
    ,"rds/table_cor_4.rds"
)


expect_equal(table_pearson(mtcars %except% qc(vs, am), col_vars = "Total")[[2]],
             unname(cor(mtcars %except% qc(vs, am))[,1]))
expect_equal(table_spearman(mtcars %except% qc(vs, am), col_vars = "Total")[[2]],
             unname(cor(mtcars %except% qc(vs, am), method = "spearman")[,1]))

context("table_summary_df error with index")
expect_error(
    table_summary_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, fun = function(x){
        
        colMeans(x)
    }, 
    use_result_row_order = FALSE
    )
)

context("table_summary_df datetime")

dates = as.POSIXct(rep(paste0("2017-02-", 1:10), each = 10))
measure = runif(length(dates), 1, 2)
expect_equal_to_reference(
table_summary_df(measure, col_vars = "Total", row_vars = dates, fun = mean)
,"rds/table_summary_df_dates1.rds"
)
expect_equal_to_reference(
table_summary_df(measure, col_vars = dates, fun = mean)
,"rds/table_summary_df_dates2.rds"
)
var_lab(dates) = "Day"
expect_equal_to_reference(
table_summary_df(measure, col_vars = "Total", row_vars = dates, fun = mean)
,"rds/table_summary_df_dates3.rds"
)
expect_equal_to_reference(
table_summary_df(measure, col_vars = dates, fun = mean)
,"rds/table_summary_df_dates4.rds"
)
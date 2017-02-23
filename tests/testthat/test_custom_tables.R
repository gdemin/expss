context("custom tables elementary functions")
make_empty = expss:::make_empty_intermediate_table()

res = list(long_table = NULL, 
           colvars = NULL, 
           rowvars = NULL, 
           cellvars = NULL,
           weight = NULL, 
           subgroup = NULL,
           cell_name = NULL
)

class(res) = union("intermediate_table", class(res))

expect_identical(make_empty, res)

res2 = res
res2$weight = as.numeric(c(1:3, 0, 0))
expect_identical(weight_(c(1:3, -1, NA)), res2)
res2$weight = NULL
expect_identical(weight_(c(1:3, -1, NA)) %>% weight_(), res2)

expect_error(weight_("sdaasd"))
expect_error(weight_(1, 2))

res2 = res
res2$subgroup = 1:3>2
expect_identical(subgroup_(1:3>2), res2)
res2$subgroup = NULL
expect_identical(subgroup_(1:3>2) %>% subgroup_(), res2)

expect_error(subgroup_("sdaasd"))
expect_error(subgroup_(1, 2))



context("stat_")


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
    (cell_(mtcars$am) %>% col_vars_(mtcars$vs) %>% stat_(colMeans))$long_table,
    "rds/long_table_summary_df1.rds")
expect_equal_to_reference(
        (col_vars_(mtcars$vs) %>% 
            cell_(list(am = mtcars$am, vs = mtcars$vs)) %>% 
                          stat(function(x) as.dtfrm(t(colMeans(x)))))$long_table,
    "rds/long_table_summary_df2.rds")

expect_equal_to_reference(
    long_table_summary_df(list(mtcars$am) , list(mtcars$vs), fun = function(x) as.dtfrm(t(colMeans(x))), row_vars = list(mtcars$vs)),
    "rds/long_table_summary_df2a.rds")


expect_equal_to_reference(
    long_table_summary_df(list(am = mtcars$am, cyl = mtcars$cyl), list(mtcars$vs, mtcars$gear),
                          fun = function(x) as.dtfrm(t(colMeans(x)))),
    "rds/long_table_summary_df3.rds")



mult1 = dtfrm(v1 = c(1,1,NA), v2 = c(NA, 2, 2))
mult2 = dtfrm(b1 = c(1, 1, 2), b2 = c(3, NA, 1))
av1 = 1:3
av2 = 5:7
av3 = 8:10
weight = c(1, 2, 3)
empty1 = as.numeric(NA)
empty2 = as.numeric(NA)

expect_equal_to_reference(
    long_table_summary_df(dtfrm(av1, av2, av3), list(mult2$b1),
                          fun =function(x) as.dtfrm(t(colMeans(x))), weight = NULL),
    "rds/long_table_summary_df6.rds")



sofisticated_fun =   function(x, weight=NULL) {
    res = dtfrm(mean = c(w_mean(x[[1]], weight), w_mean(x[[2]], weight), w_mean(x[[3]], weight)),
                n = c(w_n(x[[1]], weight), w_n(x[[2]], weight), w_n(x[[3]], weight))
    )
    dtfrm(row_labels = names(x), res)
}

expect_equal_to_reference(
    long_table_summary_df(list(dtfrm(av1, av2, av3)), list(mrset(mult2)),
                          fun = sofisticated_fun,
                          weight = NULL),
    "rds/long_table_summary_df7.rds")

expect_equal_to_reference(
    long_table_summary_df(list(dtfrm(av1, av2, av3)), list(mrset(mult2)),
                          fun = sofisticated_fun,
                          weight = 2),
    "rds/long_table_summary_df8.rds")

mult1[, 1:2] = NA
expect_equal_to_reference(
    long_table_summary_df(list(dtfrm(av1, av2, av3)), list(mrset(mult1)),
                          fun = sofisticated_fun,
                          custom_labels = "row_labels",
                          weight = 2),
    "rds/long_table_summary_df9.rds")

data(iris)
expect_equal_to_reference(
    long_table_summary_df(iris[,-5], list(iris$Species), fun = function(x){
        res = as.dtfrm(t(colMeans(x)))
        colnames(res) = paste0("means|", colnames(res))
        dtfrm(stat = "stat", res)
    } ),
    "rds/long_table_summary_df10.rds")

#################################

context("table_summary_df")

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
mtcars %calc% table_summary_df(mpg, 
                               col_vars = vs, 
                               fun = w_mean, 
                               row_vars = am
                                )
,"rds/table_summary_df0.rds"
)

expect_error(
    mtcars %calc% table_summary_df(mpg, 
                                   col_vars = vs[1:2], 
                                   fun = w_mean, 
                                   row_vars = am
    )
)

expect_error(
    mtcars %calc% table_summary_df(mpg[1:2], 
                                   col_vars = vs, 
                                   fun = w_mean, 
                                   row_vars = am
    )
)

expect_error(
    mtcars %calc% table_summary_df(mpg, 
                                   col_vars = vs, 
                                   fun = w_mean, 
                                   row_vars = am[1:2]
    )
)

expect_error(
    mtcars %calc% table_summary_df(mpg, 
                                   col_vars = vs, 
                                   fun = w_mean, 
                                   row_vars = am,
                                   weight = 1:2
    )
)



expect_equal_to_reference(
    mtcars %calc% table_summary_df(mpg, 
                                   col_vars = vs, 
                                   fun = colMeans, 
                                   row_vars = am
    )
    ,"rds/table_summary_df0.rds"
)

expect_equal_to_reference(
    mtcars %calc% table_summary_df(mpg, 
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
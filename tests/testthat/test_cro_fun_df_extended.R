

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
    mtcars %calc% cro_fun_df(mpg, 
                                   col_vars = vs, 
                                   fun = function(x, weight = NULL) numeric(0), 
                                   row_vars = am, weight = 1
    )
    ,"rds/table_summary_df0empty.rds"
)

expect_equal_to_reference(
    cro_fun_df(mtcars %except% qc(vs, am) %>% set_var_lab("|") %>% as.list, 
               col_vars = mtcars$am, fun = colMeans)
    ,"rds/table_summary_df1.rds"
)

expect_equal_to_reference(
    cro_fun_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, fun = function(x){

        dtfrm(parameter = names(x), mean = colMeans(x))
    })
    ,"rds/table_summary_df2.rds"
)

expect_equal_to_reference(
    cro_fun_df(mtcars %except% qc(vs, am) %>% unvr, col_vars = mtcars$am, fun = colMeans
    )
    ,"rds/table_summary_df3.rds"
)

expect_equal_to_reference(
    cro_fun_df(mtcars %except% qc(vs, am), col_vars = mtcars$am,  fun = colMeans)
    ,"rds/table_summary_df4.rds"
)

expect_equal_to_reference(
    cro_fun_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, row_vars = mtcars$vs,
                     fun = colMeans
                     )
    ,"rds/table_summary_df5.rds"
)


expect_equal_to_reference(
    cro_fun_df(mtcars %except% qc(vs, am), col_vars = dtfrm("Total", mtcars$am), 
                     row_vars = dtfrm("Total",mtcars$vs),
                     fun = colMeans
    )
    ,"rds/table_summary_df5c.rds"
)


expect_equal_to_reference(
    mtcars %where% FALSE %calc% cro_fun_df(vars(!fixed("vs") & !fixed("am")), col_vars = am, 
                                           row_vars = vs,
                     fun = colMeans
    )
    ,"rds/table_summary_df5a.rds"
)


expect_equal_to_reference(
    mtcars %where% FALSE %calc% cro_fun_df(vars(!fixed("vs") & !fixed("am")), 
                                           col_vars = am, 
                                           row_vars = numeric(0),
                                                 fun = colMeans
    )
    ,"rds/table_summary_df5b.rds"
)

expect_identical(
    mtcars %where% FALSE %calc% cro_fun_df(vars(!fixed("vs") & !fixed("am")), 
                                           col_vars = am, 
                                           row_vars = numeric(0),
                                           fun = colMeans
    )
    ,    mtcars %calc% cro_fun_df(vars(!fixed("vs") & !fixed("am")), 
                                                col_vars = am, 
                                                fun = colMeans,
                                  subgroup = FALSE
    )
)



expect_identical(
    mtcars %where% (cyl == 6) %calc% cro_fun_df(vars(!fixed("vs") & !fixed("am")), 
                                           col_vars = am, 
                                           fun = colMeans
    ),
    mtcars %calc% cro_fun_df(vars(!fixed("vs") & !fixed("am")), 
                                  col_vars = am, 
                                  fun = colMeans,
                                  subgroup = cyl==6
    )
)





expect_equal_to_reference(
    cro_fun_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, row_vars = mtcars$vs,
                     fun = function(x, weight = NULL){
                         w_cor(x, weight = weight)[,1]
                         
                     }
                     )
    ,"rds/table_summary_df6.rds"
)

expect_equal_to_reference(
    cro_fun_df(mtcars %except% qc(vs, am), col_vars = mtcars$am, row_vars = mtcars$vs,
                     fun = function(x, weight = NULL){
                         res = w_cor(x, weight = weight)
                         means = unlist(lapply(x, w_mean, weight = weight))
                         dtfrm(cor = res[,1], mean = means)
                     }
                     )
    ,"rds/table_summary_df7.rds"
)




expect_equal_to_reference(
    cro_fun_df(mtcars %except% qc(vs, am) %>% unvr, col_vars = mtcars$am,
                     fun = colSums
                     )
    ,"rds/table_summary_df9.rds"
)

expect_equal_to_reference(
    cro_fun_df(mtcars %except% qc(vs, am) %>% unvr, col_vars = mtcars$am,
                     fun = function(x, weight = NULL){
                         if(is.null(weight)) weight = 1
                         colSums(x*weight)
                     }, weight = 2)
    ,"rds/table_summary_df10.rds"
)


expect_equal_to_reference(
    cro_fun_df(mtcars %except% c("cyl", "am")   , 
               col_vars = list("Total", mtcars$am), fun = colMeans),
    "rds/table_summary_df11.rds"
)


expect_equal_to_reference(
    cro_fun_df(mtcars %except% c("cyl", "am") %>% set_var_lab("|") %>% as.list,
                     col_vars = list("Total", unvr(mtcars$am)),
                     fun = colMeans
                     ),
    "rds/table_summary_df12.rds"
)


expect_error(
    cro_fun_df(mtcars %except% c("cyl", "am"), col_vars = mtcars$am, fun = colMeans, weight = 2)
)




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
    mtcars %where% FALSE %calc% table_pearson(vars(!perl("vs|am")), col_vars = am, row_vars = numeric(0))
    ,"rds/table_cor_1a.rds"
)


expect_equal_to_reference(
    mtcars %where% FALSE %calc% table_pearson(vars(!perl("vs|am")), col_vars = am, row_vars = numeric(0), 
                                              weight = numeric(0))
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
    mtcars %where% FALSE %calc% table_pearson(vars(!perl("vs|am")), col_vars = am, row_vars = numeric(0))
    ,
    mtcars  %calc% table_pearson(vars(!perl("vs|am")), col_vars = am, subgroup = FALSE)
)

expect_identical(
    mtcars %where% FALSE %calc% table_spearman(vars(!perl("vs|am")), col_vars = am, row_vars = numeric(0))
    ,
    mtcars  %calc% table_spearman(vars(!perl("vs|am")), col_vars = am, subgroup = FALSE)
)

expect_identical(
    mtcars %where% (cyl == 8) %calc% table_pearson(vars(!perl("vs|am")), col_vars = am)
    ,
    mtcars  %calc% table_pearson(vars(!perl("vs|am")), col_vars = am, subgroup = (cyl == 8))
)


expect_identical(
    mtcars %where% (cyl > 4) %calc% table_pearson(vars(!perl("vs|am")), col_vars = am, row_vars = cyl)
    ,
    mtcars  %calc% table_pearson(vars(!perl("vs|am")), col_vars = am, row_vars = cyl, subgroup = (cyl > 4))
)

expect_identical(
    mtcars %where% (cyl == 8) %calc% table_spearman(vars(!perl("vs|am")), col_vars = am)
    ,
    mtcars  %calc% table_spearman(vars(!perl("vs|am")), col_vars = am, subgroup = (cyl == 8))
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


context("table_summary_df datetime")

dates = as.POSIXct(rep(paste0("2017-02-", 1:10), each = 10))
measure = runif(length(dates), 1, 2)
expect_equal_to_reference(
cro_fun_df(measure, col_vars = "Total", row_vars = dates, fun = mean)
,"rds/table_summary_df_dates1.rds"
)
expect_equal_to_reference(
    cro_fun_df(measure, col_vars = dates, fun = mean)
,"rds/table_summary_df_dates2.rds"
)
var_lab(dates) = "Day"
expect_equal_to_reference(
    cro_fun_df(measure, col_vars = "Total", row_vars = dates, fun = mean)
,"rds/table_summary_df_dates3.rds"
)
expect_equal_to_reference(
    cro_fun_df(measure, col_vars = dates, fun = mean)
,"rds/table_summary_df_dates4.rds"
)
context("custom tables")

data(mtcars)
# add labels to dataset
mtcars = apply_labels(mtcars, 
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      carb = NULL,
                      qsec = "1/4 mile time",
                      hp = "Gross horsepower",
                      vs = "Engine",
                      vs = num_lab(" 
                                   0 V-engine
                                   1 Straight engine
                                   "),
                      
                      am = "Transmission",
                      am = num_lab(" 
                                   0 Automatic
                                   1 Manual
                                   ")
                      )

res = mtcars %>% 
    tab_cells(mpg, disp) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_fun(w_mean) %>% 
    tab_pivot

expect_equal_to_reference(res, "rds/ctable0.rds")

res = mtcars %>% 
    tab_cols(vs) %>% 
    tab_cells(mpg, disp) %>% 
    tab_rows(am) %>% 
    tab_stat_fun(w_mean) %>% 
    tab_pivot

expect_equal_to_reference(res, "rds/ctable0.rds")

res = mtcars %>% 
    tab_rows(am) %>% 
    tab_cols(vs) %>% 
    tab_cells(mpg, disp) %>% 
    tab_stat_fun(w_mean) %>% 
    tab_pivot

expect_equal_to_reference(res, "rds/ctable0.rds")

res = mtcars %>% 
    tab_cells(mpg, disp) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_fun(w_mean = w_mean) %>% 
    tab_pivot

expect_equal_to_reference(res, "rds/ctable1.rds")

res = mtcars %>% 
    tab_cells(mpg, disp) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_fun("|" = w_mean) %>% 
    tab_pivot
expect_equal_to_reference(res, "rds/ctable2.rds")

res = mtcars %>% 
    tab_cells(mpg, disp) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_fun("|" = w_mean, label = "Mean value") %>% 
    tab_pivot

expect_equal_to_reference(res, "rds/ctable3.rds")

res = mtcars %>% 
    tab_cells(mpg, disp) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_fun("|" = w_mean, label = "Mean value") %>% 
    tab_pivot(stat_position = "inside_rows")

expect_equal_to_reference(res, "rds/ctable3.rds")

res = mtcars %>% 
    tab_cells(mpg, disp) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_fun("|" = w_mean, label = "Mean value") %>% 
    tab_pivot(stat_position = "inside_columns")

expect_equal_to_reference(res, "rds/ctable4.rds")

res = mtcars %>% 
    tab_cells(mpg, disp) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_mean(label = "Mean value") %>% 
    tab_pivot()
expect_equal_to_reference(res, "rds/ctable3.rds")

res = mtcars %>% 
    tab_cells(mpg, disp) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_mean(label = "Mean value") %>% 
    tab_pivot(stat_position = "inside_columns")

expect_equal_to_reference(res, "rds/ctable4.rds")

res = mtcars %>% 
    tab_cells(mpg, disp) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_mean() %>% 
    tab_stat_sd() %>% 
    tab_stat_valid_n() %>% 
    tab_pivot(stat_position = "inside_rows")

expect_equal_to_reference(res, "rds/ctable5.rds")

res = mtcars %>% 
    tab_cells(mpg, disp) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_mean() %>% 
    tab_stat_sd() %>% 
    tab_stat_valid_n() %>% 
    tab_pivot(stat_position = "inside_columns")

expect_equal_to_reference(res, "rds/ctable6.rds")

res = mtcars %>% 
    tab_cells(mpg, disp) %>% 
    tab_cols(total(), vs) %>% 
    tab_rows(am) %>% 
    tab_stat_fun(summary) %>% 
    tab_pivot()

expect_equal_to_reference(res, "rds/ctable7.rds")

res = mtcars %>% 
    tab_cells(vs) %>% 
    tab_cols(total(), am) %>% 
    tab_stat_cpct(total_row_position = "none", label = "col %") %>%
    tab_stat_rpct(total_row_position = "none", label = "row %") %>%
    tab_stat_tpct(total_row_position = "none", label = "table %") %>%
    tab_cells(total(vs, label = "#Total|cases")) %>% 
    tab_stat_cases(total_row_position = "none") %>% 
    tab_pivot(stat_position = "inside_rows") 

expect_equal_to_reference(res, "rds/ctable8.rds")

res = mtcars %>% 
    tab_subgroup(vs == 0) %>% 
    tab_cells(vs) %>% 
    tab_cols(total(), am) %>% 
    tab_stat_cpct(total_row_position = "none", label = "col %") %>%
    tab_stat_rpct(total_row_position = "none", label = "row %") %>%
    tab_stat_tpct(total_row_position = "none", label = "table %") %>%
    tab_cells(total(vs, label = "#Total|cases")) %>% 
    tab_stat_cases(total_row_position = "none") %>% 
    tab_pivot(stat_position = "inside_rows") 

expect_equal_to_reference(res, "rds/ctable9.rds")

res = mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), am) %>% 
    tab_stat_rpct(total_row_position = "none") %>%
    tab_pivot() %>% 
    tab_transpose() %>% 
    tab_sort_desc()

expect_equal_to_reference(res, "rds/ctable10.rds")

res = mtcars %>% 
    tab_cells(mpg, disp, hp) %>% 
    tab_cols(total(label = "#Total| "), am) %>%
    tab_stat_fun(Mean = w_mean, "Std. dev" = w_sd, "Valid N" = w_n, method = list) %>% 
    tab_sort_asc() %>% 
    tab_transpose() %>% 
    tab_pivot() %>% 
    split_columns()

expect_equal_to_reference(res, "rds/ctable11.rds")

set.seed(1)
df = data.frame(area=rep(c('Area 1','Area 2'), each=6),
                var_orange=sample(0:1, 12, T),
                var_banana=sample(0:1, 12, T),
                var_melon=sample(0:1, 12, T),
                var_mango=sample(0:1, 12, T))


res = df %>% 
    tab_cells(mdset(var_orange, var_banana, var_melon, var_mango)) %>% 
    tab_cols(total(), list(area)) %>% 
    tab_stat_cpct_responses(total_row_position = "below", total_statistic = "u_responses") %>% 
    tab_pivot()

expect_equal_to_reference(res, "rds/ctable12.rds")

res = mtcars %>% 
    tab_cells(cyl, carb) %>% 
    tab_cols(total(), am %nest% vs) %>%
    tab_stat_fun(Mean = w_mean, "Std. dev" = w_sd, "Valid N" = w_n) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() 

expect_equal_to_reference(res, "rds/ctable13.rds")

res = mtcars %>% 
    tab_cols(total(), am %nest% vs) %>%
    tab_cells(cyl) %>% 
    tab_stat_fun(Mean = w_mean, "Std. dev" = w_sd, "Valid N" = w_n) %>% 
    tab_stat_cpct() %>% 
    tab_cells(carb) %>% 
    tab_stat_fun(Mean = w_mean, "Std. dev" = w_sd, "Valid N" = w_n) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() 

expect_equal_to_reference(res, "rds/ctable14.rds")



res = mtcars %>%
    tab_cols(total(), am %nest% vs)

for(each in qc(cyl, carb, hp)){
    res = res %>% tab_cells(vars(each)) %>%
        tab_stat_fun(Mean = w_mean, "Std. dev" = w_sd, "Valid N" = w_n) %>%
        tab_stat_cpct()
}
res = res %>% tab_pivot()

expect_equal_to_reference(res, "rds/ctable15.rds")


res = mtcars %>% 
    tab_cells(cyl, carb) %>% 
    tab_cols(total(), am %nest% vs) %>%
    tab_stat_fun(w_mean, w_sd, w_n, method = list) %>% 
    tab_pivot()
expect_equal_to_reference(res, "rds/ctable16.rds")

res = mtcars %>%
    tab_cells(cyl) %>%
    tab_cols(total(), am) %>%
    tab_stat_cpct(total_row_position = "none", label = "col %") %>%
    tab_stat_rpct(total_row_position = "none", label = "row %") %>%
    tab_stat_tpct(total_row_position = "none", label = "table %") %>%
    tab_pivot(stat_position = "inside_columns")
expect_equal_to_reference(res, "rds/ctable17.rds")
 
res = mtcars %>%
    tab_cells(cyl) %>%
    tab_cols(total(), am) %>%
    tab_rows(vs) %>%
    tab_stat_cpct(total_row_position = "none", label = "col %") %>%
    tab_stat_rpct(total_row_position = "none", label = "row %") %>%
    tab_stat_tpct(total_row_position = "none", label = "table %") %>%
    tab_pivot(stat_position = "inside_rows")
expect_equal_to_reference(res, "rds/ctable18.rds")

context("custom tables summary stats")

mtcars$mpg[1:2] = NA
mtcars$wt[4:5] = NA
mtcars$wt[6] = -1
mtcars$wt[15] = -1
mtcars$wt[16] = 0
mtcars$wt[20] = NA


# write_labelled_spss(mtcars, "mtcars.csv")


res = mtcars %>% tab_cells(mpg, qsec, hp, disp) %>% 
    tab_stat_mean() %>% 
    tab_stat_median() %>% 
    tab_stat_sd() %>% 
    tab_stat_sum() %>% 
    tab_stat_se() %>% 
    tab_stat_unweighted_valid_n() %>% 
    tab_stat_valid_n() %>% 
    tab_stat_min() %>% 
    tab_stat_max() %>% 
    tab_pivot(stat_position = "inside_columns")
    
expect_equal_to_reference(res, "rds/ctable19.rds")

res = mtcars %>% 
    tab_weight(wt) %>% 
    tab_cells(mpg, qsec, hp, disp) %>% 
    tab_weight() %>% 
    tab_stat_mean() %>% 
    tab_stat_median() %>% 
    tab_stat_sd() %>% 
    tab_stat_sum() %>% 
    tab_stat_se() %>% 
    tab_stat_unweighted_valid_n() %>% 
    tab_stat_valid_n() %>% 
    tab_stat_min() %>% 
    tab_stat_max() %>% 
    tab_pivot(stat_position = "inside_columns")

expect_equal_to_reference(res, "rds/ctable19.rds")



res = mtcars %>% 
    tab_cells(mpg, qsec, hp, disp) %>% 
    tab_weight(wt) %>% 
    tab_stat_mean() %>% 
    tab_stat_median() %>% 
    tab_stat_sd() %>% 
    tab_stat_sum() %>% 
    tab_stat_se() %>% 
    tab_stat_unweighted_valid_n() %>% 
    tab_stat_valid_n() %>% 
    tab_stat_min() %>% 
    tab_stat_max() %>% 
    tab_pivot(stat_position = "inside_columns")

expect_equal_to_reference(res, "rds/ctable20.rds")

res = mtcars %>% 
    tab_cells(mpg, qsec, hp, disp) %>% 
    tab_subgroup(!is.na(wt) & wt>0) %>% 
    tab_weight(wt) %>% 
    tab_stat_mean() %>% 
    tab_stat_median() %>% 
    tab_stat_sd() %>% 
    tab_stat_sum() %>% 
    tab_stat_se() %>% 
    tab_stat_unweighted_valid_n() %>% 
    tab_stat_valid_n() %>% 
    tab_stat_min() %>% 
    tab_stat_max() %>% 
    tab_pivot(stat_position = "inside_columns")
    
expect_equal_to_reference(res, "rds/ctable20.rds")

res = mtcars %>% 
    tab_subgroup(!is.na(wt) & wt>0) %>% 
    tab_cells(mpg, qsec, hp, disp) %>% 
    tab_weight(wt) %>% 
    tab_stat_mean() %>% 
    tab_stat_median() %>% 
    tab_stat_sd() %>% 
    tab_stat_sum() %>% 
    tab_stat_se() %>% 
    tab_stat_unweighted_valid_n() %>% 
    tab_stat_valid_n() %>% 
    tab_stat_min() %>% 
    tab_stat_max() %>% 
    tab_pivot(stat_position = "inside_columns")

expect_equal_to_reference(res, "rds/ctable20.rds")

res1 = dtfrm(a = c(1, 2, 3, 4, 5), b = c(5, 5, 1, 2, NA)) %>% 
    tab_cells(a, b) %>% 
    tab_mis_val(3:5) %>% 
    tab_stat_mean() %>% 
    tab_stat_median() %>% 
    tab_stat_sd() %>% 
    tab_stat_sum() %>% 
    tab_stat_se() %>% 
    tab_stat_unweighted_valid_n() %>% 
    tab_stat_valid_n() %>% 
    tab_stat_min() %>% 
    tab_stat_max() %>% 
    tab_pivot(stat_position = "inside_columns")

res2 = dtfrm(a = c(1, 2, 3, 4, 5), b = c(5, 5, 1, 2, NA)) %>% 
    tab_cells(a, b) %>% 
    tab_mis_val(gt(2)) %>% 
    tab_stat_mean() %>% 
    tab_stat_median() %>% 
    tab_stat_sd() %>% 
    tab_stat_sum() %>% 
    tab_stat_se() %>% 
    tab_stat_unweighted_valid_n() %>% 
    tab_stat_valid_n() %>% 
    tab_stat_min() %>% 
    tab_stat_max() %>% 
    tab_pivot(stat_position = "inside_columns")

res4 = dtfrm(a = c(1, 2, 3, 4, 5), b = c(5, 5, 1, 2, NA)) %>% 
    tab_cells(a, b) %>% 
    tab_mis_val(3 | gt(3)) %>% 
    tab_stat_mean() %>% 
    tab_stat_median() %>% 
    tab_stat_sd() %>% 
    tab_stat_sum() %>% 
    tab_stat_se() %>% 
    tab_stat_unweighted_valid_n() %>% 
    tab_stat_valid_n() %>% 
    tab_stat_min() %>% 
    tab_stat_max() %>% 
    tab_pivot(stat_position = "inside_columns")

res3 = dtfrm(a = c(1, 2, NA, NA, NA), b = c(NA, NA, 1, 2, NA)) %>% 
    tab_mis_val(1:2) %>% 
    tab_cells(a, b) %>% 
    tab_mis_val() %>% 
    tab_stat_mean() %>% 
    tab_stat_median() %>% 
    tab_stat_sd() %>% 
    tab_stat_sum() %>% 
    tab_stat_se() %>% 
    tab_stat_unweighted_valid_n() %>% 
    tab_stat_valid_n() %>% 
    tab_stat_min() %>% 
    tab_stat_max() %>% 
    tab_pivot(stat_position = "inside_columns")

expect_identical(res1, res2)
expect_identical(res1, res3)
expect_identical(res1, res4)

data("product_test")

codeframe_likes = num_lab("
1 Liked everything
                          2 Disliked everything
                          3 Chocolate
                          4 Appearance
                          5 Taste
                          6 Stuffing
                          7 Nuts
                          8 Consistency
                          98 Other
                          99 Hard to answer
                          ")

set.seed(1)
pr_t = compute(product_test, {
    # recode age by groups
    age_cat = if_val(s2a, lo %thru% 25 ~ 1, lo %thru% hi ~ 2)
    
    var_lab(c1) = "Preferences"
    val_lab(c1) = num_lab("
                           1 VSX123 
                           2 SDF456
                           3 Hard to say
                           ")
    
    var_lab(age_cat) = "Age"
    val_lab(age_cat) = c("18 - 25" = 1, "26 - 35" = 2)
    
    var_lab(a1_1) = "Likes. VSX123"
    var_lab(b1_1) = "Likes. SDF456"
    val_lab(a1_1) = codeframe_likes
    val_lab(b1_1) = codeframe_likes
    
    var_lab(a22) = "Overall quality. VSX123"
    var_lab(b22) = "Overall quality. SDF456"
    val_lab(a22) = num_lab("
                           1 Extremely poor 
                           2 Very poor
                           3 Quite poor
                           4 Neither good, nor poor
                           5 Quite good
                           6 Very good
                           7 Excellent
                           ")
    val_lab(b22) = val_lab(a22)
    wgt = runif(.N, 0.25, 4)
})

# write_labelled_spss(pr_t, "product_test.csv")
# options(expss.digits = 3)

expect_error(
pr_t %>% 
    tab_cells(mrset(a1_1 %to% a1_6), a22) %>% 
    tab_cols(mrset(b1_1 %to% b1_6), b22) %>% 
    tab_stat_cases(
        total_statistic = c("u_cases", "u_repsonses", "u_cpct", "u_cpct_repsonses", "u_rpct", "u_tpct",
                            "w_cases", "w_repsonses", "w_cpct", "w_cpct_repsonses", "w_rpct", "w_tpct"
                            )) %>% 
    tab_pivot()
)


res = pr_t %>% 
    tab_cells(mrset(a1_1 %to% a1_6), a22) %>% 
    tab_cols(mrset(b1_1 %to% b1_6), b22) %>% 
    tab_stat_cases(
        total_statistic = c("u_cases", "u_responses", "u_cpct", "u_rpct", "u_tpct",
                            "w_cases", "w_responses", "w_cpct", "w_rpct", "w_tpct"
        )) %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ctable20_1.rds"
)


res = pr_t %>% 
    tab_weight(wgt) %>% 
    tab_cells(mrset(a1_1 %to% a1_6), a22) %>% 
    tab_cols(mrset(b1_1 %to% b1_6), b22) %>% 
    tab_stat_cases(
        total_statistic = c("u_cases", "u_responses", "u_cpct", "u_rpct", "u_tpct",
                            "w_cases", "w_responses", "w_cpct", "w_rpct", "w_tpct"
        )) %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ctable21.rds"
)


res = pr_t %>% 
    tab_weight(wgt) %>% 
    tab_cells(mrset(a1_1 %to% a1_6), a22) %>% 
    tab_cols(mrset(b1_1 %to% b1_6), b22) %>% 
    tab_stat_cpct(
        total_statistic = c("u_cases", "u_responses", "u_cpct", "u_rpct", "u_tpct",
                            "w_cases", "w_responses", "w_cpct", "w_rpct", "w_tpct"
        )) %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ctable22.rds"
)

res = pr_t %>% 
    tab_weight(wgt) %>% 
    tab_cells(mrset(a1_1 %to% a1_6), a22) %>% 
    tab_cols(mrset(b1_1 %to% b1_6), b22) %>% 
    tab_stat_rpct(
        total_statistic = c("u_cases", "u_responses", "u_cpct", "u_rpct", "u_tpct",
                            "w_cases", "w_responses", "w_cpct", "w_rpct", "w_tpct"
        )) %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ctable23.rds"
)


res = pr_t %>% 
    tab_weight(wgt) %>% 
    tab_cells(mrset(a1_1 %to% a1_6), a22) %>% 
    tab_cols(mrset(b1_1 %to% b1_6), b22) %>% 
    tab_stat_tpct(
        total_statistic = c("u_cases", "u_responses", "u_cpct", "u_rpct", "u_tpct",
                            "w_cases", "w_responses", "w_cpct", "w_rpct", "w_tpct"
        )) %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ctable24.rds"
)

res = pr_t %>% 
    tab_weight(wgt) %>% 
    tab_cells(mrset(a1_1 %to% a1_6), a22) %>% 
    tab_cols(mrset(b1_1 %to% b1_6), b22) %>% 
    tab_stat_cpct_responses(
        total_statistic = c("u_cases", "u_responses", "u_cpct", "u_rpct", "u_tpct",
                            "w_cases", "w_responses", "w_cpct", "w_rpct", "w_tpct"
        )) %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ctable25.rds"
)


# pr_t %>%
#     tab_cols(total(), age_cat) %>%
#     tab_cells(unvr(mrset(a1_1 %to% a1_6))) %>%
#     tab_stat_cpct(label = var_lab(a1_1)) %>%
#     tab_cells(unvr(mrset(b1_1 %to% b1_6))) %>%
#     tab_stat_cpct(label = var_lab(b1_1)) %>%
#     tab_pivot(stat_position = "inside_columns")


#############

res = mtcars %>% 
    tab_cells(qsec, mpg) %>% 
    tab_stat_fun(my_fun = w_mean, w_median) %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ctable26.rds"
)

res = mtcars %>% 
    tab_cells(vars(qc(mpg, qsec, hp, disp))) %>% 
    tab_rows(am) %>% 
    tab_cols(vs) %>% 
    tab_stat_fun_df(w_cor) %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ctable27.rds"
)

res = mtcars %>% 
    tab_cells(vars(qc(mpg, qsec, hp, disp))) %>% 
    tab_rows(am %nest% vs) %>% 
    tab_stat_fun_df(w_cor) %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ctable28.rds"
)

res = mtcars %>% tab_cells() %>% 
    tab_rows() %>% 
    tab_cols() %>% 
    tab_stat_cases() %>% 
    tab_pivot()

expect_equal_to_reference(
    res,
    "rds/ctable29.rds"
)
    
    
context("custom tables error")

expect_error(
    mtcars %>% tab_stat_cases()
)

expect_error(
    mtcars %>% tab_stat_cpct()
)

expect_error(
    mtcars %>% tab_stat_cpct_responses()
)

expect_error(
    mtcars %>% tab_stat_rpct()
)

expect_error(
    mtcars %>% tab_stat_tpct()
)

expect_error(
    mtcars %>% tab_stat_fun()
)

expect_error(
    mtcars %>% tab_stat_fun_df()
)

expect_error(
    mtcars %>% tab_cells(am) %>% tab_pivot()
)

expect_error(
    mtcars %>% tab_pivot()
)

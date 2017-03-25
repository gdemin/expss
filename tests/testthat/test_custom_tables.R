context("custom tables")

data(mtcars)
# add labels to dataset
mtcars = apply_labels(mtcars, 
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      carb = NULL,
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
    tab_intermediate_pivot(stat_position = "inside_rows") %>% 
    tab_cells(total(vs, label = "#Total|cases")) %>% 
    tab_stat_cases(total_row_position = "none") %>% 
    tab_pivot() 

expect_equal_to_reference(res, "rds/ctable8.rds")

res = mtcars %>% 
    tab_subgroup(vs == 0) %>% 
    tab_cells(vs) %>% 
    tab_cols(total(), am) %>% 
    tab_stat_cpct(total_row_position = "none", label = "col %") %>%
    tab_stat_rpct(total_row_position = "none", label = "row %") %>%
    tab_stat_tpct(total_row_position = "none", label = "table %") %>%
    tab_intermediate_pivot(stat_position = "inside_rows") %>% 
    tab_cells(total(vs, label = "#Total|cases")) %>% 
    tab_stat_cases(total_row_position = "none") %>% 
    tab_pivot() 

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

# mtcars %>% 
#     tab_cells(cyl) %>% 
#     tab_cols(total(), am) %>%
#     tab_stat_cpct(total_row_position = "none", label = "col %") %>%
#     tab_stat_rpct(total_row_position = "none", label = "row %") %>%
#     tab_stat_tpct(total_row_position = "none", label = "table %") %>%
#     tab_pivot(stat_label_position = "inside_columns") %>% 
#     split_columns()
# 
# mtcars %>% 
#     tab_cells(cyl) %>% 
#     tab_cols(total(), am) %>%
#     tab_rows(vs) %>%
#     tab_stat_cpct(total_row_position = "none", label = "col %") %>%
#     tab_stat_rpct(total_row_position = "none", label = "row %") %>%
#     tab_stat_tpct(total_row_position = "none", label = "table %") %>%
#     tab_pivot(stat_label_position = "inside_rows") %>% 
#     tab_split_columns()



product_test %>% 
    tab_cols(c1) %>% 
    tab_cells(list(unvr(mrset(a1_1 %to% a1_6)))) %>% 
    tab_stat_cpct(label = var_lab(a1_1)) %>% 
    tab_cells(list(unvr(mrset(b1_1 %to% b1_6)))) %>% 
    tab_stat_cpct(label = var_lab(b1_1)) %>% 
    tab_pivot(stat_position = "inside_columns") 
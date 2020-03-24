## ---- message=FALSE, warning=FALSE--------------------------------------------
library(expss)
data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (1000 lbs)",
                      qsec = "1/4 mile time",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(vs) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_caption("Simple column percent")


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(vs) %>% 
    tab_rows(am) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_caption("Split by columns and rows")


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), vs, am) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_sort_desc() %>% 
    tab_caption("Multiple banners, table is sorted by total")


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), vs %nest% am) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_caption("Nested banners")


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(carb) %>% 
    tab_cols(total(), list(cyl, vs) %nest% am) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_caption("Multiple nested banners")


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(mpg, disp, hp, wt, qsec) %>%
    tab_cols(total(), am) %>% 
    tab_stat_fun(Mean = w_mean, "Std. dev." = w_sd, "Valid N" = w_n) %>%
    tab_pivot() %>% 
    tab_caption("Multiple variable and multiple summary statistics")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(mpg, disp, hp, wt, qsec) %>%
    tab_cols(total(), am) %>% 
    tab_stat_fun(Mean = w_mean, "Valid N" = w_n, method = list) %>%
    tab_pivot() %>% 
    tab_caption("Multiple variable and multiple summary statistics - statistic lables in columns")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_subgroup(am == 0) %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), vs %nest% am) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    drop_empty_columns() %>%
    tab_caption("Filter dataset and exclude empty columns")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), vs) %>% 
    tab_rows(am) %>% 
    tab_stat_cpct(total_row_position = "above",
                  total_label = c("number of cases", "row %"),
                  total_statistic = c("u_cases", "u_rpct")) %>% 
    tab_pivot() %>% 
    tab_caption("Total at the top of the table")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>%
    tab_cells(am) %>%
    tab_cols(total(), vs) %>%
    tab_total_row_position("none") %>% 
    tab_stat_cpct(label = "col %") %>%
    tab_stat_rpct(label = "row %") %>%
    tab_stat_tpct(label = "table %") %>%
    tab_pivot(stat_position = "inside_rows") %>% 
    tab_caption("Three different statistics in each cell - stat. labels in rows")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>%
    tab_cells(am) %>%
    tab_cols(total(), vs) %>%
    tab_total_row_position("none") %>% 
    tab_stat_cpct(label = "col %") %>%
    tab_stat_rpct(label = "row %") %>%
    tab_stat_tpct(label = "table %") %>%
    tab_pivot(stat_position = "inside_columns") %>% 
    tab_caption("Three different statistics in each cell - stat. labels in columns")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), am) %>% 
    tab_stat_mean() %>%
    tab_stat_se() %>% 
    tab_stat_valid_n() %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_caption("Stacked statistics")


## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), am) %>% 
    tab_row_label("#Summary statistics") %>% 
    tab_stat_mean() %>%
    tab_stat_se() %>% 
    tab_stat_valid_n() %>% 
    tab_row_label("#Column percent") %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_caption("Stacked statistics with section headings")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cols(total(), am) %>% 
    tab_cells(mpg, hp, qsec) %>% 
    tab_stat_mean() %>%
    tab_cells(cyl, carb) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    tab_caption("Stacked statistics - different statistics for different variables")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(sheet(mpg, disp, hp, wt, qsec)) %>% 
    tab_cols(total(), am) %>% 
    tab_stat_fun_df(
        function(x){
            frm = reformulate(".", response = as.name(names(x)[1]))
            model = lm(frm, data = x)
            sheet('Coef.' = coef(model), 
                  confint(model)
            )
        }    
    ) %>% 
    tab_pivot() %>% 
    tab_caption("Linear regression by groups")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(mpg) %>% 
    tab_cols(total(), vs) %>% 
    tab_rows(subtotal(cyl, 1:2, 3:4, "5 and more" = 5 %thru% hi)) %>% 
    tab_stat_mean() %>% 
    tab_pivot() %>% 
    tab_caption("Subtotals in rows")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(mpg, qsec) %>% 
    tab_cols(total(), vs) %>% 
    tab_rows(subtotal(cyl, 1:2, 3:4, "TOTAL 5 and more" = 5 %thru% hi, position = "bottom")) %>% 
    tab_stat_mean() %>% 
    tab_pivot() %>% 
    tab_caption("Subtotals at the bottom of the table")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(mpg) %>% 
    tab_cols(total(), vs) %>% 
    tab_rows(net(cyl, 1:2, 3:4, "NET 5 and more" = 5 %thru% hi, prefix = "NET ")) %>% 
    tab_stat_mean() %>% 
    tab_pivot() %>% 
    tab_caption("Nets in rows, custom prefix")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(net(mpg, "Low mpg" = less(mean(mpg)), "High mpg" = greater_or_equal(mean(mpg)))) %>% 
    tab_cols(total(), am) %>% 
    tab_stat_cases() %>% 
    tab_pivot() %>% 
    tab_caption("Nets with complex grouping")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), vs) %>% 
    tab_stat_cpct() %>% 
    tab_pivot() %>% 
    significance_cpct(compare_type = c("first_column", "subtable"), sig_level = 0.05) %>% 
    tab_caption("Significance testing on column percent")

## ---- message=FALSE, warning=FALSE--------------------------------------------
mtcars %>% 
    tab_cells(mpg, disp, hp, wt, qsec) %>%
    tab_cols(total(), am) %>% 
    tab_stat_mean_sd_n() %>%
    tab_pivot() %>% 
    significance_means(compare_type = c("first_column", "subtable")) %>% 
    tab_caption("Significance testing on means")

## ---- message=FALSE, warning=FALSE--------------------------------------------

data(product_test)
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
product_test = compute(product_test, {
    # recode age by groups
    age_cat = recode(s2a, lo %thru% 25 ~ 1, lo %thru% hi ~ 2)
    
    var_lab(age_cat) = "Age"
    val_lab(age_cat) = c("18 - 25" = 1, "26 - 35" = 2)
    
    var_lab(a1_1) = "Likes. VSX123"
    var_lab(b1_1) = "Likes. SDF456"
    val_lab(a1_1) = codeframe_likes
    val_lab(b1_1) = codeframe_likes
    
    wgt = runif(.N, 0.25, 4)
    wgt = wgt/sum(wgt)*.N
})

product_test %>% 
    tab_cells(mrset(a1_1 %to% a1_6), mrset(b1_1 %to% b1_6)) %>% 
    tab_cols(total(), age_cat) %>% 
    tab_weight(wgt) %>% 
    tab_stat_cpct() %>% 
    tab_sort_desc() %>% 
    tab_pivot() %>% 
    tab_caption("Multiple-response variables with weighting")

## ---- message=FALSE, warning=FALSE--------------------------------------------
product_test %>% 
    tab_cols(total(), age_cat) %>% 
    tab_weight(wgt) %>% 
    # '|' is needed to prevent automatic labels creation from argument
    tab_cells("|" = unvr(mrset(a1_1 %to% a1_6))) %>% 
    tab_stat_cpct(label = var_lab(a1_1)) %>% 
    tab_cells("|" = unvr(mrset(b1_1 %to% b1_6))) %>% 
    tab_stat_cpct(label = var_lab(b1_1)) %>% 
    tab_pivot(stat_position = "inside_columns") %>% 
    tab_caption("Side-by-side variables comparison")

## ---- message=FALSE, warning=FALSE, results='asis'----------------------------

# here we specify dataset and banner
banner = mtcars %>%
    tab_cols(total(), am)

for(each in colnames(mtcars)){
    # note ..$ which is used for indirect reference to variable
    # specify variable
    curr_table = banner %>% 
        tab_cells(..$each)
    # calculate statistics
    if(length(unique(mtcars[[each]]))>6){
        curr_table = curr_table %>% 
            tab_stat_mean_sd_n() %>% 
            tab_pivot() %>% 
            significance_means()
    } else {
        curr_table = curr_table %>% 
            tab_stat_cpct() %>% 
            tab_pivot() %>% 
            significance_cpct()
    }
    # finalize table
    curr_table %>% 
        tab_caption("Variable name: ", each) %>% 
        htmlTable() %>% 
        print()
}




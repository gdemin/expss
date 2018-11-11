## ---- message=FALSE, warning=FALSE---------------------------------------
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


## ------------------------------------------------------------------------
# 'cro' examples
# just simple crosstabulation, similar to base R 'table' function
cro(mtcars$am, mtcars$vs)

# Table column % with multiple banners
cro_cpct(mtcars$cyl, list(total(), mtcars$am, mtcars$vs))

# or, the same result with another notation
mtcars %>% calc_cro_cpct(cyl, list(total(), am, vs))

# Table with nested banners (column %).          
mtcars %>% calc_cro_cpct(cyl, list(total(), am %nest% vs))      


## ------------------------------------------------------------------------
# simple example
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), am) %>% 
    tab_stat_cpct() %>% 
    tab_pivot()

# table with caption
mtcars %>% 
    tab_cells(mpg, disp, hp, wt, qsec) %>%
    tab_cols(total(), am) %>% 
    tab_stat_mean_sd_n() %>%
    tab_last_sig_means(subtable_marks = "both") %>% 
    tab_pivot() %>% 
    set_caption("Table with summary statistics and significance marks.")

# Table with the same summary statistics. Statistics labels in columns.
mtcars %>% 
    tab_cells(mpg, disp, hp, wt, qsec) %>%
    tab_cols(total(label = "#Total| |"), am) %>% 
    tab_stat_fun(Mean = w_mean, "Std. dev." = w_sd, "Valid N" = w_n, method = list) %>%
    tab_pivot()

# Different statistics for different variables.
mtcars %>%
    tab_cols(total(), vs) %>%
    tab_cells(mpg) %>% 
    tab_stat_mean() %>% 
    tab_stat_valid_n() %>% 
    tab_cells(am) %>%
    tab_stat_cpct(total_row_position = "none", label = "col %") %>%
    tab_stat_rpct(total_row_position = "none", label = "row %") %>%
    tab_stat_tpct(total_row_position = "none", label = "table %") %>%
    tab_pivot(stat_position = "inside_rows") 

# Table with split by rows and with custom totals.
mtcars %>% 
    tab_cells(cyl) %>% 
    tab_cols(total(), vs) %>% 
    tab_rows(am) %>% 
    tab_stat_cpct(total_row_position = "above",
                  total_label = c("number of cases", "row %"),
                  total_statistic = c("u_cases", "u_rpct")) %>% 
    tab_pivot()

# Linear regression by groups.
mtcars %>% 
    tab_cells(sheet(mpg, disp, hp, wt, qsec)) %>% 
    tab_cols(total(label = "#Total| |"), am) %>% 
    tab_stat_fun_df(
        function(x){
            frm = reformulate(".", response = names(x)[1])
            model = lm(frm, data = x)
            sheet('Coef.' = coef(model), 
                  confint(model)
            )
        }    
    ) %>% 
    tab_pivot() 

## ------------------------------------------------------------------------

data(product_test)

w = product_test # shorter name to save some keystrokes

# here we recode variables from first/second tested product to separate variables for each product according to their cells
# 'h' variables - VSX123 sample, 'p' variables - 'SDF456' sample
# also we recode preferences from first/second product to true names
# for first cell there are no changes, for second cell we should exchange 1 and 2.
w = w %>% 
    do_if(cell == 1, {
        recode(a1_1 %to% a1_6, other ~ copy) %into% (h1_1 %to% h1_6)
        recode(b1_1 %to% b1_6, other ~ copy) %into% (p1_1 %to% p1_6)
        recode(a22, other ~ copy) %into% h22
        recode(b22, other ~ copy) %into% p22
        c1r = c1
    }) %>% 
    do_if(cell == 2, {
        recode(a1_1 %to% a1_6, other ~ copy) %into% (p1_1 %to% p1_6)
        recode(b1_1 %to% b1_6, other ~ copy) %into% (h1_1 %to% h1_6)
        recode(a22, other ~ copy) %into% p22
        recode(b22, other ~ copy) %into% h22
        recode(c1, 1 ~ 2, 2 ~ 1, other ~ copy) %into% c1r
    }) %>% 
    compute({
        # recode age by groups
        age_cat = recode(s2a, lo %thru% 25 ~ 1, lo %thru% hi ~ 2)
        # count number of likes
        # codes 2 and 99 are ignored.
        h_likes = count_row_if(1 | 3 %thru% 98, h1_1 %to% h1_6) 
        p_likes = count_row_if(1 | 3 %thru% 98, p1_1 %to% p1_6) 
    })

# here we prepare labels for future usage
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

overall_liking_scale = num_lab("
    1 Extremely poor 
    2 Very poor
    3 Quite poor
    4 Neither good, nor poor
    5 Quite good
    6 Very good
    7 Excellent
")

w = apply_labels(w, 
    c1r = "Preferences",
    c1r = num_lab("
        1 VSX123 
        2 SDF456
        3 Hard to say
    "),
    
    age_cat = "Age",
    age_cat = c("18 - 25" = 1, "26 - 35" = 2),
    
    h1_1 = "Likes. VSX123",
    p1_1 = "Likes. SDF456",
    h1_1 = codeframe_likes,
    p1_1 = codeframe_likes,
    
    h_likes = "Number of likes. VSX123",
    p_likes = "Number of likes. SDF456",
    
    h22 = "Overall quality. VSX123",
    p22 = "Overall quality. SDF456",
    h22 = overall_liking_scale,
    p22 = overall_liking_scale
)


## ------------------------------------------------------------------------
# 'tab_mis_val(3)' remove 'hard to say' from vector 
w %>% tab_cols(total(), age_cat) %>% 
      tab_cells(c1r) %>% 
      tab_mis_val(3) %>% 
      tab_stat_cases() %>% 
      tab_last_sig_cases() %>% 
      tab_pivot()
    

## ------------------------------------------------------------------------
# lets specify repeated parts of table creation chains
banner = w %>% tab_cols(total(), age_cat, c1r) 
# column percent with significance
tab_cpct_sig = . %>% tab_stat_cpct() %>% 
                    tab_last_sig_cpct(sig_labels = paste0("<b>",LETTERS, "</b>"))

# means with siginifcance
tab_means_sig = . %>% tab_stat_mean_sd_n(labels = c("<b><u>Mean</u></b>", "sd", "N")) %>% 
                      tab_last_sig_means(
                          sig_labels = paste0("<b>",LETTERS, "</b>"),   
                          keep = "means")

# Preferences
banner %>% 
    tab_cells(c1r) %>% 
    tab_cpct_sig() %>% 
    tab_pivot() 

# Overall liking
banner %>%  
    tab_cells(h22) %>% 
    tab_means_sig() %>% 
    tab_cpct_sig() %>%  
    tab_cells(p22) %>% 
    tab_means_sig() %>% 
    tab_cpct_sig() %>%
    tab_pivot() 

# Likes
banner %>% 
    tab_cells(h_likes) %>% 
    tab_means_sig() %>% 
    tab_cells(mrset(h1_1 %to% h1_6)) %>% 
    tab_cpct_sig() %>% 
    tab_cells(p_likes) %>% 
    tab_means_sig() %>% 
    tab_cells(mrset(p1_1 %to% p1_6)) %>% 
    tab_cpct_sig() %>%
    tab_pivot() 

# below more complicated table where we compare likes side by side
# Likes - side by side comparison
w %>% 
    tab_cols(total(label = "#Total| |"), c1r) %>% 
    tab_cells(list(unvr(mrset(h1_1 %to% h1_6)))) %>% 
    tab_stat_cpct(label = var_lab(h1_1)) %>% 
    tab_cells(list(unvr(mrset(p1_1 %to% p1_6)))) %>% 
    tab_stat_cpct(label = var_lab(p1_1)) %>% 
    tab_pivot(stat_position = "inside_columns") 


## ---- eval=FALSE---------------------------------------------------------
#  write_labelled_csv(w, file  filename = "product_test.csv")

## ---- eval=FALSE---------------------------------------------------------
#  write_labelled_spss(w, file  filename = "product_test.csv")

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
library(expss)
w = text_to_columns("
      A  B  C
      2 15 50
      1 70 80
      3 30 40
      2 30 40
")

knitr::kable(w, row.names = TRUE, format = "html", table.attr = "style='width:30%;'") 
knitr::asis_output("<br>")

## ---- results='hide', message=FALSE, warning=FALSE-----------------------
library(expss)
w = text_to_columns("
        a  b  c
        2 15 50
        1 70 80
        3 30 40
        2 30 40
")


## ---- eval=FALSE---------------------------------------------------------
#  w$d = ifelse(w$b>60, 1, 0)

## ---- eval=FALSE---------------------------------------------------------
#  w = compute(w, {
#      d = ifelse(b>60, 1, 0)
#      e = 42
#      abc_sum = sum_row(a, b, c)
#      abc_mean = mean_row(a, b, c)
#  })

## ---- eval=FALSE---------------------------------------------------------
#  count_if(1, w)

## ---- eval=FALSE---------------------------------------------------------
#  calculate(w, count_if(1, a, b, c))

## ---- eval=FALSE---------------------------------------------------------
#  w$d = count_row_if(gt(1), w)

## ---- eval=FALSE---------------------------------------------------------
#  w = compute(w, {
#      d = count_row_if(gt(1), a, b, c)
#  })
#  

## ---- eval=FALSE---------------------------------------------------------
#  count_col_if(le(1), w$a)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
library(expss)
tbl_crit = text_to_columns("
 Excel R
  <1 lt(1)
 <=1 le(1)
 <>1 ne(1)
  =1 eq(1)
 >=1 ge(1)
  >1 gt(1)
")

knitr::kable(tbl_crit, row.names = FALSE, 
             format = "html", 
             table.attr = "style='width:30%;'", 
             align = "r") 
knitr::asis_output("<br>")

## ---- eval=FALSE---------------------------------------------------------
#  sum(w, na.rm = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  w$d = mean_row(w)

## ---- eval=FALSE---------------------------------------------------------
#  w = compute(w, {
#      d = mean_row(a, b, c)
#  })
#  

## ---- eval=FALSE---------------------------------------------------------
#  sum_col(w$a)

## ---- eval=FALSE---------------------------------------------------------
#  sum_if(gt(40), w)

## ---- eval=FALSE---------------------------------------------------------
#  calculate(w, sum_if(gt(40), a, b, c))

## ---- eval=FALSE---------------------------------------------------------
#  w$d = sum_row_if(lt(40), w)

## ---- eval=FALSE---------------------------------------------------------
#  w = compute(w, {
#      d = sum_row_if(lt(40), a, b, c)
#  })
#  

## ---- eval=FALSE---------------------------------------------------------
#  mean_col_if(lt(3), w$a, data = w$b)

## ---- eval=FALSE---------------------------------------------------------
#  calculate(w, mean_col_if(lt(3), a, data = sheet(b, c)))

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
dict = text_to_columns("
    X  Y
    1  apples
    2  oranges
    3  peaches
")

knitr::kable(dict, align = "l", row.names = TRUE, format = "html", table.attr = "style='width:30%;'") 
knitr::asis_output("<br>")

## ---- eval=FALSE---------------------------------------------------------
#  dict = text_to_columns("
#      x  y
#      1  apples
#      2  oranges
#      3  peaches
#  ")

## ---- eval=FALSE---------------------------------------------------------
#  w$d = vlookup(w$a, dict, 2)

## ---- eval=FALSE---------------------------------------------------------
#  w$d = vlookup(w$a, dict, "y")

## ---- eval=FALSE---------------------------------------------------------
#  w$d = 1

## ---- results='hide', message=FALSE, warning=FALSE-----------------------
w = compute(w, {
    d = 1
})

## ---- eval=FALSE---------------------------------------------------------
#  w = compute(w, {
#      d = ifelse(a == 3, 2, NA)
#  })

## ---- eval=FALSE---------------------------------------------------------
#  w = compute(w, {
#      d = ifs(a == 3 ~ 2)
#  })

## ---- eval=FALSE---------------------------------------------------------
#  w = do_if(w, a>1, {
#      d = 4
#  })

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if(lo %thru% hi, a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if(NA, a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if(1 %thru% 5, a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if(1 %thru% hi, a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if(ge(1), a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if(lo %thru% 1, a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if (le(1), a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if(1 %thru% 5 | 99, a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if(c(1:5, NA), a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  recode(v1) = c(0 ~ 1, 1 ~ 0, 2:3 ~ -1, 9 ~ 9, other ~ NA)

## ---- eval=FALSE---------------------------------------------------------
#  recode(qvar) = c(1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, 11 %thru% hi ~ 3, other ~ 0)

## ---- eval=FALSE---------------------------------------------------------
#  recode(strngvar) = c(c('A', 'B', 'C') ~ 'A', c('D', 'E', 'F') ~ 'B', other ~ ' ')

## ---- eval=FALSE---------------------------------------------------------
#  voter = recode(age, NA ~ 9, 18 %thru% hi ~ 1, 0 %thru% 18 ~ 0)
#  # or
#  recode(age, NA ~ 9, 18 %thru% hi ~ 1, 0 %thru% 18 ~ 0) %into% voter

## ---- results='hide'-----------------------------------------------------
w = apply_labels(w,
                 a = "Fruits",
                 b = "Cost",
                 c = "Price"
)

## ---- results='hide'-----------------------------------------------------
w = apply_labels(w, 
                 a = num_lab("
                        1 apples
                        2 oranges
                        3 peaches 
                    ")
)

## ---- eval=FALSE---------------------------------------------------------
#  val_lab(w$a) = num_lab("
#      1 apples
#      2 oranges
#      3 peaches
#  ")
#  

## ------------------------------------------------------------------------
fre(w$a) # Frequency of fruits
cro_cpct(w$b, w$a) # Column percent of cost by fruits
cro_mean(sheet(w$b, w$c), w$a) # Mean cost and price by fruits


## ---- message=FALSE, warning=FALSE, include=FALSE------------------------
library(expss)

## ---- results='hide', message=FALSE, warning=FALSE-----------------------
w = read.csv(text = "
a,b,c
2,15,50
1,70,80
3,30,40
2,30,40"
)


## ---- eval=FALSE---------------------------------------------------------
#  w$d = ifelse(w$b>60, 1, 0)

## ---- eval=FALSE---------------------------------------------------------
#  w = modify(w, {
#      d = ifelse(b>60, 1, 0)
#      e = 42
#      abc_sum = sum_row(a, b, c)
#      abc_mean = mean_row(a, b, c)
#  })

## ---- eval=FALSE---------------------------------------------------------
#  count_if(1, w)

## ---- eval=FALSE---------------------------------------------------------
#  with(w, count_if(1, a, b, c))

## ---- eval=FALSE---------------------------------------------------------
#  w$d = count_row_if(gt(1), w)

## ---- eval=FALSE---------------------------------------------------------
#  w = modify(w, {
#      d = count_row_if(gt(1), a, b, c)
#  })
#  

## ---- eval=FALSE---------------------------------------------------------
#  count_col_if(lte(1), w$a)

## ---- eval=FALSE---------------------------------------------------------
#  sum(w, na.rm = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  w$d = mean_row(w)

## ---- eval=FALSE---------------------------------------------------------
#  w = modify(w, {
#      d = mean_row(a, b, c)
#  })
#  

## ---- eval=FALSE---------------------------------------------------------
#  sum_col(w$a)

## ---- eval=FALSE---------------------------------------------------------
#  sum_if(gt(40), w)

## ---- eval=FALSE---------------------------------------------------------
#  with(w, sum_if(gt(40), a, b, c))

## ---- eval=FALSE---------------------------------------------------------
#  w$d = sum_row_if(lt(40), w)

## ---- eval=FALSE---------------------------------------------------------
#  w = modify(w, {
#      d = sum_row_if(lt(40), a, b, c)
#  })
#  

## ---- eval=FALSE---------------------------------------------------------
#  mean_col_if(lt(3), w$a, data = w$b)

## ---- eval=FALSE---------------------------------------------------------
#  with(w, mean_col_if(lt(3), a, data = dtfrm(b, c)))

## ---- eval=FALSE---------------------------------------------------------
#  dict = read.csv(text = "
#  x,y
#  1,apples
#  2,oranges
#  3,peaches",
#  stringsAsFactors = FALSE
#  )

## ---- eval=FALSE---------------------------------------------------------
#  w$d = vlookup(w$a, dict, 2)

## ---- eval=FALSE---------------------------------------------------------
#  w$d = vlookup(w$a, dict, "y")

## ---- eval=FALSE---------------------------------------------------------
#  w$d = 1

## ---- results='hide', message=FALSE, warning=FALSE-----------------------
default_dataset(w)

.compute({
    d = 1
})

## ---- eval=FALSE---------------------------------------------------------
#  .compute({
#      d = ifelse(a == 3, 2, NA)
#  })

## ---- eval=FALSE---------------------------------------------------------
#  .compute({
#      d = ifs(a == 3 ~ 2)
#  })

## ---- eval=FALSE---------------------------------------------------------
#  .do_if(a>1, {
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
#  cnt = count_row_if(gte(1), a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if(lo %thru% 1, a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if (lte(1), a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if(1 %thru% 5 | 99, a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  cnt = count_row_if(c(1:5, NA), a1 %to% a5)

## ---- eval=FALSE---------------------------------------------------------
#  if_val(v1) = c(0 ~ 1, 1 ~ 0, 2:3 ~ -1, 9 ~ 9, . ~ NA)

## ---- eval=FALSE---------------------------------------------------------
#  if_val(qvar) = c(1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, 11 %thru% hi ~ 3, . ~ 0)

## ---- eval=FALSE---------------------------------------------------------
#  if_val(strngvar) = c(c('A', 'B', 'C') ~ 'A', c('D', 'E', 'F') ~ 'B', . ~ ' ')

## ---- eval=FALSE---------------------------------------------------------
#  voter = if_val(age, NA ~ 9, 18 %thru% hi ~ 1, 0 %thru% 18 ~ 0)

## ---- results='hide'-----------------------------------------------------
.compute({
    var_lab(a) = "Fruits"
    var_lab(b) = "Cost"
    var_lab(c) = "Price"
})

## ---- results='hide'-----------------------------------------------------
.compute({
    val_lab(a) = ml_left("
        1 apples
        2 oranges
        3 peaches 
    ")
})

## ---- eval=FALSE---------------------------------------------------------
#  val_lab(w$a) = ml_left("
#      1 apples
#      2 oranges
#      3 peaches
#  ")
#  

## ------------------------------------------------------------------------
fre(w$a) # Frequency of fruits
cro_cpct(w$b, w$a) # Column percent of cost by fruits
cro_mean(dtfrm(w$b, w$c), w$a) # Mean cost and price by fruits

## ---- eval=FALSE---------------------------------------------------------
#  .fre(a) # Frequency of fruits
#  .cro_cpct(b, a) # Column percent of cost by fruits
#  .cro_mean(dtfrm(b, c), a) # Mean cost and price by fruits

## ------------------------------------------------------------------------
library(expss)
library(knitr)
options(digits = 2) # for pretty printing
data(product_test)

default_dataset(product_test)

## here we recode variables from first/second tested product to separate variables for each product

# create empty variables - 'h' variables for VSX123 and 'p' variables for 'SDF456'
.set(c("h1_`1:6`","h22", "p1_`1:6`", "p22"))

# recode variables according to their cells
.recode(vars("h1_`1:6`","h22", "p1_`1:6`", "p22"), 
        cell == 1 ~ vars("a1_`1:6`","a22", "b1_`1:6`", "b22"),
        cell == 2 ~ vars("b1_`1:6`","b22", "a1_`1:6`", "a22")
)

# here we prepare likes codeframe for future usage
codeframe_likes = ml_left("
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

# recode preferences from first/second product to true names
# for first cell there are no changes, for second cell we should change 1 and 2.
.do_if(cell == 1, {
    c1r = c1
})
.do_if(cell == 2, {
    c1r = if_val(c1, 1 ~ 2, 2 ~ 1)
})
.compute({
    # recode age by groups
    age_cat = if_val(s2a, lo %thru% 25 ~ 1, lo %thru% hi ~ 2)
    # counter number of likes
    # codes 1, 3-98. 2 and 9 are ignored.
    h_likes = count_row_if(1 | 3 %thru% 98, h1_1 %to% h1_6) 
    p_likes = count_row_if(1 | 3 %thru% 98, p1_1 %to% p1_6) 
    
    # Apply labels

    var_lab(c1r) = "Preferences"
    val_lab(c1r) = ml_left("
        1 VSX123 
        2 SDF456
        3 Hard to say
    ")
    
    var_lab(age_cat) = "Age"
    val_lab(age_cat) = c("18 - 25" = 1, "26 - 35" = 2)
    
    var_lab(h1_1) = "Likes. VSX123"
    var_lab(p1_1) = "Likes. SDF456"
    val_lab(h1_1) = codeframe_likes
    val_lab(p1_1) = codeframe_likes
    
    var_lab(h_likes) = "Number of likes. VSX123"
    var_lab(p_likes) = "Number of likes. SDF456"
    
    var_lab(h22) = "Overall quality. VSX123"
    var_lab(p22) = "Overall quality. SDF456"
    val_lab(h22) = ml_left("
                           1 Extremely poor 
                           2 Very poor
                           3 Quite poor
                           4 Neither good, nor poor
                           5 Quite good
                           6 Very good
                           7 Excellent
                           ")
    val_lab(p22) = val_lab(h22)
})

# Tables. 
# 'kable' function just makes tables prettier in this document. 

# column percents.
kable(.fre(c1r))
# is there significant difference between preferences?
# '... %d% 3' remove 'hard to say' from vector 
.with(chisq.test(table(c1r %d% 3))) # yes, it is significant
kable(.cro_cpct(c1r, age_cat))
kable(.cro_cpct(h22, age_cat))
kable(.cro_cpct(p22, age_cat))
kable(.cro_cpct(h1_1 %to% h1_6, age_cat))
kable(.cro_cpct(p1_1 %to% p1_6, age_cat))
# means
kable(.cro_mean(dtfrm(h22, p22, h_likes, p_likes), age_cat))


## ---- eval=FALSE---------------------------------------------------------
#  write_labelled_csv(product_test, file  filename = "product_test.csv")

## ---- eval=FALSE---------------------------------------------------------
#  write_labelled_spss(product_test, file  filename = "product_test.csv")


# Introduction

*expss* package implements in R some popular functions from spreadsheets and SPSS Statistics software. See examples below. Package can be installed by typing `install.packages("expss")` in R/RStudio console. You can get help about any function by typing `?function_name` in the R console.

# Excel functions

Excel toy table:

|       | A   | B   | C    |
|:------|----:|----:|-----:|
| **1** |  2  | 15  |  50  |
| **2** |  1  | 70  |  80  |
| **3** |  3  | 30  |  40  |
| **4** |  2  | 30  |  40  |

Code for creating the same table in R:

```
w = read.csv(text = "
a,b,c
2,15,50
1,70,80
3,30,40
2,30,40"
)

```
`w` is the name of our table.

##### IF
_____

**Excel: `IF(B1>60, 1, 0)`**

**R:**
Here we create new column with name `d` with results.
```
w$d = ifelse(w$b>60, 1, 0)
```
If we need to use multiple transformations it is often convenient to use `modify` function. Inside `modify` we can put arbitrary number of statements:

```
w = modify(w, {
    d = ifelse(b>60, 1, 0)
    e = 42
    abc_sum = sum_row(a, b, c)
    abc_mean = mean_row(a, b, c)
})
```

##### COUNTIF
_______

Count 1's in entire dataset.

**Excel: `COUNTIF(A1:C4, 1)`**

**R:**
```
count_if(1, w)
```
or
```
with(w, count_if(1, a, b, c))
```    
Count values greater than 1 in each row of dataset.

**Excel: `COUNTIF(A1:C1, ">1")`**

**R**: 
```
w$d = count_row_if(gt(1), w)  
```
or

```
w = modify(w, {
    d = count_row_if(gt(1), a, b, c) 
})

```

Count values less than or equal to 1 in column A of dataset.

**Excel: `COUNTIF(A1:A4, "<=1")`**

**R**: 
```
count_col_if(lte(1), w$a)
```

**Table of criteria:**

| Excel   |   R       |
|---------|-----------|
| "<1"    |   lt(1)   |
| "<=1"   |   lte(1)  |
| "<>1"	  |   neq(1)  |
| "=1"    |   eq(1)   |
| ">=1"   |   gte(1)  |
| ">1"    |   gt(1)   |



##### SUM/AVERAGE
_______
Sum all values in dataset.

**Excel: `SUM(A1:C4)`**

**R:**
```
sum(w, na.rm = TRUE)
```

Calculate average of each row of dataset.

**Excel: `AVERAGE(A1:C1)`**

**R**: 
```
w$d = mean_row(w)  
```
or

```
w = modify(w, {
    d = mean_row(a, b, c) 
})

```

Sum values of column `A` of dataset.

**Excel: `SUM(A1:A4)`**

**R**: 
```
sum_col(w$a)
```

##### SUMIF/AVERAGE_IF
_______
Sum values greater than 40 in entire dataset.

**Excel: `SUMIF(A1:C4, ">40")`**

**R:**
```
sum_if(gt(40), w)
```
or
```
with(w, sum_if(gt(40), a, b, c))
```    
Sum values less than 40 in each row of dataset.

**Excel: `SUMIF(A1:C1, "<40")`**

**R**: 
```
w$d = sum_row_if(lt(40), w)  
```
or

```
w = modify(w, {
    d = sum_row_if(lt(40), a, b, c) 
})

```

Calculate average of `B` column with column `A` values less than 3.

**Excel: `AVERAGEIF(A1:A4, "<3", B1:B4)`**

**R**: 
```
mean_col_if(lt(3), w$a, data = w$b)
```
or, if we want calculate means for both `b` and `c` columns:
```
with(w, mean_col_if(lt(3), a, data = dtfrm(b, c)))
```

##### VLOOKUP
_______
Our dictionary for lookup:

|       | X   | Y        |
|:------|----:|:---------|
| **1** |  1  | apples   |
| **2** |  2  | oranges  |
| **3** |  3  | peaches  |


Code for creating the same dictionary in R:

```
dict = read.csv(text = "
x,y
1,apples
2,oranges
3,peaches",
stringsAsFactors = FALSE
)
```

**Excel: `VLOOKUP(A1, $X$1:$Y$3, 2, FALSE)`**

**R**: 
```
w$d = vlookup(w$a, dict, 2)
```
or, we can use column names:
```
w$d = vlookup(w$a, dict, "y")
```

# SPSS functions

##### COMPUTE
_______
**SPSS**:
```
COMPUTE d = 1.
```

**R**:
```
w$d = 1
```
or, in specific data.frame

```
default_dataset(w)

.compute({
    d = 1
})
```
There can be arbitrary number of statements inside `.compute`.

##### IF
_______
**SPSS**:
```
IF(a = 3) d = 2.
```

**R**:

Default dataset should be already predefined as in previous example.
```
.compute({
    d = ifelse(a == 3, 2, NA)
})
```
or,

```
.compute({
    d = ifs(a == 3 ~ 2)
})
```

##### DO IF
_______
**SPSS**:
```
DO IF (a>1).
    COMPUTE d = 4.
END IF.
```

**R**:
```
.do_if(a>1, {
    d = 4
})
```

There can be arbitrary number of statements inside `.do_if`.

##### COUNT
_______
**SPSS**:
```
COUNT cnt = a1 TO a5 (LO THRU HI).
```
**R**:
```
cnt = count_row_if(lo %thru% hi, a1 %to% a5)
```

**SPSS**:
```
COUNT cnt = a1 TO a5 (SYSMIS).
```
**R**:
```
cnt = count_row_if(NA, a1 %to% a5)
```

**SPSS**:
```
COUNT cnt = a1 TO a5 (1 THRU 5).
```
**R**:
```
cnt = count_row_if(1 %thru% 5, a1 %to% a5)
```

**SPSS**:
```
COUNT cnt = a1 TO a5 (1 THRU HI).
```
**R**:
```
cnt = count_row_if(1 %thru% hi, a1 %to% a5)
```
or,
```
cnt = count_row_if(gte(1), a1 %to% a5)
```

**SPSS**:
```
COUNT cnt = a1 TO a5 (LO THRU 1).
```
**R**:
```
cnt = count_row_if(lo %thru% 1, a1 %to% a5)
```
or,
```
cnt = count_row_if (lte(1), a1 %to% a5)
```

**SPSS**:
```
COUNT cnt = a1 TO a5 (1 THRU 5, 99).
```
**R**:
```
cnt = count_row_if(1 %thru% 5 | 99, a1 %to% a5)
```

**SPSS**:
```
COUNT cnt = a1 TO a5(1,2,3,4,5, SYSMIS).
```

**R**:
```
cnt = count_row_if(c(1:5, NA), a1 %to% a5)
```

`count_row_if` can be used with default dataset inside the `.compute`.

##### RECODE
_______
**SPSS**:
```
RECODE V1 (0=1) (1=0) (2, 3=-1) (9=9) (ELSE=SYSMIS)
```
**R**:
```
if_val(v1) = c(0 ~ 1, 1 ~ 0, 2:3 ~ -1, 9 ~ 9, . ~ NA)
```
**SPSS**:
```
RECODE QVAR(1 THRU 5=1)(6 THRU 10=2)(11 THRU HI=3)(ELSE=0).
```
**R**:
```
if_val(qvar) = c(1 %thru% 5 ~ 1, 6 %thru% 10 ~ 2, 11 %thru% hi ~ 3, . ~ 0)
```
**SPSS**:
```
RECODE STRNGVAR ('A', 'B', 'C'='A')('D', 'E', 'F'='B')(ELSE=' '). 
```
**R**:
```
if_val(strngvar) = c(c('A', 'B', 'C') ~ 'A', c('D', 'E', 'F') ~ 'B', . ~ ' ')
```
**SPSS**:
```
RECODE AGE (MISSING=9) (18 THRU HI=1) (0 THRU 18=0) INTO VOTER. 
```
**R**:
```
voter = if_val(age, NA ~ 9, 18 %thru% hi ~ 1, 0 %thru% 18 ~ 0)
```

`if_val` can be used with default dataset inside the `.compute`.

##### VARIABLE LABELS
_______
**SPSS**:
```
VARIABLE LABELS a "Fruits"
                b "Cost"
                c "Price".
```
**R**:
```
.compute({
    var_lab(a) = "Fruits"
    var_lab(b) = "Cost"
    var_lab(c) = "Price"
})
```

##### VALUE LABELS
_______
**SPSS**:
```
VALUE LABELS a
    1 "apples"
    2 "oranges"
    3 "peaches". 
```

**R**:
```
.compute({
    val_lab(a) = ml_left("
        1 apples
        2 oranges
        3 peaches 
    ")
})
```
or, without using default dataset:
```
val_lab(w$a) = ml_left("
    1 apples
    2 oranges
    3 peaches 
")

```
##### Simple tables
_______
**R**:
```{r}
fre(w$a) # Frequency of fruits
cro_cpct(w$b, w$a) # Column percent of cost by fruits
cro_mean(dtfrm(w$b, w$c), w$a) # Mean cost and price by fruits
```
or, the same with default dataset:
```
.fre(a) # Frequency of fruits
.cro_cpct(b, a) # Column percent of cost by fruits
.cro_mean(dtfrm(b, c), a) # Mean cost and price by fruits
```


# Session example

It is rather artificial dataset with data from product test of two samples of
chocolate sweets. 150 respondents tested two kinds of sweets (codenames:
VSX123 and SDF546). Sample was divided into two groups (cells) of 75
respondents in each group. In cell 1 product VSX123 was presented first and
then SDF546. In cell 2 sweets were presented in reversed order. Questions
about respondent impressions about first product are in the block A (and
about second tested product in the block B). At the end of the questionnaire 
there is a question about preferences between sweets.

List of variables:

- `id` Respondent Id
- `cell` First tested product (cell number)
- `s2a` Age
- `a1_1-a1_6` What did you like in these sweets? Multiple response. First tested product
- `a22` Overall quality. First tested product
- `b1_1-b1_6` What did you like in these sweets? Multiple response. Second tested product
- `b22` Overall quality. Second tested product
- `c1` Preferences

```{r}
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

```

We can save labelled dataset as *.csv file with accompanying R code for labelling.

```
write_labelled_csv(product_test, file  filename = "product_test.csv")
```

Or, we can save dataset as *.csv file with SPSS syntax to read data and apply labels.

```
write_labelled_spss(product_test, file  filename = "product_test.csv")
```









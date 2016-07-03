## Excel functions

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

**Excel: `IF(B1>60, 1, 0)`**

**R:**
Here we create new column with name `d` with results.
```
w$d = ifelse(w$b>60, 1, 0)
```
If we need to use multiple transformations it is often convenient to use `modify` function. Inside `modify` we can put arbitrary number of statements.

```
w = modify(w, {
    d = ifelse(b>60, 1, 0)
})
```

##### COUNTIF

Count 1's in entire dataset.

**Excel: `COUNTIF(A1:С4, 1)`**

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

Table of criteria:

| Excel   |   R       |
|---------|-----------|
| "<1"    |   lt(1)   |
| "<=1"   |   lte(1)  |
| "<>1"	  |   neq(1)  |
| "=1"    |   eq(1)   |
| ">=1"   |   gte(1)  |
| ">1"    |   gt(1)   |



##### SUM/AVERAGE
Sum all values in dataset.

**Excel: `SUM(A1:С4)`**

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
Sum values greater than 40 in entire dataset.

**Excel: `SUMIF(A1:С4, ">40")`**

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

## SPSS functions

##### COMPUTE

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
There are can be arbitrary number of statements inside `.compute`.

##### IF

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

There are can be arbitrary number of statements inside `.do_if`.

##### COUNT

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
**R**:
```
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


## Session example

загрузка
групировка возраста
перекодировка 
метки
подсчет лайков и дислайков
разбивка по возрасту предпочтений, лайков и дислайков
средние оценки и кол-во лайков/дислайков




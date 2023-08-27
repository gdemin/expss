## examples form Microsoft Excel

context("excel sumif")

dfs = read.csv(
    text = "
    property_value,commission,data
    100000,7000,250000
    200000,14000,	
    300000,21000,	
    400000,28000,"
)

expect_equal(
    with(dfs, sum_if(gt(160000), property_value, data = commission)),
    63000
)

expect_equal(
    with(dfs, sum_if(gt(160000), property_value)),
    900000
)

expect_equal(
    with(dfs, sum_if(300000, property_value, data = commission)),
    21000
)

expect_equal(
    with(dfs, sum_if(gt(data[1]), property_value, data = commission)),
    49000
)

dfs = read.csv(
    text = "
category,food,sales
Vegetables,Tomatoes,2300
Vegetables,Celery,5500
Fruits,Oranges,800
,Butter,400
Vegetables,Carrots,4200
Fruits,Apples,1200"
    ,stringsAsFactors = FALSE
)

expect_equal(
    with(dfs, sum_if("Fruits", category, data = sales)),
    2000
)

expect_equal(
    with(dfs, sum_if("Vegetables", category, data = sales)),
    12000
)

expect_equal(
    with(dfs, sum_if(function(x) grepl("es$", x), food, data = sales)),
    4300
)

expect_equal(
    with(dfs, sum_if("", category, data = sales)),
    400
)

dfs = read.csv(
    text = "
quantity_sold,product,salesperson
5,Apples,1
4,Apples,2
15,Artichokes,1
3,Artichokes,2
22,Bananas,1
12,Bananas,2
10,Carrots,1
33,Carrots,2"
    ,stringsAsFactors = FALSE
)

### too complex and useless
expect_equal(
    with(dfs, 
         sum_if(when(grepl("^A",product) & salesperson==1), quantity_sold)
         ),
    20
)

expect_equal(
    with(dfs, sum(quantity_sold[grepl("^A",product) & salesperson==1])),
    20
)




context("excel averageif")

dfs = read.csv(
    text = "
    property_value,commission,data
    100000,7000,250000
    200000,14000,	
    300000,21000,	
    400000,28000,"
)

expect_equal(
    with(dfs, mean_if(lt(23000), commission)),
    14000
)

expect_equal(
    with(dfs, mean_if(lt(95000), property_value)),
    NaN
)

expect_equal(
    with(dfs, mean_if(gt(250000), property_value, data = commission)),
    24500
)


dfs = read.csv(
    text = '
region,profits
East,45678
West,23789
North,-4789
South (New Office),0
MidWest,9678'
    ,stringsAsFactors = FALSE
)


expect_equal(
    with(dfs, mean_if(fixed("West"), region, data = profits)),
    16733.5
)

expect_equal(
    with(dfs, mean_if(!fixed("(New Office)"), region, data = profits)),
    18589
)


context("excel minif/maxif")

dfs = read.csv(
    text = '
grade,weight 
89,1
93,2
96,2
85,3
91,1
88,1'
    ,stringsAsFactors = FALSE
)

expect_equal(
    with(dfs, min_if(1, weight, data = grade)),
    88
)

expect_equal(
    with(dfs, max_if(1, weight, data = grade)),
    91
)

dfs = read.csv(
    text = '
weight,grade 
10,b
11,a
100,a
111,b
1,a
1,a'
    ,stringsAsFactors = FALSE
)

expect_equal(
    with(dfs, min_if("a", grade[2:5], data = weight[1:4])),
    10
)

expect_error(
    with(dfs, min_if("a", grade[2:5], data = weight[1:5]))
)

dfs = read.csv(
    text = '
weight,grade 
10,b
1,a
100,a
1,b
1,a
1,a'
    ,stringsAsFactors = FALSE
)

expect_equal(
    with(dfs, max_if("a", grade[2:5], data = weight[1:4])),
    10
)

expect_equal(
    with(dfs, max_if("e", grade[2:5], data = weight[1:4])),
    NA_real_
)

context("median/sd if")

dfs = read.csv(
    text = "
    property_value,commission,data
    100000,7005,250045
    200000,14000,	
    300000,21001,	
    400001,28001,"
)

mat_dfs = as.matrix(dfs)
expect_equal(
    with(dfs, median_if(lt(23000), commission)),
    median(mat_dfs[mat_dfs<23000], na.rm = TRUE)
)

expect_equal(
    with(dfs, median_if(lt(95000), property_value)),
    as.integer(NA)
)

expect_equal(
    with(dfs, median_if(gt(250000), property_value, data = commission)),
    median(mat_dfs[mat_dfs[,1]>250000, 2], na.rm = TRUE)
)

expect_equal(
    with(dfs, sd_if(lt(23000), commission)),
    sd(mat_dfs[mat_dfs<23000], na.rm = TRUE)
)

expect_equal(
    with(dfs, sd_if(lt(95000), property_value)),
    as.integer(NA)
)

expect_equal(
    with(dfs, sd_if(gt(250000), property_value, data = commission)),
    sd(mat_dfs[mat_dfs[,1]>250000, 2], na.rm = TRUE)
)

data(iris)

test_iris = iris[,-5]
expect_equal(
    median_row_if(when(1:150<75), test_iris),
    ifelse(1:150<75, apply(test_iris, 1, median, na.rm = TRUE), NA)
)

expect_equal(
   sd_row_if(when(1:150<75), test_iris),
    ifelse(1:150<75, apply(test_iris, 1, sd, na.rm = TRUE), NA)
)

test_iris2 = test_iris
test_iris2[1:150>74, ] = NA
expect_identical(
    median_col_if(when(1:150<75), test_iris),
    apply(test_iris2, 2, median, na.rm = TRUE)
)

expect_equal(
    sd_col_if(when(1:150<75), test_iris),
    apply(test_iris2, 2, sd, na.rm = TRUE)
)


context("apply_if")

m_med = function(x) median(x, na.rm = TRUE)
m_sd = function(x) sd(x, na.rm = TRUE)
data(iris)

test_iris = iris[,-5]
expect_identical(
    apply_row_if(m_med, when(1:150<75), test_iris),
    ifelse(1:150<75, apply(test_iris, 1, median, na.rm = TRUE), NA)
)

expect_identical(
    apply_row_if(m_sd, when(1:150<75), test_iris),
    ifelse(1:150<75, apply(test_iris, 1, sd, na.rm = TRUE), NA)
)

test_iris2 = test_iris
test_iris2[1:150>74, ] = NA
expect_identical(
    apply_col_if(m_med, when(1:150<75), test_iris),
    apply(test_iris2, 2, median, na.rm = TRUE)
)

expect_identical(
    apply_col_if(m_sd, when(1:150<75), test_iris),
    apply(test_iris2, 2, sd, na.rm = TRUE)
)


bad_function = function(x) c(m_med(x), m_sd(x))

expect_error(
    apply_row_if(bad_function, when(1:150<75), test_iris)
)

expect_error(
    apply_col_if(bad_function,when(1:150<75), test_iris)
)






    
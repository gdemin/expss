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









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
         sum_if(list(grepl("^A",product),salesperson==1), quantity_sold)
         ),
    20
)

expect_equal(
    with(dfs, sum(quantity_sold[grepl("^A",product) & salesperson==1])),
    20
)

dfs = read.csv(
    text = '
daily_measurements,first_day,second_day,third_day,fourth_day
"Rain (total inches)",3.3,0.8,5.5,5.5
"Average temperature (degrees)",55,39,39,57.5
"Average wind speed (miles per hour)",6.5,19.5,6,6.5'
,stringsAsFactors = FALSE
)


expect_equal(
    sum_if(list(dfs[2, -1]>=40, dfs[3, -1]<10), dfs[1, -1]),
    8.8
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


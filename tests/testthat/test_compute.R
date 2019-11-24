context("compute")
suppressWarnings(RNGversion("3.5.0"))

aa = 10 %r% 5
a_ = 20 %r% 5
a_1 = 1 %r% 5
a_2 = 2 %r% 5
a_4 = 4 %r% 5
a_5 = 5 %r% 5

b_1 = -1 %r% 5
b_2 = -2 %r% 5
b_4 = -4 %r% 5
b_5 = -5 %r% 5 

dfs = data.frame(
    zz = 42,
    b_3 = 44,
    aa = 10 %r% 5,
    b_ = 20 %r% 5,
    b_1 = 11 %r% 5,
    b_2 = 12 %r% 5,
    b_4 = 14 %r% 5,
    b_5 = 15 %r% 5 
)

expect_error(compute(dfs, {.n = 42}))
expect_error(compute(dfs, {.N = 42}))




result_dfs = dfs
result_dfs$b_total = with(dfs, sum_row(b_1, b_2, b_4, b_5))
set.seed(1)
result_dfs$b_total = with(dfs, sum_row(b_1, b_2, b_4, b_5))
result_dfs$random_numer = runif(nrow(dfs))

dfs2 = dfs
dfs2$test = 1:5

result_dfs2 = dfs2
# result_dfs2$a_total = ifelse(dfs2$test %in% 2:4, sum_row(a_1, a_2, a_4, a_5), NA)
result_dfs2$b_total = ifelse(dfs2$test %in% 2:4, with(dfs, sum_row(b_1, b_2, b_4, b_5)), NA)
result_dfs2$aa = ifelse(dfs2$test %in% 2:4, result_dfs2$aa+1, result_dfs2$aa)

expect_identical(calc(dfs, sum_row(b_1 %to% b_5)), result_dfs$b_total)
expect_identical(dfs %calc% sum_row(b_1 %to% b_5), result_dfs$b_total)
expect_identical(dfs %calculate% sum_row(b_1 %to% b_5), result_dfs$b_total)

default_dataset(dfs)
expect_identical(.calc(sum_row(b_1 %to% b_5)), result_dfs$b_total)

set.seed(1)
expect_identical(calc(dfs, runif(.N)), result_dfs$random_numer)

set.seed(1)
expect_identical(
    compute(dfs, {
        b_total = sum_row(vars(b_1 %to% b_5))
        random_numer = runif(.n)
    }), 
    result_dfs
)

set.seed(1)
expect_identical(
    compute(dfs, 
        b_total = sum_row(vars(b_1 %to% b_5)),
        random_numer = runif(.n)
    ), 
    result_dfs
)

set.seed(1)
expect_identical(
    compute(dfs, 
            b_total <- sum_row(vars(b_1 %to% b_5)),
            random_numer <- runif(.n)
    ), 
    result_dfs
)

set.seed(1)
expect_identical(
    compute(dfs, 
            {b_total = sum_row(vars(b_1 %to% b_5))},
            {random_numer = runif(.n)}
    ), 
    result_dfs
)



set.seed(1)
expect_identical(
    compute(dfs, {
        b_total = sum_row(vars(from("b_1") & to("b_5")))
        random_numer = runif(.n)
    }), 
    result_dfs
)

set.seed(1)
expect_identical(
    compute(dfs, {
        b_total = sum_row(vars(b_1 %to% b_5))
        random_numer = runif(.n)
    }), 
    result_dfs
)



# Bad number of rows: 'random_numer' has 2 rows instead of 5 rows. 
expect_error(
    compute(dfs, {
        random_numer = runif(2)
    })
)





result_dfs$random_numer = NULL
expect_identical(
    compute(dfs, {
        b_total = sum_row(b_1 %to% b_5)
    }), 
    result_dfs
)


context("compute magrittr")

expect_identical(
    dfs %>% compute( {
        b_total = sum_row(vars(from("b_1") & to("b_5")))
    }), 
    result_dfs
)

expect_identical(
    dfs %>% compute( {
        b_total = sum_row(vars(b_1 %to% b_5))
    }), 
    result_dfs
)

expect_identical(
    dfs %>% compute( {
        b_total = sum_row(b_1 %to% b_5)
    }), 
    result_dfs
)




context("do_if")
set.seed(1)

result_dfs2[result_dfs2$test %in% 2:4, "random_numer"] = runif(3) 
set.seed(1)
expect_identical(
    do_if(dfs2, test %in% 2:4,
              {
                  b_total = sum_row(b_1 %to% b_5)
                  aa = aa + 1
                  random_numer = runif(.n)
              }), 
    result_dfs2
)

dt_dfs2 = data.table(dfs2)
dt_result_dfs2 = data.table(result_dfs2)
set.seed(1)
dt_dfs3 = do_if(dt_dfs2, test %in% 2:4,
          {
              b_total = sum_row(b_1 %to% b_5)
              aa = aa + 1
              random_numer = runif(.n)
          })

expect_identical(dt_dfs2, dt_result_dfs2)
expect_identical(dt_dfs3, dt_result_dfs2)

set.seed(1)
expect_identical(
    do_if(dfs2, 2:4,
              {
                  b_total = sum_row(b_1 %to% b_5)
                  aa = aa + 1
                  random_numer = runif(.n)
              }), 
    result_dfs2
)


set.seed(1)
expect_identical(
    do_if(dfs2, test %in% 2:4,
              {
                  b_total = sum_row(b_1 %to% b_5)
                  aa = aa + 1
                  random_numer = runif(.N)
              }), 
    result_dfs2
)


set.seed(1)
expect_identical(
    do_if(dfs2, test %in% 2:4,
              {
                  b_total = sum_row(b_1 %to% b_5)
                  aa = aa + 1
                  random_numer = runif(.N)
              }), 
    result_dfs2
)


expect_error(
    do_if(dfs2, test %in% 2:4,
              {
                  a_total = sum_row(a_1 %to% a_5)
                  b_total = sum_row(b_1 %to% b_5)
                  aa = aa + 1
              })
)
expect_error(do_if(dfs2,test %in% 2:4,  {.n = 42}))
expect_error(do_if(dfs2,test %in% 2:4,  {.N = 42}))



result_dfs2$random_numer = NULL



result_dfs2$b_total = NULL
dfs2$b_total = NULL
expect_identical(
    do_if(dfs2, test %in% 2:4,
              {
                  aa = aa + 1
              }), 
    result_dfs2
)


expect_identical(
    do_if(dfs2, 2:4,
              {
                  aa = aa + 1
              }), 
    result_dfs2
)

context("compute labels")

result_dfs3 = dfs
result_dfs3$b_total = with(dfs, sum_row(b_1, b_2, b_4, b_5))
var_lab(result_dfs3$aa) = "my label"
val_lab(result_dfs3$aa) = c(one = 1, two = 2)

expect_identical(
    compute(dfs, {
        b_total = sum_row(b_1, b_2, b_4, b_5)
        var_lab(aa) = "my label"
        val_lab(aa) = c(one = 1, two = 2)
        
        
    }),
    result_dfs3
)

expect_identical(
    compute(dfs, 
        b_total = sum_row(b_1, b_2, b_4, b_5),
        var_lab(aa) <- "my label",
        val_lab(aa) <- c(one = 1, two = 2),
        'var_lab(aa)' = "my label"
        
        
    ),
    sheet(result_dfs3, 'var_lab(aa)' = "my label")
)



result_dfs3$b_total = NULL
result_dfs3$aa = 1
expect_identical(
    compute(dfs, {
        b_total = NULL
        aa = 1
        
        
    }),
    result_dfs3
)

# doesn't work
# result_dfs3$b_total = NULL
# expect_identical(
#     do_if(dfs, 1:5 %in% 2:4, {
#         
#         var_lab(aa) = "my label"
#         val_lab(aa) = c(one = 1, two = 2)
# 
#     }),
#     result_dfs3
# )

data(iris)

iris2 = iris

iris2 = compute(iris2, {Species = NULL})

expect_identical(iris2, iris[,-5])



iris2 = iris

expect_error(do_if(iris2, Sepal.Length<5, {Species = NULL}))
expect_error(do_if(iris2, Sepal.Length<5, {Sepal.Width = 1:3}))




#### 
context("compute.list/do_if.list")

data(iris)
iris_list = split(iris, iris$Species)


res = lapply(iris_list, function(dfs) {dfs$aggr = sum(dfs$Sepal.Length); dfs})

res0 = compute(iris_list, {aggr = sum(Sepal.Length)})
expect_identical(res0, res)



res_calc = calc(iris_list, sum(Sepal.Length))
expect_identical(res_calc, lapply(iris_list, function(dfs){sum(dfs$Sepal.Length)}))



res = lapply(iris_list, function(dfs) {
    dfs$aggr = ifelse(dfs$Sepal.Width>mean(dfs$Sepal.Width), 1, NA)
    dfs
    })

res0 = do_if(iris_list, Sepal.Width>mean(Sepal.Width), {aggr = 1})
expect_identical(res0, res)


context("compute new_var")
# data(iris)
test_iris = iris
test_iris$new_var = NA
test_iris$new_logi = FALSE
test_iris$new_num = 0
test_iris$new_char = ""

res_iris = compute(iris, {
    new_var = .new_var()
    new_logi = .new_logical()
    new_num = .new_numeric()
    new_char = .new_character()
})

expect_identical(res_iris, test_iris)

test_iris = iris
test_iris[1:50, "new_var"] = NA
test_iris[1:50, "new_logi"] = FALSE
test_iris[1:50, "new_num"] = 0
test_iris[1:50, "new_char"] = ""

res_iris = do_if(iris, Species == "setosa", {
    new_var = .new_var()
    new_logi = .new_logical()
    new_num = .new_numeric()
    new_char = .new_character()
})

expect_identical(res_iris, test_iris)

context("use_labels")

data(mtcars)
mtcars = apply_labels(mtcars,
                   mpg = "Miles/(US) gallon",
                   cyl = "Number of cylinders",
                   disp = "Displacement (cu.in.)",
                   hp = "Gross horsepower",
                   drat = "Rear axle ratio",
                   wt = "Weight (lb/1000)",
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



expect_identical(
    use_labels(mtcars, summary(..data)), 
    summary(names2labels(mtcars))
)

res = with(mtcars, table(am, vs))
expect_identical(use_labels(unvr(mtcars), table(am, vs)), res)

names(dimnames(res)) = c(var_lab(mtcars$am), var_lab(mtcars$vs))
expect_identical(
    use_labels(mtcars, table(am, vs)), 
    res
)

default_dataset(mtcars)

expect_identical(
    .calculate(table(am, vs), use_labels = TRUE), 
    res
)

expect_identical(
    use_labels(iris, summary(Sepal.Length)), 
    summary(iris$Sepal.Length)
)

var_lab(mtcars$am) = "Duplicated label"
var_lab(mtcars$vs) = "Duplicated label"
names(dimnames(res)) = c("Duplicated label am",  "Duplicated label vs")
expect_identical(
    use_labels(mtcars, table(am, vs)), 
    res
)



context("do_if new")

data(iris)

iris2 = iris

iris2$nest = iris[,-5]
res = iris2
res$nest_sum[res$Species=="versicolor"] = rowSums(res$nest[res$Species=="versicolor", ])
res$nest[res$Species=="versicolor", ] = res$nest[res$Species=="versicolor", ]*3


wah = do_if(iris2, Species=="versicolor", {
    nest_sum = rowSums(nest)
    nest = nest*3
    
})

expect_identical(wah, res)

iris2 = iris
iris2$new_var = NA*1

expect_identical(
    do_if(iris, Sepal.Length>10, {
        Sepal.Width = Sepal.Length/Petal.Length
        new_var = 42
    }),
    iris2
)

expect_identical(
    do_if(data.table(iris), Sepal.Length>10, {
        Sepal.Width = Sepal.Length/Petal.Length
        new_var = 42
    }),
    data.table(iris2)
)

context("compute chain")
data(mtcars)
mtcars = unlab(mtcars)
res_mtcars = mtcars
res_mtcars$mpg2 = res_mtcars$mpg*2
res_mtcars$mpg4 = res_mtcars$mpg2*2
res_mtcars$am = NULL
expect_identical(compute(mtcars, 
        mpg2 = mpg*2,
        mpg4 = mpg2*2,
        am = NULL
        ),
        res_mtcars)

expect_identical(compute(mtcars, 
                         {mpg2 = mpg*2
                         mpg4 = mpg2*2
                         am = NULL}
),
res_mtcars)

expect_identical(compute(mtcars, 
                         mpg2 <- mpg*2,
                         mpg4 <- mpg2*2,
                         am = NULL
),
res_mtcars)

dt_mtcars = as.data.table(mtcars)
dt_res = as.data.table(res_mtcars)

expect_identical(compute(dt_mtcars, 
                         mpg2 = mpg*2,
                         mpg4 = mpg2*2,
                         am = NULL
),
dt_res)
dt_mtcars = as.data.table(mtcars)
expect_identical(compute(dt_mtcars, 
                         {mpg2 = mpg*2
                         mpg4 = mpg2*2
                         am = NULL}
),
dt_res)
dt_mtcars = as.data.table(mtcars)
expect_identical(compute(dt_mtcars, 
                         mpg2 <- mpg*2,
                         mpg4 <- mpg2*2,
                         am = NULL
),
dt_res)


context("do_if chain")
data(mtcars)
res_mtcars = mtcars
res_mtcars$mpg2[mtcars$cyl>4] = res_mtcars$mpg[mtcars$cyl>4]*2
res_mtcars$mpg4[mtcars$cyl>4] = res_mtcars$mpg2[mtcars$cyl>4]*2
expect_identical(do_if(mtcars, cyl>4, 
                       mpg2 = mpg*2,
                       mpg4 = mpg2*2
                       
),
res_mtcars)

expect_identical(do_if(mtcars, cyl>4, 
                       {mpg2 = mpg*2
                       mpg4 = mpg2*2
                       }
),
res_mtcars)

expect_identical(do_if(mtcars, cyl>4, 
                       mpg2 <- mpg*2,
                       mpg4 <- mpg2*2
),
res_mtcars)

dt_mtcars = as.data.table(mtcars)
dt_res = as.data.table(res_mtcars)

expect_identical(do_if(dt_mtcars, cyl>4, 
                       mpg2 = mpg*2,
                       mpg4 = mpg2*2
),
dt_res)
dt_mtcars = as.data.table(mtcars)
expect_identical(do_if(dt_mtcars, cyl>4, 
                       {mpg2 = mpg*2
                       mpg4 = mpg2*2
                       }
),
dt_res)
dt_mtcars = as.data.table(mtcars)
expect_identical(do_if(dt_mtcars, cyl>4, 
                       mpg2 <- mpg*2,
                       mpg4 <- mpg2*2
),
dt_res)
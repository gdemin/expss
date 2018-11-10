context("..")

data(iris)

test_iris = iris
test_iris$i1 = 10
test_iris$i2 = 20
test_iris$i3 = 30

res_iris = iris

res_iris = compute(iris,  
                   {
                       i = qc(i1, i2, i3)
                       value = c(10, 20, 30)
                       for(j in 1:3){
                           ..[i[j]] = value[j]
                       }
                       rm(i, value, j)
                   })



expect_identical(res_iris, test_iris)



k = 42
test_iris = iris
test_iris$i1 = 42
test_iris$i2 = 42
test_iris$i3 = 42


res_iris = compute(iris, 
                   {
                       for(i in qc(i1, i2, i3)){
                           ..$i = k
                       }
                       
                       rm(i)
                   })


expect_identical(res_iris, test_iris)



data(iris)

test_iris = iris
test_iris[, paste0(letters[1], seq_len(1))] = 1
test_iris[, paste0(letters[2], seq_len(2))] = 2
test_iris[, paste0(letters[3], seq_len(3))] = 3



res_iris = compute(iris, {
    for(i in c(1, 2, 3)){
        new_name = paste0(letters[i], seq_len(i))
        for(each in new_name) ..$each = i
    }
    rm(i, new_name, each)
})


expect_identical(res_iris, test_iris)


data(iris)

test_iris = iris
set.seed(123)
test_iris$i1 = runif(nrow(iris))
test_iris$i2 = runif(nrow(iris))
test_iris$i3 = runif(nrow(iris))


set.seed(123)
res_iris = compute(iris,  
                     {
                         for(i in qc(i1, i2, i3)) ..$i = runif(.N)
                         rm(i)
                     })

expect_identical(res_iris, test_iris)


test_iris = iris
test_iris$log = log(iris$Sepal.Length)
test_iris$exp = exp(iris$Sepal.Length)

res_iris = iris

i = qc(log, exp)
fun = c(log, exp)
res_iris = compute(res_iris, {

    for(j in 1:2) ..[i[j]] = fun[[j]](Sepal.Length)
    rm(j)
})

expect_identical(res_iris, test_iris)

test_iris = iris
test_iris$log = iris$Sepal.Length
test_iris$exp = iris$Sepal.Length

res_iris = iris
curr = "Sepal.Length"

res_iris = compute(res_iris, {
    
    for(j in 1:2) ..[i[j]] = ..$curr
    rm(j)
})

expect_identical(res_iris, test_iris)


data(iris)
test_iris = iris[, "Species", drop = FALSE]

to_delete = qc(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
res_iris = compute(iris, {
    for(i in to_delete){
        ..$i = NULL
    } 
    rm(i)
})

expect_identical(res_iris, test_iris)


context(".. internal")

data(iris)

test_iris = iris
test_iris$i3 = 100
test_iris$i1 = 10
test_iris$i2 = 20
test_iris$i4 = 30


name1 = "i1"
name2 = "i2"
name3 = "i4"

res_iris = test_iris[, c(name1, name2, name3)]
res_iris2 = test_iris
res_iris2$new = 60


expect_identical(
    keep(test_iris, ..$name1 %to% ..$name3),
    res_iris)
expect_identical(
    keep(test_iris, ..[(name1)] %to% ..[(name3)]),
    res_iris)
expect_identical(
    keep(test_iris, ..$name1, ..$name2, ..$name3),
    res_iris)
expect_identical(
    keep(test_iris, ..[(name1)], ..[(name2)], ..[(name3)]),
    res_iris)

expect_identical(
    keep(test_iris, ..$name1 %to% ..$name3, i3),
    sheet(res_iris, i3 = test_iris$i3))

expect_identical(
    calc(test_iris, ..$name1 %to% ..$name3),
    res_iris)

expect_identical(
    calc(test_iris, ..[(name1)] %to% ..[(name3)]),
    res_iris)

expect_identical(
    calc(test_iris, vars(..$name1 %to% ..$name3)),
    res_iris)

expect_identical(
    calc(test_iris, vars(..[(name1)] %to% ..[(name3)])),
    res_iris)

expect_identical(
    calc(test_iris, vars(..$name1, ..$name2, ..$name3)),
    res_iris)

expect_identical(
    calc(test_iris, vars(..[(name1)], ..[(name2)], ..[(name3)])),
    res_iris)

################

expect_identical(
    compute(test_iris, {
        new = sum_row(..$name1, ..$name2, ..$name3)
    }),
    res_iris2)

expect_identical(
    compute(test_iris, {
        new = sum_row(..[(name1), (name2), (name3)])
    }),
    res_iris2)

expect_identical(
    compute(test_iris, {
        new = sum_row(..[(name1)] %to% ..[(name3)])
    }),
    res_iris2)

expect_identical(
    compute(test_iris, {
        new = sum_row(vars(..$name1 %to% ..$name3))
    }),
    res_iris2)

expect_identical(
    compute(test_iris, {
        new = sum_row(vars(..[(name1)] %to% ..[(name3)]))
    }),
    res_iris2)

expect_identical(
    compute(test_iris, {
        new = sum_row(vars(..$name1, ..$name2, ..$name3))
    }),
    res_iris2)

expect_identical(
    compute(test_iris, {
        new = sum_row(vars(..[(name1)], ..[(name2)], ..[(name3)]))
    }),
    res_iris2)


#####
expect_identical(
    capture.output(print(..)), 'Object for variable substitution. Usage: `..$varname` or `..["varname"]`. '
)

a = "a"
class(a) = "internal_parameter"

expect_error({a$a = 5})
expect_error({a[a] = 5})


#####################

context(".. unboxing")

data(iris)
test_iris = iris
test_iris$a = 1
test_iris$b = 2
test_iris$d = 3

to_unbox = list(a = 1, b = 2, d = 3) 

res_iris = compute(iris, {
    ..[] = to_unbox
})

expect_identical(res_iris, test_iris)

to_unbox = sheet(a = 1, b = 2, d = 3) 

res_iris = compute(iris, {
    ..[] = to_unbox
})

expect_identical(res_iris, test_iris)

##########################

res_iris = compute(iris, {
    to_unbox %into% ..[]
})

expect_identical(res_iris, test_iris)

to_unbox = sheet(a = 1, b = 2, d = 3)

res_iris = compute(iris, {
    to_unbox %into% ..[]
})

expect_identical(res_iris, test_iris)

######################
res_iris = compute(iris, {
    to_unbox %into% ""
})

expect_identical(res_iris, test_iris)


to_unbox = sheet(a = 1, b = 2, d = 3)

res_iris = compute(iris, {
    to_unbox %into% ""
})

expect_identical(res_iris, test_iris)

#########

expect_error({..[] = 1:3})
expect_error({..[] = as.list(1:3)})
expect_error({..[] = list(a = 1, 2)})

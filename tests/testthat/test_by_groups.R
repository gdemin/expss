context("by_groups")

data(iris)
expect_error(by_groups(iris, "Species"))
expect_error(by_groups(iris, "Species", mean, b ~ sum(Sepal.Width)))
expect_error(by_groups(iris, "Species", mean, sum))
expect_error(by_groups(iris, "species", mean))
expect_equal(by_groups(iris, Species, mean), 
                 aggregate( . ~ Species , data = iris, FUN = mean))


expect_equal(by_groups(iris, fixed("Species"), mean), 
                 aggregate( . ~ Species , data = iris, FUN = mean))

var_name = "Species"
expect_equal(by_groups(iris, fixed(var_name), mean), 
                 aggregate( . ~ Species , data = iris, FUN = mean))

expect_equal(by_groups(iris, ..$var_name, mean), 
                 aggregate( . ~ Species , data = iris, FUN = mean))


expect_equal(by_groups(iris, ..[(var_name)], mean), 
                 aggregate( . ~ Species , data = iris, FUN = mean))

expect_equal(by_groups(iris[,-5], mean), as.data.frame(t(colMeans(iris[,-5]))))
expect_equal(by_groups(iris[,-5], character(0), mean), as.data.frame(t(colMeans(iris[,-5]))))
alpha = 3
expect_equal(by_groups(iris, Species, function(x){mean(x)}),
                 aggregate( . ~ Species , data = iris, FUN = function(x){mean(x)}))

expect_equal(by_groups(iris, Species,  ~ mean(Sepal.Width), ~ sum(Sepal.Length)),
                 as.data.frame(data.table::data.table(iris)[, list("mean(Sepal.Width)" = mean(Sepal.Width), "sum(Sepal.Length)" = sum(Sepal.Length)), by = "Species"]))

expect_equal(by_groups(iris, Species,  mean_width ~ mean(Sepal.Width),sum_length ~ sum(Sepal.Length)),
                 as.data.frame(data.table::data.table(iris)[, list(mean_width = mean(Sepal.Width), sum_length = sum(Sepal.Length)), by = "Species"]))

expect_equal(
    by_groups(as.list(iris), Species,  mean_width ~ mean(Sepal.Width),sum_length ~ sum(Sepal.Length)),
                 as.data.frame(data.table::data.table(iris)[, list(mean_width = mean(Sepal.Width), sum_length = sum(Sepal.Length)), by = "Species"]))


# expect_identical(by_groups(iris, Species,  mean_width ~ alpha*mean(Sepal.Width),sum_length ~ alpha*sum(Sepal.Length)),
#                  as.data.frame(data.table(iris)[, list(mean_width = alpha*mean(Sepal.Width), sum_length = alpha*sum(Sepal.Length)), by = "Species"]))


expect_equal(by_groups(iris,   ~ mean(Sepal.Width), ~ sum(Sepal.Length)),
                 as.data.frame(data.table::data.table(iris)[, list("mean(Sepal.Width)" = mean(Sepal.Width), "sum(Sepal.Length)" = sum(Sepal.Length))]))

expect_equal(by_groups(iris,  mean_width ~ mean(Sepal.Width), sum_length ~ sum(Sepal.Length)),
                 as.data.frame(data.table::data.table(iris)[, list(mean_width = mean(Sepal.Width), sum_length = sum(Sepal.Length))]))


data(mtcars)
dt_mtcars = data.table::data.table(mtcars)
# setkey(dt_mtcars, am, vs)
expect_equal(by_groups(mtcars, am, vs,  ~ mean(hp), ~ sum(mpg)),
                 as.data.frame(dt_mtcars[, list("mean(hp)" = mean(hp), "sum(mpg)" = sum(mpg)), by = "am,vs"]))

expect_equal(by_groups(mtcars, am, vs,  hp ~ mean(hp), mpg ~ sum(mpg)),
                 as.data.frame(dt_mtcars[, list(hp = mean(hp), mpg = sum(mpg)), by = "am,vs"]))

expect_equal(by_groups(mtcars, am, vs,  mean),
                 as.data.frame(dt_mtcars[, lapply(.SD, mean), by = "am,vs"]))


dt_iris = data.table::data.table(iris)

expect_error(by_groups(dt_iris, "Species"))
expect_error(by_groups(dt_iris, "Species", mean, b ~ sum(Sepal.Width)))
expect_error(by_groups(dt_iris, "species", mean))
expect_equal(by_groups(dt_iris, "Species", mean),
             data.table::as.data.table(aggregate( . ~ Species , data = iris, FUN = mean)))



expect_equal(by_groups(dt_iris, Species, function(x){mean(x)}),
             data.table::as.data.table(aggregate( . ~ Species , data = iris, FUN = function(x){mean(x)})))

# setkey(dt_iris, Species, verbose = FALSE)
expect_equal(by_groups(dt_iris, Species,  ~ mean(Sepal.Width), ~ sum(Sepal.Length)),
                 (dt_iris[, list("mean(Sepal.Width)" = mean(Sepal.Width), "sum(Sepal.Length)" = sum(Sepal.Length)), by = "Species"]))

expect_equal(by_groups(dt_iris, Species,  mean_width ~ mean(Sepal.Width),sum_length ~ sum(Sepal.Length)),
                 (dt_iris[, list(mean_width = mean(Sepal.Width), sum_length = sum(Sepal.Length)), by = "Species"]))

expect_equal(by_groups(dt_iris,   ~ mean(Sepal.Width), ~ sum(Sepal.Length)),
                 (dt_iris[, list("mean(Sepal.Width)" = mean(Sepal.Width), "sum(Sepal.Length)" = sum(Sepal.Length))]))

expect_equal(by_groups(dt_iris,  mean_width ~ mean(Sepal.Width), sum_length ~ sum(Sepal.Length)),
                 (dt_iris[, list(mean_width = mean(Sepal.Width), sum_length = sum(Sepal.Length))]))

dt_iris$Species = NULL

expect_equal(by_groups(dt_iris, mean), data.table::as.data.table(t(colMeans(iris[,-5]))))

context("by_groups variables")

data(mtcars)
dt_mtcars = data.table::data.table(mtcars)
# setkey(dt_mtcars, am, vs)
group_var = c( "am", "vs")
expect_error(by_groups(mtcars, group_var,  ~ mean(hp), ~ sum(mpg)))
expect_equal(by_groups(mtcars, (group_var),  ~ mean(hp), ~ sum(mpg)),
                 as.data.frame(dt_mtcars[, list("mean(hp)" = mean(hp), "sum(mpg)" = sum(mpg)), by = "am,vs"]))

formulas_var = c( ~ mean(hp), ~ sum(mpg))
expect_equal(by_groups(mtcars, (formulas_var), "am", "vs"),
                 as.data.frame(dt_mtcars[, list("mean(hp)" = mean(hp), "sum(mpg)" = sum(mpg)), by = "am,vs"]))

expect_equal(by_groups(mtcars, (group_var), (formulas_var)),
                 as.data.frame(dt_mtcars[, list("mean(hp)" = mean(hp), "sum(mpg)" = sum(mpg)), by = "am,vs"]))


fun =  mean
expect_equal(by_groups(mtcars, (group_var), (fun)),
             as.data.frame(dt_mtcars[, lapply(.SD, mean), by = "am,vs"]))

expect_equal(by_groups(mtcars, (group_var), fun),
             as.data.frame(dt_mtcars[, lapply(.SD, mean), by = "am,vs"]))



context("by_groups labels preserving")
data(mtcars)
mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"
    var_lab(vs) = "V/S"
    var_lab(am) = "Transmission (0 = automatic, 1 = manual)"
    val_lab(am) = c(" automatic" = 0, " manual" =  1)
    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
})

aggr = by_groups(mtcars, am, mean)
comp = sapply(colnames(aggr), function(each) var_lab(mtcars[[each]]) == var_lab(aggr[[each]]))
expect_identical(all(comp), TRUE)
expect_identical(val_lab(mtcars$am), val_lab(aggr$am))

aggr = by_groups(mtcars, am, hp ~ mean(hp))
comp = sapply(colnames(aggr), function(each) var_lab(mtcars[[each]]) == var_lab(aggr[[each]]))
expect_identical(all(comp), TRUE)
expect_identical(val_lab(mtcars$am), val_lab(aggr$am))

context("by_groups emtpy df")
expect_identical(
    by_groups(iris[FALSE,], Species, sss ~ mean(Sepal.Length)),
    data.frame(iris[FALSE, "Species", drop = FALSE], sss = numeric(0)))

expect_identical(
    by_groups(iris[FALSE,], Species, mean),
    iris[FALSE,][, c(5, 1:4)]
    )

########################

context("by_groups special")
data(iris)

expect_equal(by_groups(iris[,-5], character(0), mean), as.data.frame(t(colMeans(iris[,-5]))))

expect_equal(by_groups(iris, Species, function(x){mean(x)}),
                 aggregate( . ~ Species , data = iris, FUN = function(x){mean(x)}))




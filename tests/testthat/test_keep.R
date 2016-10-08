context("keep")

# c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

data(iris)
expect_identical(keep(iris, "Species", other), iris[, c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
expect_identical(keep(iris, "Species", perl("^Sepal")), iris[, c("Species", "Sepal.Length", "Sepal.Width")])
expect_identical(keep(iris, "Species", perl("Length$")), iris[, c("Species", "Sepal.Length", "Petal.Length")])
expect_identical(keep(iris, "Species", fixed("Length")), iris[, c("Species", "Sepal.Length", "Petal.Length")])
expect_identical(keep(iris, "Species", fixed("Length"), fixed("Width")), 
                 iris[, c("Species", "Sepal.Length", "Petal.Length", "Sepal.Width", "Petal.Width")])

expect_identical(keep(iris, qc(Species, Sepal.Length, Petal.Length)), iris[, c("Species", "Sepal.Length", "Petal.Length")])
expect_identical(iris %keep% qc(Species, Sepal.Length, Petal.Length), iris[, c("Species", "Sepal.Length", "Petal.Length")])
expect_identical(keep(iris, "Species"), iris[, c("Species"), drop = FALSE])
expect_error(keep(iris, "Species", "not_exists"))

expect_identical(keep(as.matrix(iris), "Species", perl("^Sepal")), as.matrix(iris)[, c("Species", "Sepal.Length", "Sepal.Width")])
expect_identical(keep(as.list(iris), "Species", perl("^Sepal")), as.list(iris)[c("Species", "Sepal.Length", "Sepal.Width")])
expect_identical(keep(setNames(1:5, colnames(iris)), "Species", perl("^Sepal")),
                 setNames(1:5, colnames(iris))[c("Species", "Sepal.Length", "Sepal.Width")])


context("except")
expect_identical(except(iris, "Species", other), iris[, FALSE, drop = FALSE])
expect_identical(except(iris, "Species"), iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
expect_identical(except(iris, "Species", perl("^Sepal")), iris[, c("Petal.Length", "Petal.Width")])
expect_identical(iris %except% c("Species", perl("^Sepal")), iris[, c("Petal.Length", "Petal.Width")])
expect_identical(except(iris, fixed("Length"), fixed("Width")), 
                 iris[, c("Species"), drop = FALSE])

expect_identical(except(iris, qc(Species, Sepal.Length, Petal.Length)), iris[, c("Sepal.Width", "Petal.Width")])
expect_error(except(iris, "Species", "not_exists"))

expect_identical(except(as.matrix(iris), "Species", perl("^Sepal")), as.matrix(iris)[, c("Petal.Length", "Petal.Width")])
expect_identical(except(as.list(iris), "Species", perl("^Sepal")), as.list(iris)[c("Petal.Length", "Petal.Width")])
expect_identical(except(setNames(1:5, colnames(iris)), "Species", perl("^Sepal")),
                 setNames(1:5, colnames(iris))[c("Petal.Length", "Petal.Width")])

data("airquality")
expect_identical(airquality %keep% from("Wind"), airquality[, c("Wind", "Temp", "Month", "Day")])
expect_identical(airquality %except% to("Wind"), airquality[, c("Temp", "Month", "Day")])
expect_identical(airquality %keep% (from("Ozone") & to("Wind")), airquality[, c("Ozone", "Solar.R", "Wind")])


context("keep default_dataset")
data(iris)
aaa = iris
default_dataset(aaa)
.keep("Species", other)
expect_identical(aaa, iris[, c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])

aaa = iris
.except("Species", other)
expect_identical(aaa, iris[, FALSE, drop = FALSE])

aaa = iris
.except("Species")
expect_identical(aaa, iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])


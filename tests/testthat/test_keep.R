context("keep")

# c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

data(iris)
expect_identical(keep(iris, items(5)), iris[, "Species", drop = FALSE])
expect_identical(except(iris, items(5)), iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
expect_identical(except(iris, items(1, 2, 3, 4)), iris[, "Species", drop = FALSE])
expect_identical(keep(iris, items(1, 2, 3, 4)), iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])

expect_identical(keep(iris, "Species", other), iris[, c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
expect_warning(keep(iris, "Species", other, "Species"))
expect_identical(keep(iris, "Species", perl("^Sepal")), iris[, c("Species", "Sepal.Length", "Sepal.Width")])
expect_identical(keep(iris, "Species", perl("Length$")), iris[, c("Species", "Sepal.Length", "Petal.Length")])
expect_identical(keep(iris, "Species", fixed("Length")), iris[, c("Species", "Sepal.Length", "Petal.Length")])
expect_identical(keep(iris, "Species", fixed("Length"), fixed("Width")), 
                 iris[, c("Species", "Sepal.Length", "Petal.Length", "Sepal.Width", "Petal.Width")])

expect_identical(keep(iris, qc(Species, Sepal.Length, Petal.Length)), iris[, c("Species", "Sepal.Length", "Petal.Length")])
expect_identical(keep(data.table::data.table(iris), qc(Species, Sepal.Length, Petal.Length)),
                 data.table::data.table(iris)[, c("Species", "Sepal.Length", "Petal.Length")])
expect_identical(iris %keep% qc(Species, Sepal.Length, Petal.Length), iris[, c("Species", "Sepal.Length", "Petal.Length")])
expect_identical(keep(iris, "Species"), iris[, c("Species"), drop = FALSE])
expect_identical(keep(iris, "Species"), iris[, c("Species"), drop = FALSE])
expect_error(keep(iris, "Species", "not_exists"))

expect_identical(keep(as.matrix(iris), "Species", perl("^Sepal")), 
                 as.matrix(iris)[, c("Species", "Sepal.Length", "Sepal.Width")])


context("except")
expect_identical(except(iris, "Species", other), iris[, FALSE, drop = FALSE])
expect_identical(except(iris, "Species"), iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
expect_identical(except(iris, "Species", perl("^Sepal")), iris[, c("Petal.Length", "Petal.Width")])
expect_identical(iris %except% c("Species", perl("^Sepal")), iris[, c("Petal.Length", "Petal.Width")])
expect_identical(except(iris, fixed("Length"), fixed("Width")), 
                 iris[, c("Species"), drop = FALSE])

expect_identical(except(iris, qc(Species, Sepal.Length, Petal.Length)), iris[, c("Sepal.Width", "Petal.Width")])
expect_identical(except(data.table::data.table(iris), qc(Species, Sepal.Length, Petal.Length)),
                 data.table::data.table(iris)[, c("Sepal.Width", "Petal.Width")])

expect_error(except(iris, "Species", "not_exists"))

expect_identical(except(as.matrix(iris), "Species", perl("^Sepal")), as.matrix(iris)[, c("Petal.Length", "Petal.Width")])

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

context("keep duplicted names")
data(iris)
ex_iris = iris[,-5]
colnames(ex_iris) = c("a", "a", "a", "a")

expect_identical(ex_iris %keep% "a", ex_iris)
expect_identical(ex_iris %except% "a", ex_iris[, FALSE, drop = FALSE])

context("keep/except %to%")
data(mtcars)
expect_identical(keep(mtcars, am %to% carb), keep(mtcars, from("am") & to("carb"))) 
expect_identical(keep(mtcars, am %to% carb, hp %to% drat),
                 keep(mtcars, from("am") & to("carb"), from("hp") & to("drat"))) 
expect_identical(except(mtcars, am %to% carb), except(mtcars, from("am") & to("carb"))) 
expect_identical(except(mtcars, am %to% carb, hp %to% drat),
                 except(mtcars, from("am") & to("carb"), from("hp") & to("drat"))) 

expect_identical(mtcars %keep% (am %to% carb), keep(mtcars, from("am") & to("carb"))) 
expect_identical(mtcars %keep% list(am %to% carb, hp %to% drat),
                 keep(mtcars, from("am") & to("carb"), from("hp") & to("drat"))) 
expect_identical(mtcars %except% (am %to% carb), except(mtcars, from("am") & to("carb"))) 
expect_identical(mtcars %except% list(am %to% carb, hp %to% drat),
                 except(mtcars, from("am") & to("carb"), from("hp") & to("drat"))) 

expect_error(keep(mtcars, am1 %to% carb))
expect_error(keep(mtcars, am %to% carb1))
expect_error(keep(mtcars, carb %to% am))


context("keep/except subst")
dfs = data.frame(
    aa = 10 %r% 5,
    b_ = 20 %r% 5,
    b_1 = 11 %r% 5,
    b_2 = 12 %r% 5,
    b_3 = 12 %r% 5,
    b_4 = 14 %r% 5,
    b_5 = 15 %r% 5 
)

aaa = paste0("b_", 1:5)
bbb = c("aa", "b_")
expect_identical(keep(dfs, subst("b_`1:5`")), dfs[, aaa])
expect_identical(dfs %keep% subst("b_`1:5`"), dfs[, aaa])

expect_identical(except(dfs, subst("b_`1:5`")), dfs[, bbb])
expect_identical(dfs %except% subst("b_`1:5`"), dfs[, bbb])

i = 1:5
expect_identical(keep(dfs, subst("b_`i`")), dfs[, aaa])
expect_identical(dfs %keep% subst("b_`i`"), dfs[, aaa])

expect_identical(except(dfs, subst("b_`i`")), dfs[, bbb])
expect_identical(dfs %except% subst("b_`i`"), dfs[, bbb])

ex1 = function(){
    items = 1:5
    keep(dfs, subst("b_`items`"))

}


ex2 = function(){
    items = 1:5
    dfs %keep% subst("b_`items`")

}

global_items = 2:3

ex3 = function(){
    dfs %keep% subst("b_`global_items`")
    
}

expect_identical(ex1(), dfs[, aaa])
expect_identical(ex2(), dfs[, aaa])
expect_identical(ex3(), dfs[, aaa[2:3]])
    
def_dfs = dfs
default_dataset(def_dfs)

.keep(subst("b_`i`"))
expect_identical(def_dfs, dfs[, aaa])

def_dfs = dfs
.except(subst("b_`i`"))
expect_identical(def_dfs, dfs[, bbb])

context("keep edge cases")

expect_identical(iris %keep% NULL, iris[, FALSE, drop = FALSE])
expect_identical(iris %except% NULL, iris)
expect_identical(as.matrix(iris) %except% NULL, as.matrix(iris))
# expect_identical(1:5 %except% NULL, 1:5)
# expect_identical(1:5 %keep% NULL, integer(0))

expect_identical(iris %keep% factor("Species"), iris[, 5, drop = FALSE])
expect_identical(iris %except% factor("Species"), iris[,-5])



context("keep/except list")

# expect_identical(keep(as.list(iris), "Species", perl("^Sepal")), as.list(iris)[c("Species", "Sepal.Length", "Sepal.Width")])
expect_error(keep(setNames(1:5, colnames(iris)), "Species", perl("^Sepal")))

# expect_identical(except(as.list(iris), "Species", perl("^Sepal")), as.list(iris)[c("Petal.Length", "Petal.Width")])
expect_error(except(setNames(1:5, colnames(iris)), "Species", perl("^Sepal")))

data(iris)

iris_list = list(iris[,-1], iris[,-5])

expect_identical(keep(iris_list,  Petal.Length %to% Petal.Width),
                 list(iris[,3:4], iris[,3:4])
                 )

expect_identical(except(iris_list,  Petal.Length %to% Petal.Width),
                 list(iris[,c(2,5)], iris[,1:2])
)






# curr = "Petal.Length"
# # 
# expect_identical(keep(iris_list,  curr),
#                  list(iris[,"Petal.Length", drop = FALSE], iris[,"Petal.Length", drop = FALSE])
# )
#     
# 
# expect_identical(except(iris_list,  curr),
#                  list(iris_list[[1]] %except% curr, iris_list[[2]] %except% curr)
# )





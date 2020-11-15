context("default_dataset")

data(iris)
aaa = iris
default_dataset(aaa)

expect_identical(ref(default_dataset()),aaa)



def = default_dataset()
ref(def)[,1] = NA

expect_identical(ref(default_dataset()),aaa)
expect_identical(all(is.na(aaa[,1])),TRUE)

data(mtcars)
bbb = mtcars
default_dataset(~bbb)

expect_identical(ref(default_dataset()),bbb)

default_dataset(NULL)
expect_error(default_dataset())

context("sort default_dataset")
data(iris)
def = iris
default_dataset(def)

.sort_asc(Sepal.Length)
expect_identical(def, iris[order(iris$Sepal.Length, decreasing = FALSE, na.last = FALSE), ])

.sort_desc(Sepal.Length)
expect_identical(def, iris[order(iris$Sepal.Length, decreasing = TRUE, na.last = TRUE), ])

.sort_asc(Sepal.Length, na.last = TRUE)
expect_identical(def, 
                 iris[order(iris$Sepal.Length, decreasing = FALSE, na.last = TRUE), ])

.sort_desc(Sepal.Length, na.last = FALSE)
expect_identical(def, 
                 iris[order(iris$Sepal.Length, decreasing = TRUE, na.last = FALSE), ])

context("do_repeat default_dataset")

data(iris)
def_iris = iris
default_dataset(def_iris)

test_iris = iris
test_iris$i1 = 10
test_iris$i2 = 20
test_iris$i3 = 30

res_iris = iris

.do_repeat(i = qc(i1, i2, i3), value = c(10, 20, 30),
           {
               i = value
           })

expect_identical(def_iris, test_iris)

k = 42
test_iris = iris
test_iris$i1 = 42
test_iris$i2 = 42
test_iris$i3 = 42

def_iris = iris
default_dataset(def_iris)
.do_repeat(i = qc(i1, i2, i3),
           {
               i = k
           })

expect_identical(def_iris, test_iris)


test_iris = iris
test_iris$i1 = 43
test_iris$i2 = 43
test_iris$i3 = 43

def_iris = iris
default_dataset(def_iris)
fff = function(){
    j = 43
    .do_repeat(i = qc(i1, i2, i3),
               {
                   i = j
               })
}
fff()
expect_identical(def_iris, test_iris)


context("where default_dataset")

data(iris)
d_iris = iris

default_dataset(d_iris)

.where(Species == "setosa")
expect_identical(d_iris, iris[iris$Species == "setosa", ])

d_iris = iris

## cond - special name which exists inside `where`
cond = "setosa"
.where(Species == cond)
expect_identical(d_iris, iris[iris$Species == "setosa", ])

.where(1:5)
expect_identical(d_iris, iris[1:5, ])


context("apply_labels default_dataset")

data(mtcars)
default_mtcars = mtcars
default_dataset(default_mtcars)

correct_mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"
    var_lab(vs) = "Engine"
    val_lab(vs) = c("V-engine" = 0, 
                    "Straight engine" = 1) 
    var_lab(am) = "Transmission"
    val_lab(am) = c(automatic = 0, 
                    manual=1)
    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
})


.apply_labels( 
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
    am = c(automatic = 0, 
           manual=1),
    gear = "Number of forward gears",
    carb = "Number of carburetors"
)

expect_identical(correct_mtcars, default_mtcars)


data(mtcars)
default_mtcars = mtcars
default_dataset(default_mtcars)

correct_mtcars = modify(mtcars,{
    var_lab(mpg) = "Miles/(US) gallon"
    var_lab(cyl) = "Number of cylinders"
    var_lab(disp) = "Displacement (cu.in.)"
    var_lab(hp) = "Gross horsepower"
    var_lab(drat) = "Rear axle ratio"
    var_lab(wt) = "Weight (lb/1000)"
    var_lab(qsec) = "1/4 mile time"
    var_lab(vs) = "Engine"
    var_lab(am) = "Transmission"
    var_lab(gear) = "Number of forward gears"
    var_lab(carb) = "Number of carburetors"
})


.apply_labels( 
    mpg = "Miles/(US) gallon",
    cyl = "Number of cylinders",
    disp = "Displacement (cu.in.)",
    hp = "Gross horsepower",
    drat = "Rear axle ratio",
    wt = "Weight (lb/1000)",
    qsec = "1/4 mile time",
    vs = "Engine",
    am = "Transmission",
    gear = "Number of forward gears",
    carb = "Number of carburetors"
)

expect_identical(correct_mtcars, default_mtcars)

context("keep default_dataset")
data(iris)
aaa = iris
default_dataset(aaa)
.keep("Species", other())
expect_identical(aaa, iris[, c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])

aaa = iris
default_dataset(aaa)
.keep(Species, other())
expect_identical(aaa, iris[, c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])


aaa = iris
.except("Species", other())
expect_identical(aaa, iris[, FALSE, drop = FALSE])

aaa = iris
.except("Species")
expect_identical(aaa, iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])


context("add_rows default dataset")
a = data.frame(x = 1:5, y = 6:10)
b = data.frame(y = 6:10, z = 11:15)
d = data.frame(y = 6:10, w = 16:20)
e = data.frame(f = 21:25, g = 26:30)

default_dataset(a)

.add_rows(b)
expect_known_value(a, "rds/add_rows1.rds",  update = FALSE)

a = data.frame(x = 1:5, y = 6:10)
.add_rows(b, d)
expect_known_value(a, "rds/add_rows2.rds",  update = FALSE)

a = data.frame(x = 1:5, y = 6:10)
.add_rows(b, d, e)
expect_known_value(a, "rds/add_rows3.rds",  update = FALSE)

a = data.frame(x = 1:5, y = 6:10)
.add_rows(b, nomatch_columns = "drop")
expect_known_value(a, "rds/add_rows4.rds",  update = FALSE)

a = data.frame(x = 1:5, y = 6:10)
.add_rows(b, d, nomatch_columns = "drop")
expect_known_value(a, "rds/add_rows5.rds",  update = FALSE)


a = data.frame(x = 1:5, y = 6:10)
.add_rows(NA)
expect_known_value(a, "rds/add_rows6a.rds",  update = FALSE)

a = data.frame(x = 1:5, y = 6:10)
.add_rows(1:2)
expect_known_value(a, "rds/add_rows6b.rds",  update = FALSE)

a = NULL
.add_rows(1)
expect_identical(a, rbind(NULL, 1))

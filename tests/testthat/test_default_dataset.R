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


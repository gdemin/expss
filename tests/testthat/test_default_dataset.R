context("default_dataset")

data(iris)
default_dataset(iris)

expect_identical(ref(default_dataset()),iris)

def = default_dataset()
ref(def)[,1] = NA

expect_identical(ref(default_dataset()),iris)
expect_identical(all(is.na(iris[,1])),TRUE)

default_dataset(NULL)
expect_error(default_dataset())

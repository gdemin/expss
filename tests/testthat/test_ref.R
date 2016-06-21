context("ref")

a = 1:3

b = ~a

expect_identical(a,ref(b))

ref(b)[2] = 4
expect_identical(a,c(1,4,3))


sample_in_place = function(dataset,N = 50){
    ds = ref(dataset)
    ref(dataset) = ds[sample(nrow(ds),N),]
    invisible(dataset)
}

data(iris)
set.seed(123)
etalon = iris[sample(150,50),]
iris_in_place = iris
ref_to_iris = ~iris_in_place
set.seed(123)
sample_in_place(ref_to_iris)
expect_identical(iris_in_place,etalon)
# without references
set.seed(123)
expect_identical(sample_in_place(iris),etalon)


# top 10 rows 
head10 = function(x){
 ds = head(ref(x),10)
 ref(x) = ds
 invisible(ds) # for usage without refences
}

data(iris)
aaa = iris
ref_to_iris = ~aaa
head10(ref_to_iris)
expect_identical(nrow(aaa),10L)

# however

data(mtcars)
mtcars10 = head10(mtcars) 

expect_identical(nrow(mtcars10),10L)
expect_identical(nrow(mtcars),32L)

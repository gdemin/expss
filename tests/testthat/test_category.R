context("category")

set.seed(123)
dichotomy_matrix = matrix(sample(0:1,40,replace = TRUE,prob=c(.6,.4)),nrow=10)
colnames(dichotomy_matrix) = c("Milk","Sugar","Tea","Coffee")
expect_equal_to_reference(category(dichotomy_matrix,compress=FALSE),"rds/category1.rds")

category_matrix=category(dichotomy_matrix)
expect_identical(val_lab(category_matrix),c(Milk = 1L,Sugar = 2L,Tea = 3L,Coffee = 4L))

class(dichotomy_matrix) = union("dichotomy",class(dichotomy_matrix))

expect_identical(dichotomy(category_matrix,use_na = FALSE),dichotomy_matrix*1.0)


dichotomy_matrix[] = 0
expect_equal_to_reference(category(dichotomy_matrix),"rds/category2.rds")
expect_equal_to_reference(category(dichotomy_matrix, prefix = "zero"),"rds/category2.rds")
expect_equal_to_reference(category_df(dichotomy_matrix, prefix = "zero"),"rds/category2df.rds")

expect_equal_to_reference(category(dichotomy_matrix, compress = FALSE),"rds/category3.rds")


expect_identical(category(dichotomy_matrix[,FALSE,drop = FALSE], compress = FALSE),structure(integer(0), .Dim = c(10L, 0L)))
expect_identical(category(dichotomy_matrix[,FALSE,drop = FALSE]),structure(integer(0), .Dim = c(10L, 0L)))
expect_identical(category(numeric(0)),structure(integer(0), .Dim = 0:1))

expect_identical(category(t(t(c(0,1,0,1,0,1)))),structure(c(NA, 1L, NA, 1L, NA, 1L), .Dim = c(6L, 1L)))
expect_identical(category(t(c(0,1,0,1,0,1))), structure(c(2L, 4L, 6L), .Dim = c(1L, 3L)))
expect_identical(category(c(0,1,0,1,0,1)),structure(c(NA, 1L, NA, 1L, NA, 1L), .Dim = c(6L, 1L)))

set.seed(123)
dichotomy_matrix = matrix(sample(0:1,40,replace = TRUE,prob=c(.6,.4)),nrow=10)
colnames(dichotomy_matrix) = c("Milk","Sugar","Tea","Coffee")

expect_equal_to_reference(category(dichotomy_matrix, prefix = "products_"), "rds/category4.rds")


# data.frame with variable labels
dichotomy_dataframe = as.data.frame(dichotomy_matrix)
colnames(dichotomy_dataframe) = paste0("product_", 1:4)
var_lab(dichotomy_dataframe[[1]]) = "Milk"
var_lab(dichotomy_dataframe[[2]]) = "Sugar"
var_lab(dichotomy_dataframe[[3]]) = "Tea"
var_lab(dichotomy_dataframe[[4]]) = "Coffee"

expect_equal_to_reference(category_df(dichotomy_dataframe, prefix = "products_"), "rds/category5.rds")




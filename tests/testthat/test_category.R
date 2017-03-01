context("category")

set.seed(123)
dichotomy_matrix = matrix(sample(0:1,40,replace = TRUE,prob=c(.6,.4)),nrow=10)
colnames(dichotomy_matrix) = c("Milk","Sugar","Tea","Coffee")
expect_warning(category(dichotomy_matrix,compress=FALSE))

expect_warning(category(dichotomy_matrix,compress=TRUE))



dichotomy_matrix[] = 0
expect_warning(category(dichotomy_matrix, prefix = "zero",compress=TRUE))
expect_equal_to_reference(as.category(dichotomy_matrix, prefix = "zero", compress=TRUE),"rds/category2df.rds")
expect_equal_to_reference(as.category(dichotomy_matrix, prefix = "zero", compress=FALSE),"rds/category3df.rds")
expect_true(is.category(as.category(dichotomy_matrix, prefix = "zero", compress=FALSE)))



expect_equal_to_reference(as.category(dichotomy_matrix[,FALSE, drop = FALSE], compress = FALSE),
                 "rds/category4df.rds")
expect_equal_to_reference(as.category(dichotomy_matrix[,FALSE], compress = FALSE),
                 "rds/category4df.rds")

expect_identical(as.category(numeric(0),compress=TRUE),
structure(list(V1 = integer(0)), .Names = "V1", row.names = integer(0), class = c("category", 
"data.frame")))

expect_equal_to_reference(as.category(t(t(c(0,1,0,1,0,1))),compress=TRUE),
                 "rds/category5df.rds")
expect_equal_to_reference(as.category(t(c(0,1,0,1,0,1)),compress=TRUE), 
                 "rds/category6df.rds")
expect_equal_to_reference(as.category(c(0,1,0,1,0,1),compress=TRUE),
                 "rds/category5df.rds")


set.seed(123)
dichotomy_matrix = matrix(sample(0:1,40,replace = TRUE,prob=c(.6,.4)),nrow=10)
colnames(dichotomy_matrix) = c("Milk","Sugar","Tea","Coffee")


# data.frame with variable labels
dichotomy_dataframe = as.data.frame(dichotomy_matrix)
colnames(dichotomy_dataframe) = paste0("product_", 1:4)
var_lab(dichotomy_dataframe[[1]]) = "Milk"
var_lab(dichotomy_dataframe[[2]]) = "Sugar"
var_lab(dichotomy_dataframe[[3]]) = "Tea"
var_lab(dichotomy_dataframe[[4]]) = "Coffee"

expect_equal_to_reference(as.category(dichotomy_dataframe, prefix = "products_",compress=TRUE), 
                          "rds/category5.rds")




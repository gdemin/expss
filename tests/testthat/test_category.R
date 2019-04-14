context("category")
suppressWarnings(RNGversion("3.5.0"))

set.seed(123)
dichotomy_matrix = matrix(sample(0:1,40,replace = TRUE,prob=c(.6,.4)),nrow=10)
colnames(dichotomy_matrix) = c("Milk","Sugar","Tea","Coffee")



dichotomy_matrix[] = 0

expect_equal_to_reference(as.category(dichotomy_matrix, prefix = "zero", compress=TRUE),
                          "rds/category2df.rds",  update = FALSE)
expect_equal_to_reference(as.category(dichotomy_matrix, prefix = "zero", compress=FALSE),
                          "rds/category3df.rds",  update = FALSE)
expect_true(is.category(as.category(dichotomy_matrix, prefix = "zero", compress=FALSE)))



expect_equal_to_reference(as.category(dichotomy_matrix[,FALSE, drop = FALSE], compress = FALSE),
                 "rds/category4df.rds",  update = FALSE)

expect_identical(as.category(numeric(0),compress=TRUE),
structure(list(V1 = integer(0)), .Names = "V1", row.names = integer(0), class = c("category", 
"data.frame")))

expect_equal_to_reference(as.category(t(t(c(0,1,0,1,0,1))),compress=TRUE),
                 "rds/category5df.rds",  update = FALSE)
expect_equal_to_reference(as.category(t(c(0,1,0,1,0,1)),compress=TRUE), 
                 "rds/category6df.rds",  update = FALSE)
expect_equal_to_reference(as.category(c(0,1,0,1,0,1),compress=TRUE),
                 "rds/category5df.rds",  update = FALSE)


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
                          "rds/category5.rds",  update = FALSE)


dichotomy_dataframe2 = dichotomy_dataframe
var_lab(dichotomy_dataframe2[[4]]) = NULL

expect_identical(as.category(dichotomy_dataframe2, prefix = "products_",compress=TRUE), 
                          add_val_lab(
                              as.category(dichotomy_dataframe, prefix = "products_",compress=TRUE),
                              c("product_4" = 4L)
                          )
)


dich = as.data.frame(matrix(NA, nrow = 3, ncol = 3))
expect_identical(as.category(dich, compress = TRUE),
structure(list(`NA` = c(NA, NA, NA)), 
           .Names = "NA", row.names = c(NA, 
-3L), 
class = c("category", "data.frame")))

expect_identical(as.category(dich, compress = TRUE),
structure(list(`NA` = c(NA, NA, NA)), .Names = "NA", row.names = c(NA, 
-3L), class = c("category", "data.frame"))
)



expect_identical(as.category(dich[FALSE, FALSE, drop = FALSE]), 
structure(list(`NA` = logical(0)), 
.Names = "NA", 
row.names = integer(0), 
class = c("category", 
"data.frame")))

set.seed(123)
dichotomy_matrix = matrix(sample(0:1,40,replace = TRUE,prob=c(.6,.4)),nrow=10)
colnames(dichotomy_matrix) = c("Used product|Milk","Used product|Sugar",
                               "Used product|Tea","Used product|Coffee")

expect_equal_to_reference(
as.category(dichotomy_matrix),
"rds/category7.rds",  update = FALSE
)


expect_equal_to_reference(
    as.category(dichotomy_matrix, compress = TRUE),
    "rds/category8.rds",  update = FALSE
)
context("dichotomy.default")

vec = 1

expect_identical(dichotomy(vec),
                 structure(1, .Dim = c(1L, 1L), .Dimnames = list(NULL, "1"), class = c("dichotomy", "matrix")))
vec = 1:3
expect_identical(dichotomy(vec),
                 structure(c(1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("1", "2", "3")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("1", "2")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec,keep_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("1", "2")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
val_lab(vec) = c(a=1,b=2,d=45)

expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec,keep_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))


expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "b","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
val_lab(vec) = c(a=1,d=45)

expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "2")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "2","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
var_lab(vec) = "Label"
val_lab(vec) = c(a=1,d=45)

expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2","Label|d")), class = c("dichotomy", "matrix")))


dep = c(1,1,1,2,2,2)
indep = c(1,1,3,2,2,3)
var_lab(indep) = "Label"
val_lab(indep) = c(a=1,b=2,c=3)
expect_equal_to_reference(lm(dep ~ dichotomy(indep)),"lm_dichotomy_default.rds")








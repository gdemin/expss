context("dichotomy.default")


expect_identical(dichotomy(numeric(0)),
    structure(numeric(0), .Dim = c(0L, 0L), class = c("dichotomy", 
                                                  "matrix")))
expect_identical(dichotomy(c(NA,NA,NA)),
                 structure(numeric(0), .Dim = c(3L, 0L), class = c("dichotomy", 
                                                                   "matrix")))

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

expect_identical(dichotomy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("1", "2")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
val_lab(vec) = c(a=1,b=2,d=45)

expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec,use_na = FALSE),
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

expect_identical(dichotomy(indep,keep=1),structure(c(1, 1, 0, 0, 0, 0), .Dim = c(6L, 1L), .Dimnames = list(
    NULL, "Label|a"), class = c("dichotomy", "matrix")))
expect_identical(dichotomy(indep,keep=2:3),structure(c(0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1), 
                                                     .Dim = c(6L,2L), .Dimnames = list(NULL, c("Label|b", "Label|c")), 
                                                     class = c("dichotomy","matrix")))
expect_identical(dichotomy(indep,keep=c("c")),structure(c(0, 0, 1, 0, 0, 1), .Dim = c(6L, 1L), .Dimnames = list(
    NULL, "Label|c"), class = c("dichotomy", "matrix")))
expect_identical(dichotomy(indep,keep=c("a","c")),structure(c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1), .Dim = c(6L, 2L), 
                                                            .Dimnames = list(NULL, c("Label|a", "Label|c")), class = c("dichotomy", 
                                                                                                                       "matrix")))

vec = letters[1:5]
expect_identical(dichotomy(vec),structure(c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 
                                            0, 1, 0, 0, 0, 0, 0, 1), .Dim = c(5L, 5L), .Dimnames = list(NULL, 
                                                                                                        c("a", "b", "c", "d", "e")), class = c("dichotomy", "matrix"
                                                                                                        )))


vec = as.factor(letters[1:5])
expect_identical(dichotomy(vec),structure(c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 
                           0, 1, 0, 0, 0, 0, 0, 1), .Dim = c(5L, 5L), .Dimnames = list(NULL, 
                                                                                       c("a", "b", "c", "d", "e")), class = c("dichotomy", "matrix"
                                                                                       )))

expect_identical(dichotomy(vec,keep=c("d","e")),structure(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1), .Dim = c(5L, 2L), .Dimnames = list(
    NULL, c("d", "e")), class = c("dichotomy", "matrix")))


context("dichotomy.data.frame")
vec = data.frame(1)

expect_identical(dichotomy(vec),
                 structure(1, .Dim = c(1L, 1L), .Dimnames = list(NULL, "1"), class = c("dichotomy", "matrix")))
vec = data.frame(1:3)
expect_identical(dichotomy(vec),
                 structure(c(1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("1", "2", "3")), class = c("dichotomy", "matrix")))


vec = data.frame(c(1:2,NA))
expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("1", "2")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("1", "2")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
val_lab(vec) = c(a=1,b=2,d=45)

vec = data.frame(vec)

expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))


expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "b","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec)

expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "2")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "2","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
var_lab(vec) = "Label"
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec)

expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2","Label|d")), class = c("dichotomy", "matrix")))

########## label on entire dataset
vec = c(1:2,NA)
vec = data.frame(vec)
val_lab(vec) = c(a=1,b=2,d=45)



expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))


expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "b","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
vec = data.frame(vec)
val_lab(vec) = c(a=1,d=45)


expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "2")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "2","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
var_lab(vec) = "Label"
vec = data.frame(vec)
val_lab(vec) = c(a=1,d=45)


expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2","Label|d")), class = c("dichotomy", "matrix")))

#############



vec = c(1:2,NA)
val_lab(vec) = c(a=1,b=2,d=45)

vec = data.frame(vec,vec)

expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))


expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "b","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec,vec,vec)

expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "2")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "2","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
var_lab(vec) = "Label"
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec,vec,vec)

expect_identical(dichotomy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2","Label|d")), class = c("dichotomy", "matrix")))

#############

vec = c(1:2,NA)
val_lab(vec) = c(a=1,b=2,d=45)

vec = data.frame(vec,3:5)

expect_identical(dichotomy(vec),
                 structure(c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 
                                                                                    5L), .Dimnames = list(NULL, c("a", "b", "3", "4", "5")), class = c("dichotomy", 
                                                                                                                                                       "matrix")))

expect_identical(dichotomy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 
    5L), .Dimnames = list(NULL, c("a", "b", "3", "4", "5")), class = c("dichotomy", "matrix"))) 

expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 
                             0), .Dim = c(3L, 6L), .Dimnames = list(NULL, c("a", "b", "3", 
                "4", "5", "d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec,c(2,1,NA))

expect_identical(dichotomy(vec),
                 structure(c(1, 1, NA, 1, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "2")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec, keep_unused = TRUE),
                 structure(c(1, 1, NA, 1, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "2","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
var_lab(vec) = "Label"
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec,vec,vec)

expect_identical(dichotomy(vec,keep="a"),
                 structure(c(1, 0, NA), .Dim = c(3L, 1L), .Dimnames = list(
                     NULL, c("Label|a")), class = c("dichotomy", "matrix")))

expect_identical(dichotomy(vec, keep_unused = TRUE,keep=2:45),
                 structure(c(0, 1, NA,0,0,NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c( "Label|2","Label|d")), class = c("dichotomy", "matrix")))

set.seed(123)
brands = t(replicate(20,sample(c(1:5,NA),4,replace = FALSE)))
expect_equal_to_reference(dichotomy(brands[,1]),"brands.rds")

###########



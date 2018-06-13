context("dichotomy.default")

expect_error(as.dichotomy(NULL))

expect_identical(dummy(numeric(0)),
                 structure(numeric(0), .Dim = c(0L, 0L), class = c("dichotomy", 
                                                                   "matrix")))

expect_identical(as.dichotomy(numeric(0)),
structure(list(`NA` = logical(0)), row.names = integer(0), .Names = "NA", class = c("dichotomy", 
                     "data.frame")))


expect_identical(dummy(c(NA,NA,NA)),
structure(c(NA, NA, NA)*1, .Dim = c(3L, 1L), .Dimnames = list(
 NULL, "NA"), class = c("dichotomy", "matrix")))

expect_identical(as.dichotomy(c(NA,NA,NA)),
structure(list(`NA` = c(NA, NA, NA)), row.names = c(NA, -3L), .Names = "NA", class = c("dichotomy", 
"data.frame")))


vec = 1

expect_identical(dummy(vec),
                 structure(1, .Dim = c(1L, 1L), .Dimnames = list(NULL, "1"), class = c("dichotomy", "matrix")))

expect_identical(as.dichotomy(vec),
structure(list(v1 = structure(1, label = "1", class = c("labelled", 
                     "numeric"))), .Names = "v1", row.names = c(NA, -1L), class = c("dichotomy", 
                                                                                    "data.frame")))


vec = 1:3
expect_identical(dummy(vec),
                 structure(c(1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("1", "2", "3")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, presence = 5, absence = -5),
                 structure(c(5, -5, -5, -5, 5, -5, -5, -5, 5), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("1", "2", "3")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, presence = "Y", absence = "N"),
                 structure(c("Y", "N", "N", "N", "Y", "N", "N", "N", "Y"), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("1", "2", "3")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, presence = FALSE, absence = TRUE),
                 structure(c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE), 
                           .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("1", "2", "3")), class = c("dichotomy", "matrix")))

expect_identical(dummy1(vec, presence = 5, absence = -5),
                 structure(c(5, -5, -5, -5, 5, -5), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("1", "2")), class = c("dichotomy", "matrix")))

expect_identical(dummy1(vec, presence = "Y", absence = "N"),
                 structure(c("Y", "N", "N", "N", "Y", "N"), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("1", "2")), class = c("dichotomy", "matrix")))

expect_identical(dummy1(vec, presence = FALSE, absence = TRUE),
                 structure(c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE), 
                           .Dim = c(3L, 2L), .Dimnames = list(
                               NULL, c("1", "2")), class = c("dichotomy", "matrix")))

expect_identical(as.dichotomy(vec),
structure(list(v1 = structure(c(1, 0, 0), label = "1", class = c("labelled", 
"numeric")), v2 = structure(c(0, 1, 0), label = "2", class = c("labelled", 
"numeric")), v3 = structure(c(0, 0, 1), label = "3", class = c("labelled", 
"numeric"))), .Names = c("v1", "v2", "v3"), row.names = c(NA, 
                          -3L), class = c("dichotomy", "data.frame")))

expect_identical(as.dichotomy(vec, presence = 5, absence = -5),
structure(list(v1 = structure(c(5, -5, -5), label = "1", class = c("labelled", 
"numeric")), v2 = structure(c(-5, 5, -5), label = "2", class = c("labelled", 
"numeric")), v3 = structure(c(-5, -5, 5), label = "3", class = c("labelled", 
"numeric"))), .Names = c("v1", "v2", "v3"), row.names = c(NA, 
  -3L), class = c("dichotomy", "data.frame")))

expect_identical(as.dichotomy(vec, presence = "Y", absence = "N"),
structure(list(v1 = structure(c("Y", "N", "N"), label = "1", class = c("labelled", 
"character")), v2 = structure(c("N", "Y", "N"), label = "2", class = c("labelled", 
"character")), v3 = structure(c("N", "N", "Y"), label = "3", class = c("labelled", 
"character"))), .Names = c("v1", "v2", "v3"), row.names = c(NA, 
                            -3L), class = c("dichotomy", "data.frame")))


vec = c(1:2,NA)
expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("1", "2")), class = c("dichotomy", "matrix")))

expect_identical(as.dichotomy(vec),
structure(list(v1 = structure(c(1, 0, NA), label = "1", class = c("labelled", 
"numeric")), v2 = structure(c(0, 1, NA), label = "2", class = c("labelled", 
"numeric"))), .Names = c("v1", "v2"), row.names = c(NA, -3L), class = c("dichotomy", 
                                                               "data.frame"))
)

expect_identical(dummy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("1", "2")), class = c("dichotomy", "matrix")))

expect_identical(as.dichotomy(vec, use_na = FALSE),
structure(list(v1 = structure(c(1, 0, 0), label = "1", class = c("labelled", 
"numeric")), v2 = structure(c(0, 1, 0), label = "2", class = c("labelled", 
"numeric"))), .Names = c("v1", "v2"), row.names = c(NA, -3L), class = c("dichotomy", 
                                                           "data.frame"))
)

vec = c(1:2,NA)
val_lab(vec) = c(a=1,b=2,d=45)

vec_mat = as.dtfrm(c(1:2,NA))
val_lab(vec_mat) = c(a=1,b=2,d=45)

expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec_mat),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))

expect_identical(as.dichotomy(vec), as.dichotomy(vec_mat))

expect_identical(as.dichotomy(vec, use_na = FALSE),
structure(list(v1 = structure(c(1, 0, 0), label = "a", class = c("labelled", 
"numeric")), v2 = structure(c(0, 1, 0), label = "b", class = c("labelled", 
                 "numeric"))), .Names = c("v1", "v2"), row.names = c(NA, -3L), class = c("dichotomy", 
                                                                                         "data.frame"))
)

expect_identical(dummy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))


expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "b","d")), class = c("dichotomy", "matrix")))

expect_identical(as.dichotomy(vec, use_na = FALSE, keep_unused = TRUE),
structure(list(v1 = structure(c(1, 0, 0), label = "a", class = c("labelled", 
"numeric")), v2 = structure(c(0, 1, 0), label = "b", class = c("labelled", 
                                 "numeric")),
v45 = structure(c(0, 0, 0), label = "d", class = c("labelled", 
                                                  "numeric"))), 
.Names = c("v1", "v2", "v45"), row.names = c(NA, -3L), class = c("dichotomy", 
 "data.frame"))
)

expect_identical(as.dichotomy(vec, use_na = TRUE, keep_unused = TRUE),
    structure(list(v1 = structure(c(1, 0, NA), label = "a", class = c("labelled", 
      "numeric")), v2 = structure(c(0, 1, NA), label = "b", class = c("labelled", 
                                                                     "numeric")),
    v45 = structure(c(0, 0, NA), label = "d", class = c("labelled", 
       "numeric"))), 
    .Names = c("v1", "v2", "v45"), row.names = c(NA, -3L), class = c("dichotomy", 
                "data.frame"))
    )

vec = c(1:2,NA)
val_lab(vec) = c(a=1,d=45)

expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "2")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "2","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
var_lab(vec) = "Label"
val_lab(vec) = c(a=1,d=45)

expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2","Label|d")), class = c("dichotomy", "matrix")))


dep = c(1,1,1,2,2,2)
indep = c(1,1,3,2,2,3)
var_lab(indep) = "Label"
val_lab(indep) = c(a=1,b=2,c=3)
if(as.numeric(version$major) ==3 && as.numeric(version$minor)<5){
    expect_equal_to_reference(lm(dep ~ dummy(indep)),"rds/lm_dichotomy_default.rds",  update = FALSE)
    expect_equal_to_reference(lm(dep ~ dummy1(indep)),"rds/lm_dichotomy_default2.rds",  update = FALSE)
}

expect_identical(dummy(indep,keep_values=1),structure(c(1, 1, 0, 0, 0, 0), .Dim = c(6L, 1L), .Dimnames = list(
    NULL, "Label|a"), class = c("dichotomy", "matrix")))

expect_identical(dummy(indep,drop_values=2:3),structure(c(1, 1, 0, 0, 0, 0), .Dim = c(6L, 1L), .Dimnames = list(
    NULL, "Label|a"), class = c("dichotomy", "matrix")))

expect_identical(dummy(indep,keep_values=2:3),structure(c(0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1), 
                                                     .Dim = c(6L,2L), .Dimnames = list(NULL, c("Label|b", "Label|c")), 
                                                     class = c("dichotomy","matrix")))

expect_identical(dummy(indep,drop_values=1),structure(c(0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1), 
                                                            .Dim = c(6L,2L), .Dimnames = list(NULL, c("Label|b", "Label|c")), 
                                                            class = c("dichotomy","matrix")))


expect_identical(dummy(indep,keep_labels=c("c")),structure(c(0, 0, 1, 0, 0, 1), .Dim = c(6L, 1L), .Dimnames = list(
NULL, "Label|c"), class = c("dichotomy", "matrix")))

expect_identical(dummy(indep,drop_labels=c("a","b")),structure(c(0, 0, 1, 0, 0, 1), .Dim = c(6L, 1L), .Dimnames = list(
NULL, "Label|c"), class = c("dichotomy", "matrix")))

expect_identical(dummy(indep,keep_labels=c("a","c")),structure(c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1), .Dim = c(6L, 2L), 
.Dimnames = list(NULL, c("Label|a", "Label|c")), class = c("dichotomy", 
                                                           "matrix")))
expect_identical(dummy(indep,drop_labels=c("b")),structure(c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1), .Dim = c(6L, 2L), 
       .Dimnames = list(NULL, c("Label|a", "Label|c")), class = c("dichotomy", 
                                                                  "matrix")))


vec = letters[1:5]
expect_identical(dummy(vec),structure(c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 
                                            0, 1, 0, 0, 0, 0, 0, 1), .Dim = c(5L, 5L), .Dimnames = list(NULL, 
                                                                                                        c("a", "b", "c", "d", "e")), class = c("dichotomy", "matrix"
                                                                                                        )))


vec = as.factor(letters[1:5])
expect_identical(dummy(vec),structure(c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 
                           0, 1, 0, 0, 0, 0, 0, 1), .Dim = c(5L, 5L), .Dimnames = list(NULL, 
                                                                                       c("a", "b", "c", "d", "e")), class = c("dichotomy", "matrix"
                                                                                       )))

expect_identical(dummy(vec,keep_labels=c("d","e")),structure(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1), .Dim = c(5L, 2L), .Dimnames = list(
    NULL, c("d", "e")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec,drop_labels=letters[1:3]),structure(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1), .Dim = c(5L, 2L), .Dimnames = list(
    NULL, c("d", "e")), class = c("dichotomy", "matrix")))


context("dichotomy.data.frame")
vec = data.frame(1)

expect_identical(dummy(vec),
                 structure(1, .Dim = c(1L, 1L), .Dimnames = list(NULL, "1"), class = c("dichotomy", "matrix")))
vec = data.frame(1:3)
expect_identical(dummy(vec),
                 structure(c(1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("1", "2", "3")), class = c("dichotomy", "matrix")))




vec = data.frame(c(1:2,NA))
expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("1", "2")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("1", "2")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
val_lab(vec) = c(a=1,b=2,d=45)

vec = data.frame(vec)

expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))


expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "b","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec)

expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "2")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "2","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
var_lab(vec) = "Label"
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec)

expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2","Label|d")), class = c("dichotomy", "matrix")))

########## label on entire dataset
vec = c(1:2,NA)
vec = data.frame(vec)
val_lab(vec) = c(a=1,b=2,d=45)



expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))


expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "b","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
vec = data.frame(vec)
val_lab(vec) = c(a=1,d=45)


expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "2")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "2","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
var_lab(vec) = "Label"
vec = data.frame(vec)
val_lab(vec) = c(a=1,d=45)


expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2","Label|d")), class = c("dichotomy", "matrix")))

#############



vec = c(1:2,NA)
val_lab(vec) = c(a=1,b=2,d=45)

vec = data.frame(vec,vec)

expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "b")), class = c("dichotomy", "matrix")))


expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "b","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec,vec,vec)

expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "2")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "2","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
var_lab(vec) = "Label"
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec,vec,vec)

expect_identical(dummy(vec),
                 structure(c(1, 0, NA, 0, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, NA, 0, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("Label|a", "Label|2","Label|d")), class = c("dichotomy", "matrix")))

#############

vec = c(1:2,NA)
val_lab(vec) = c(a=1,b=2,d=45)

vec = data.frame(vec,3:5)

expect_identical(dummy(vec),
structure(c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 
5L), .Dimnames = list(NULL, c("a", "b", "3", "4", "5")), class = c("dichotomy",  "matrix")))

expect_identical(dummy(vec,use_na = FALSE),
                 structure(c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 
    5L), .Dimnames = list(NULL, c("a", "b", "3", "4", "5")), class = c("dichotomy", "matrix"))) 

expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 
                             0), .Dim = c(3L, 6L), .Dimnames = list(NULL, c("a", "b", "3", 
                "4", "5", "d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec,c(2,1,NA))

expect_identical(dummy(vec),
                 structure(c(1, 1, NA, 1, 1, NA), .Dim = c(3L, 2L), .Dimnames = list(
                     NULL, c("a", "2")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, keep_unused = TRUE),
                 structure(c(1, 1, NA, 1, 1, NA,0,0,NA), .Dim = c(3L, 3L), .Dimnames = list(
                     NULL, c("a", "2","d")), class = c("dichotomy", "matrix")))


vec = c(1:2,NA)
var_lab(vec) = "Label"
val_lab(vec) = c(a=1,d=45)
vec = data.frame(vec,vec,vec)

expect_identical(dummy(vec,keep_labels="a"),
                 structure(c(1, 0, NA), .Dim = c(3L, 1L), .Dimnames = list(
                     NULL, c("Label|a")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec,drop_values =2),
                 structure(c(1, 0, NA), .Dim = c(3L, 1L), .Dimnames = list(
                     NULL, c("Label|a")), class = c("dichotomy", "matrix")))

expect_identical(dummy(vec, keep_unused = TRUE,keep_values=c(2:7,45)),
                 structure(c(0, 1, NA, 0, 0, NA, 0, 0, NA, 0, 0, NA, 0, 0, NA, 
                             0, 0, NA, 0, 0, NA), .Dim = c(3L, 7L), .Dimnames = list(NULL, 
                             c("Label|2", "Label|3", "Label|4", "Label|5", "Label|6", 
                            "Label|7", "Label|d")), class = c("dichotomy", "matrix")))



expect_error(dummy(vec, keep_unused = TRUE,keep_labels="unknown label"))

set.seed(123)
brands = as.data.frame(t(replicate(20,sample(c(1:5,NA),4,replace = FALSE))))
expect_equal_to_reference(dummy(brands[,1]),"rds/brands.rds",  update = FALSE)
var_lab(brands) = "Used brands"
val_lab(brands) = make_labels("
                              1 Brand A
                              2 Brand B
                              3 Brand C
                              4 Brand D
                              5 Brand E
                              ")
expect_equal_to_reference(as.dichotomy(brands, prefix = "brand_"),"rds/brands_df.rds",  update = FALSE)

brands2 = as.dichotomy(brands)
brands2[5, ] = NA 
brands2[15, ] = 0 

expect_identical(total(brands2), structure(c(1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, 
 1, 1, 1, 1), 
 label = "", 
 class = c("labelled", "numeric"), 
 labels = structure(1, .Names = "Used brands|#Total")))

expect_identical(total(set_var_lab(names2labels(brands2), "")),
                 structure(c(1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, 
                                             1, 1, 1, 1), 
                                           label = "", 
                                           class = c("labelled", "numeric"), 
                                           labels = structure(1, .Names = "Used brands|#Total")))

expect_identical(total(set_var_lab(brands2, "")),
                 structure(c(1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, 
                             1, 1, 1, 1), 
                           label = "", 
                           class = c("labelled", "numeric"), 
                           labels = structure(1, .Names = "#Total")))

expect_equal_to_reference(as.category(as.dichotomy(brands, prefix = "brand_"),compress=TRUE),
                          "rds/brands_df_cat.rds",  update = FALSE)


###########

dich = as.data.frame(matrix(NA, nrow = 3, ncol = 3))

expect_identical(as.dichotomy(dich), 
                 structure(list(`NA` = c(NA, NA, NA)), 
                           row.names = c(NA, -3L), .Names = "NA",
                           class = c("dichotomy", 
"data.frame")))

expect_identical(dummy(dich), 
structure(c(NA, NA, NA)*1, 
.Dim = c(3L, 1L), 
.Dimnames = list(NULL, 
"NA"), class = c("dichotomy", "matrix")))


expect_identical(as.dichotomy(NA), 
structure(list(`NA` = c(NA)), 
   row.names = c(NA, -1L), .Names = "NA",
   class = c("dichotomy", 
             "data.frame")))

expect_identical(dummy(NA), 
                 structure(NA*1, 
                           .Dim = c(1L, 1L), 
                           .Dimnames = list(NULL, 
"NA"), class = c("dichotomy", "matrix")))

expect_identical(dummy1(NA), 
structure(NA*1, 
.Dim = c(1L, 1L), 
.Dimnames = list(NULL, 
"NA"), class = c("dichotomy", "matrix")))


expect_identical(as.dichotomy(dich[FALSE, FALSE, drop = FALSE]),
structure(list(`NA` = logical(0)), row.names = integer(0), .Names = "NA", class = c("dichotomy", 
 "data.frame")))

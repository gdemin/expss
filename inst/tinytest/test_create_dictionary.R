context("create dictionary/apply_dictionary")


df = sheet(
    id = 1,
    with_labels = 1,
    same_labels1 = 2,
    same_labels2 = 3,
    other_labels = 4,
    no_labels = 5,
    no_var_lab = 6
) %>% 
    apply_labels(
        id = "Repsondent ID",
        with_labels = "Brand awarness",
        with_labels = num_lab("
                              1 One
                              2 Two
                              3 Three
                              "),
        same_labels1 = num_lab("
                              1 One
                              2 Two
                              3 Three
                              "),
        same_labels2 = num_lab("
                              1 One
                              2 Two
                              3 Three
                              "),
        other_labels = "Overall liking",
        other_labels = num_lab("
                              1 Like
                              2 Dislike
                              3 Hard to say
                              "),
        no_var_lab = num_lab("
                              1 Yes
                              2 No
                              ")
    )


dict = create_dictionary(df)
res = structure(list(variable = c("id", "with_labels", "with_labels", 
"with_labels", "with_labels", "same_labels1", "same_labels2", 
"other_labels", "other_labels", "other_labels", "other_labels", 
"no_var_lab", "no_var_lab"), value = c(NA, NA, 1, 2, 3, NA, NA, 
NA, 1, 2, 3, 1, 2), label = c("Repsondent ID", "Brand awarness", 
"One", "Two", "Three", "with_labels", "with_labels", "Overall liking", 
"Like", "Dislike", "Hard to say", "Yes", "No"), meta = c("varlab", 
"varlab", NA, NA, NA, "reference", "reference", "varlab", NA, 
NA, NA, NA, NA)), row.names = c(NA, -13L), class = "data.frame")


expect_identical(dict, res)
expect_equal(apply_dictionary(unlab(df), dict), df)

dict = create_dictionary(df, remove_repeated = TRUE)

res = structure(list(variable = c("id", "with_labels", NA, NA, NA, "same_labels1", 
"same_labels2", "other_labels", NA, NA, NA, "no_var_lab", NA), 
value = c(NA, NA, 1, 2, 3, NA, NA, NA, 1, 2, 3, 1, 2), label = c("Repsondent ID", 
"Brand awarness", "One", "Two", "Three", "with_labels", "with_labels", 
"Overall liking", "Like", "Dislike", "Hard to say", "Yes", 
"No"), meta = c("varlab", "varlab", NA, NA, NA, "reference", 
"reference", "varlab", NA, NA, NA, NA, NA)), row.names = c(NA, 
-13L), class = "data.frame")

expect_identical(dict, res)
expect_equal(apply_dictionary(unlab(df), dict), df)

dict = create_dictionary(df, remove_repeated = TRUE, use_references = FALSE)

res = structure(list(variable = c("id", "with_labels", NA, NA, NA, "same_labels1", 
NA, NA, "same_labels2", NA, NA, "other_labels", NA, NA, NA, "no_var_lab", 
NA), value = c(NA, NA, 1, 2, 3, 1, 2, 3, 1, 2, 3, NA, 1, 2, 3, 
1, 2), label = c("Repsondent ID", "Brand awarness", "One", "Two", 
"Three", "One", "Two", "Three", "One", "Two", "Three", "Overall liking", 
"Like", "Dislike", "Hard to say", "Yes", "No"), meta = c("varlab", 
"varlab", NA, NA, NA, NA, NA, NA, NA, NA, NA, "varlab", NA, NA, 
NA, NA, NA)), row.names = c(NA, -17L), class = "data.frame")

expect_identical(dict, res)
expect_equal(apply_dictionary(unlab(df), dict), df)

dict = create_dictionary(df, use_references = FALSE)

res = structure(list(variable = c("id", "with_labels", "with_labels", 
"with_labels", "with_labels", "same_labels1", "same_labels1", 
"same_labels1", "same_labels2", "same_labels2", "same_labels2", 
"other_labels", "other_labels", "other_labels", "other_labels", 
"no_var_lab", "no_var_lab"), value = c(NA, NA, 1, 2, 3, 1, 2, 
3, 1, 2, 3, NA, 1, 2, 3, 1, 2), label = c("Repsondent ID", "Brand awarness", 
"One", "Two", "Three", "One", "Two", "Three", "One", "Two", "Three", 
"Overall liking", "Like", "Dislike", "Hard to say", "Yes", "No"
), meta = c("varlab", "varlab", NA, NA, NA, NA, NA, NA, NA, NA, 
NA, "varlab", NA, NA, NA, NA, NA)), row.names = c(NA, -17L), class = "data.frame")


expect_identical(dict, res)
expect_equal(apply_dictionary(unlab(df), dict), df)


dict = create_dictionary(unlab(df))
res = structure(list(variable = logical(0), value = logical(0), label = logical(0), 
meta = logical(0)), row.names = integer(0), class = "data.frame")
expect_identical(dict, res)
expect_equal(apply_dictionary(unlab(df), dict), unlab(df))

my_vec = set_val_lab(1:3, c(one = 1, two = 2))
res = structure(list(variable = logical(0), value = logical(0), label = logical(0), 
meta = logical(0)), row.names = integer(0), class = "data.frame")
dict = create_dictionary(my_vec)
res = structure(list(variable = c("x", "x"), value = c(1, 2), label = c("one", 
"two"), meta = c(NA, NA)), row.names = c(NA, -2L), class = "data.frame")
expect_identical(dict, res)

###########

dict = create_dictionary(df)
dict = dict[dict$meta %in% "reference", ]

expect_warning(apply_dictionary(unlab(df), dict))
expect_equal(suppressWarnings(apply_dictionary(unlab(df), dict)), unlab(df))

dict = create_dictionary(df)
dict = dict[dict$meta %in% "varlab", ]
expect_equal(apply_dictionary(unlab(df), dict), unvl(df))

dict = create_dictionary(df)
dict = dict[dict$meta %in% c(NA, "reference"), ]
expect_equal(apply_dictionary(unlab(df), dict), unvr(df))
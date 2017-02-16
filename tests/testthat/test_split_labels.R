context("split_labels")

data(mtcars)

# apply labels
mtcars = apply_labels(mtcars,
    cyl = "Number of cylinders",
    vs = "Engine",
    vs = c("V-engine" = 0,
                    "Straight engine" = 1),
    am = "Transmission",
    am = c(automatic = 0,
                    manual=1),
    gear = "Number of forward gears",
    carb = "Number of carburetors"
)

# without sorting
tabl = mtcars %>% calculate(table_cpct(list(cyl, gear, carb), list("#total", vs, am)))
expect_equal_to_reference(split_labels(tabl[[1]]), "rds/split_labels1.RDS")
expect_equal_to_reference(split_labels(colnames(tabl)), "rds/split_labels2.RDS")

expect_equal_to_reference(split_labels(tabl[[1]], remove_repeated = FALSE), "rds/split_labels3.RDS")
expect_equal_to_reference(split_labels(colnames(tabl), remove_repeated = FALSE), "rds/split_labels4.RDS")


expect_identical(split_labels(letters, remove_repeated = TRUE), as.matrix(as.character(letters)))
expect_identical(split_labels(letters, remove_repeated = FALSE), as.matrix(as.character(letters)))

context("split_columns")
# replace first column with new columns 
expect_equal_to_reference(split_columns(tabl), "rds/split_columns1.rds")
expect_equal_to_reference(split_columns(tabl, "row_labels"), "rds/split_columns1.rds")
expect_equal_to_reference(split_columns(tabl, c(TRUE, rep(FALSE, ncol(tabl)-1))), "rds/split_columns1.rds")

expect_equal_to_reference(split_columns(tabl, remove_repeated = FALSE), "rds/split_columns2.rds")
expect_equal_to_reference(split_columns(tabl, "row_labels", remove_repeated = FALSE), "rds/split_columns2.rds")
expect_equal_to_reference(split_columns(tabl, c(TRUE, rep(FALSE, ncol(tabl)-1)), remove_repeated = FALSE), 
                          "rds/split_columns2.rds")

expect_error(split_columns(tabl, c(1, 42, NA)))
expect_error(split_columns(tabl, "row_labs"))
expect_error(split_columns(tabl, c(TRUE, rep(FALSE, ncol(tabl)))))

add_1  = paste0(tabl[[1]], "|2")
add_2  = paste0(tabl[[1]], "|3")

tabl2 = dtfrm(tabl, add_1, tabl[,-1], add_2)

expect_equal_to_reference(
    split_columns(tabl2, c(1, 7, 13)),
    "rds/split_columns3.rds"
)

expect_equal_to_reference(
    split_columns(tabl2, c("row_labels", "add_1", "add_2")),
    "rds/split_columns3.rds"
)

expect_equal_to_reference(
    split_columns(tabl2, colnames(tabl2) %in% c("row_labels", "add_1", "add_2")),
    "rds/split_columns3.rds"
)


expect_equal_to_reference(split_columns(as.matrix(tabl[[1]])), "rds/split_columns4.RDS")
expect_equal_to_reference(split_columns(as.matrix(tabl[[1]]), remove_repeated = FALSE), "rds/split_columns5.RDS")

tabl[,1] = as.factor(tabl[[1]])
expect_equal_to_reference(split_columns(tabl), "rds/split_columns1.rds")

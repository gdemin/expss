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
tabl = mtcars %>% calculate(cro_cpct(list(cyl, gear, carb), list("#total", vs, am), total_label = "#Total"))
expect_equal_to_reference(split_labels(tabl[[1]]), "rds/split_labels1.RDS")

expect_equal_to_reference(split_labels(tabl[[1]], split = "\\|", perl = TRUE), "rds/split_labels1.RDS")
expect_equal_to_reference(split_labels(tabl[[1]], split = "|", fixed = TRUE), "rds/split_labels1.RDS")
expect_equal_to_reference(split_labels(tabl[[1]], split = "\\|", fixed = FALSE), "rds/split_labels1.RDS")
expect_equal_to_reference(split_labels(tabl[[1]], split = "\\|", fixed = FALSE, perl = TRUE), "rds/split_labels1.RDS")
expect_equal_to_reference(split_labels(tabl[[1]], split = "\\|", fixed = FALSE, perl = FALSE), "rds/split_labels1.RDS")
expect_equal_to_reference(split_labels(tabl[[1]], split = "|", fixed = TRUE, perl = FALSE), "rds/split_labels1.RDS")
expect_warning(split_labels(tabl[[1]], split = "\\|", fixed = TRUE, perl = TRUE))
expect_equal_to_reference(split_labels(colnames(tabl)), "rds/split_labels2.RDS")

expect_equal_to_reference(split_labels(tabl[[1]], remove_repeated = FALSE), "rds/split_labels3.RDS")
expect_equal_to_reference(split_labels(colnames(tabl), remove_repeated = FALSE), "rds/split_labels4.RDS")


expect_identical(split_labels(letters, remove_repeated = TRUE), as.matrix(as.character(letters)))
expect_identical(split_labels(letters, remove_repeated = FALSE), as.matrix(as.character(letters)))
expect_identical(split_labels(character(0)), matrix(NA, ncol = 0, nrow = 0))

vec = c("c1|1", "c1|1", "c1|2", "c1|2", "c1|3", "c1|3")
expect_equal_to_reference(
    split_labels(vec),
    "rds/split_labels5.RDS")


vec = c("", "")
expect_identical(split_labels(vec), matrix(vec, ncol = 1))

context("split_columns")
# replace first column with new columns 
expect_equal_to_reference(split_columns(tabl), "rds/split_columns1.rds")
expect_equal_to_reference(split_columns(tabl, split = "|", fixed = TRUE), "rds/split_columns1.rds")
expect_equal_to_reference(split_columns(tabl, split = "\\|", perl = TRUE), "rds/split_columns1.rds")
expect_equal_to_reference(make_subheadings(split_columns(tabl)), "rds/split_columns1subheadings.rds")
expect_equal_to_reference(split_columns(tabl, "row_labels"), "rds/split_columns1.rds")
expect_equal_to_reference(split_columns(tabl, c(TRUE, rep(FALSE, ncol(tabl)-1))), "rds/split_columns1.rds")


expect_equal_to_reference(split_columns(tabl, remove_repeated = FALSE), "rds/split_columns2.rds")
expect_equal_to_reference(split_columns(tabl, "row_labels", remove_repeated = FALSE), "rds/split_columns2.rds")
expect_equal_to_reference(split_columns(tabl, c(TRUE, rep(FALSE, ncol(tabl)-1)), 
                                        remove_repeated = FALSE), 
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
expect_equal_to_reference(make_subheadings(split_columns(as.matrix(tabl[[1]]))), 
                          "rds/split_columns4subheafings.RDS")
expect_equal_to_reference(split_columns(as.matrix(tabl[[1]]), remove_repeated = FALSE),
                          "rds/split_columns5.RDS")

tabl[,1] = as.factor(tabl[[1]])
expect_equal_to_reference(split_columns(tabl), "rds/split_columns1.rds")
expect_equal_to_reference(make_subheadings(split_columns(tabl)),
                          "rds/split_columns1subheadings.rds")

colnames(tabl)[1] = "My super table"
expect_equal_to_reference(split_columns(tabl), "rds/split_columns1b.rds")
expect_equal_to_reference(make_subheadings(split_columns(tabl)),
                          "rds/split_columns1b_subheadings.rds")

tabl_fre = fre(mtcars$am)
expect_equal_to_reference(
    split_columns(tabl_fre),
    "rds/split_columns_fre.rds")

context("split_table_to_df")
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
mtcars$tot = 1
var_lab(mtcars$tot ) = "#total"
val_lab(mtcars$tot ) = c("#total" = 1)
tabl = mtcars %>% 
    tab_cells(cyl, gear, carb) %>% 
    tab_cols(tot, vs, am) %>% 
    tab_stat_cpct()

expect_error(split_columns(tabl))
expect_error(split_table_to_df(tabl))

tabl = tabl %>% tab_pivot() 

expect_equal_to_reference(split_table_to_df(tabl), "rds/split_table_to_df1.rds")
expect_equal_to_reference(make_subheadings(split_table_to_df(tabl)),
                          "rds/split_table_to_df1subheadings.rds")
expect_equal_to_reference(split_table_to_df(tabl, remove_repeated = FALSE),
                          "rds/split_table_to_df2.rds")

expect_identical(split_table_to_df(tabl[, FALSE]), tabl[, FALSE])
expect_equal_to_reference(split_table_to_df(tabl[FALSE, ]),
                          "rds/split_table_to_df3.rds")
expect_equal_to_reference(split_table_to_df(tabl[FALSE, ], remove_repeated = FALSE),
                          "rds/split_table_to_df4.rds")

expect_equal_to_reference(split_table_to_df(tabl[, 1]),
                          "rds/split_table_to_df5.rds")

expect_equal_to_reference(make_subheadings(split_table_to_df(tabl[, 1])),
                          "rds/split_table_to_df5subheadings.rds")

#####
expect_equal_to_reference(split_table_to_df(tabl[, 1], remove_repeated = FALSE),
                          "rds/split_table_to_df6.rds")

colnames(tabl)[1] = "My super table"

expect_equal_to_reference(split_table_to_df(tabl), "rds/split_table_to_df1b.rds")
expect_equal_to_reference(make_subheadings(split_table_to_df(tabl)), "rds/split_table_to_df1b_subheadings.rds")
expect_equal_to_reference(split_table_to_df(tabl, remove_repeated = FALSE),
                          "rds/split_table_to_df2b.rds")

expect_identical(split_table_to_df(tabl[, FALSE]), tabl[, FALSE])


expect_equal_to_reference(split_table_to_df(tabl[, 1]),
                          "rds/split_table_to_df5b.rds")

expect_equal_to_reference(make_subheadings(split_table_to_df(tabl[, 1])),
                          "rds/split_table_to_df5b_subheadings.rds")

expect_equal_to_reference(split_table_to_df(tabl[, 1], remove_repeated = FALSE),
                          "rds/split_table_to_df6b.rds")


context("split_columns subheading")

tabl = mtcars %>% 
    tab_cells(cyl, gear, carb) %>% 
    tab_cols(tot, vs, am) %>%
    tab_stat_mean(label = "|") %>% 
    tab_stat_cpct() %>% 
    tab_pivot()

expect_equal_to_reference(make_subheadings(split_columns(tabl)), "rds/split_subheadings1.rds")
expect_equal_to_reference(make_subheadings(split_table_to_df(tabl)), "rds/split_subheadings2.rds")

tabl = mtcars %>% 
    tab_cols(tot, vs, am) %>%
    tab_cells(cyl) %>% 
    tab_stat_mean(label = "|") %>% 
    tab_stat_cpct() %>% 
    tab_cells(gear) %>% 
    tab_stat_mean(label = "|") %>% 
    tab_stat_cpct() %>% 
    tab_cells(carb) %>% 
    tab_stat_mean(label = "|") %>% 
    tab_stat_cpct() %>% 
    tab_pivot()

expect_equal_to_reference(make_subheadings(split_columns(tabl)), "rds/split_subheadings3.rds")
expect_equal_to_reference(make_subheadings(split_table_to_df(tabl)), "rds/split_subheadings4.rds")

tabl[[1]] = paste0(tabl[[1]], "|newlab")
expect_equal_to_reference(make_subheadings(split_table_to_df(tabl)),
                          "rds/split_subheadings5.rds")


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

tabl = mtcars %>% 
    tab_cells(am %nest% vs) %>% 
    tab_cols(total(), cyl) %>% 
    tab_stat_cpct() %>% 
    tab_pivot()

expect_error(make_subheadings(split_columns(tabl), 0))

expect_equal_to_reference(
    make_subheadings(split_columns(tabl), 1),
    "rds/split_subheadings6.rds")

expect_equal_to_reference(
    make_subheadings(split_columns(tabl), 2),
    "rds/split_subheadings7.rds")

expect_equal_to_reference(
    make_subheadings(split_columns(tabl), 3),
    "rds/split_subheadings8.rds")

expect_equal_to_reference(
    make_subheadings(split_columns(tabl), 4),
    "rds/split_subheadings9.rds")


tabl = mtcars %>% 
    tab_cols(total(), cyl) %>% 
    tab_cells(mpg, disp) %>% 
    tab_stat_mean() %>% 
    tab_cells(am %nest% vs) %>% 
    tab_stat_cpct() %>% 
    tab_pivot()

expect_equal_to_reference(
    make_subheadings(split_columns(tabl), 1),
    "rds/split_subheadings10.rds")

expect_equal_to_reference(
    make_subheadings(split_columns(tabl), 2),
    "rds/split_subheadings11.rds")

expect_equal_to_reference(
    make_subheadings(split_columns(tabl), 3),
    "rds/split_subheadings12.rds")

expect_equal_to_reference(
    make_subheadings(split_columns(tabl), 4),
    "rds/split_subheadings13.rds")

#############################
context("make_subheadings etable")

tabl = mtcars %>% 
    tab_cells(am %nest% vs) %>% 
    tab_cols(total(), cyl) %>% 
    tab_stat_cpct() %>% 
    tab_pivot()

expect_error(make_subheadings(tabl, 0))

expect_equal_to_reference(
    make_subheadings(tabl, 1),
    "rds/split_subheadings14.rds")

expect_equal_to_reference(
    make_subheadings(tabl, 2),
    "rds/split_subheadings15.rds")

expect_equal_to_reference(
    make_subheadings(tabl, 3),
    "rds/split_subheadings16.rds")

expect_equal_to_reference(
    make_subheadings(tabl, 4),
    "rds/split_subheadings17.rds")


tabl = mtcars %>% 
    tab_cols(total(), cyl) %>% 
    tab_cells(mpg, disp) %>% 
    tab_stat_mean() %>% 
    tab_cells(am %nest% vs) %>% 
    tab_stat_cpct() %>% 
    tab_pivot()

expect_equal_to_reference(
    make_subheadings(tabl, 1),
    "rds/split_subheadings18.rds")

expect_equal_to_reference(
    make_subheadings(tabl, 2),
    "rds/split_subheadings19.rds")

expect_equal_to_reference(
    make_subheadings(tabl, 3),
    "rds/split_subheadings20.rds")

expect_equal_to_reference(
    make_subheadings(tabl, 4),
    "rds/split_subheadings21.rds")

expect_error(
    make_subheadings(tabl, 5)
)

tabl = mtcars %>% 
    tab_cols(total(), cyl) %>% 
    tab_cells(list(unvr(vs))) %>% 
    tab_stat_cpct() %>% 
    tab_pivot()

expect_equal_to_reference(
    make_subheadings(tabl, 1),
    "rds/split_subheadings22.rds")




context("merge etable")

data("mtcars")

mtcars = modify(mtcars, {
    var_lab(vs) = "vs"
    var_lab(am) = "am"
    var_lab(gear) = "gear"
    var_lab(carb) = "carb"})

expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, am, gear, carb), list("Count"), total_row_position = "none")
     percents = cro_cpct(list(vs, am, gear, carb), list("Column N %"), total_row_position = "none")
     merge(counts, percents)
     }), "rds/merge1.rds",  update = FALSE)


expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, am, gear, carb), list("Count"), total_row_position = "none")
    percents = cro_cpct(list(vs, am, gear, carb), list("Column N %"), total_row_position = "none")
    merge(counts, percents, suffixes = NULL)
}), "rds/merge1.rds",  update = FALSE)


expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, am, gear, carb), list("Count"), total_row_position = "none")
    percents = cro_cpct(list(vs, am, gear, carb), list("Column N %"), total_row_position = "none")
    merge(counts, percents, suffixes = c("_first_table", "_second_table"))
}), "rds/merge1a.rds",  update = FALSE)

### weird
expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, am, gear, am), list("Count"), total_row_position = "none")
    percents = cro_cpct(list(vs, am, vs, carb), list("Column N %"), total_row_position = "none")
    merge(counts, percents)
}), "rds/merge2.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, am), list("Count"), total_row_position = "none")
    percents = cro_cpct(list(gear, carb), list("Column N %"), total_row_position = "none")
    merge(counts, percents)
}), "rds/merge3.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars, {
    counts = cro(list(am, gear, carb), list("Count"), total_row_position = "none")
    percents = cro_cpct(list(vs, am, gear, carb), list("Column N %"), total_row_position = "none")
    merge(counts, percents)
}), "rds/merge4.rds",  update = FALSE)

####
expect_equal_to_reference(
with(mtcars, {
    counts = cro(list(vs, am, gear, carb), list("Count"), total_row_position = "above")
    percents = cro_cpct(list(vs, gear, carb), list("Column N %"), total_row_position = "above")
    merge(counts, percents)

}), "rds/merge4a.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, gear, carb), list("Count"), total_row_position = "none")
    percents = cro_cpct(list(vs, am, gear, carb), list("Column N %"), total_row_position = "none")
    merge(counts, percents)
}), "rds/merge5.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, gear, carb), list("Count"))
    merge(counts, counts)
}), "rds/merge6.rds",  update = FALSE)

context("%merge%")

expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, am, gear, carb), list("Count"), total_row_position = "none")
    percents = cro_cpct(list(vs, am, gear, carb), list("Column N %"), total_row_position = "none")
    counts %merge% percents
}), "rds/merge1.rds",  update = FALSE)

### weird
expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, am, gear, am), list("Count"), total_row_position = "none")
    percents = cro_cpct(list(vs, am, vs, carb), list("Column N %"), total_row_position = "none")
    counts %merge% percents
}), "rds/merge2.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, am), list("Count"), total_row_position = "none")
    percents = cro_cpct(list(gear, carb), list("Column N %"), total_row_position = "none")
    counts %merge% percents
}), "rds/merge3.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars, {
    counts = cro(list(am, gear, carb), list("Count"), total_row_position = "none")
    percents = cro_cpct(list(vs, am, gear, carb), list("Column N %"), total_row_position = "none")
    counts %merge% percents
}), "rds/merge4.rds",  update = FALSE)

####
expect_equal_to_reference(
    with(mtcars, {
        counts = cro(list(vs, am, gear, carb), list("Count"), total_row_position = "above")
        percents = cro_cpct(list(vs, gear, carb), list("Column N %"), total_row_position = "above")
        counts %merge% percents

    }), "rds/merge4a.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, gear, carb), list("Count"), total_row_position = "none")
    percents = cro_cpct(list(vs, am, gear, carb), list("Column N %"), total_row_position = "none")
    counts %merge% percents
}), "rds/merge5.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars, {
    counts = cro(list(vs, gear, carb), list("Count"))
    counts %merge% counts
}), "rds/merge6.rds",  update = FALSE)


context("merge etable")

data("mtcars")

mtcars = modify(mtcars, {
    var_lab(vs) = "vs"
    var_lab(am) = "am"
    val_lab(am) = c("automatic transmission" = 1, "manual transmission" = 0)
    var_lab(gear) = "gear"
    var_lab(carb) = "carb"})


expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(list(unvr(carb)), list(unvr(am), total()),
                total_label = "#Total") 
    freq %merge% cross


}), "rds/merge7.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(list(unvr(carb)), list(unvr(am), total()), total_label = "#Total")
    cross %merge% freq


}), "rds/merge8.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars,{
    freq = fre(carb[carb!=2])
    cross = cro(list(unvr(carb[carb!=4])), list(unvr(am[carb!=4]), "#Total"), total_label = "#Total")
    freq %merge% cross


}), "rds/merge9.rds",  update = FALSE)

# weird
expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(list(unvr(vs)), list(unvr(am), total()), total_label = "#Total")
    cross %merge% freq


}), "rds/merge10.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars,{
    freq1 = fre(gear)
    freq2 = fre(carb)
    merge(freq1, freq2, suffixes = c(" gear", " carb"))
}), "rds/merge11.rds",  update = FALSE)


context("merge simple_summary")
data("mtcars")

mtcars = modify(mtcars, {
    var_lab(vs) = "vs"
    var_lab(am) = "am"
    val_lab(am) = c("automatic transmission" = 1, "manual transmission" = 0)
    var_lab(gear) = "gear"
    var_lab(carb) = "carb"})

expect_equal_to_reference(
    with(mtcars, 
         merge(cro_mean(list(V1 = mpg, gear, carb), list(unvr(am), total())), 
               cro_mean(list(gear, carb), list(unvr(am), total()))
         )
    ), 
    "rds/merge_simple_summary1.rds",  update = FALSE)


expect_equal_to_reference(
    with(mtcars, 
         merge(cro_mean(list(V1 = mpg, gear, carb), list(unvr(am), total())), 
               cro_mean(list(gear, carb), list(unvr(vs), total()))
         )
    ), 
    "rds/merge_simple_summary2.rds",  update = FALSE)


expect_equal_to_reference(
    with(mtcars, 
         cro_mean(list(V1 = mpg, gear, carb), list(unvr(am), total()))  %merge%  
             cro_mean(list(gear, carb), list(unvr(am), total()))
         
    ), 
    "rds/merge_simple_summary1.rds",  update = FALSE)


expect_equal_to_reference(
    with(mtcars, 
         cro_mean(list(V1 = mpg, gear, carb), list(unvr(am), total())) %merge%
               cro_mean(list(gear, carb), list(unvr(vs), total()))
         
    ), 
    "rds/merge_simple_summary2.rds",  update = FALSE)


context("left join")

a = sheet(x = 1:5, y = 11:15)
b = sheet(x = 3:7, z = 13:17)
d = sheet(t = 3:7, z = 13:17)
expect_identical(a %merge% b, merge(a, b, all.x = TRUE, all.y = FALSE))
expect_error(a %merge% d)


context("merge duplicated columns")

data(mtcars)
expect_equal_to_reference(fre(mtcars$cyl[mtcars$mpg<20]) %merge% fre(mtcars$cyl) ,
                           "rds/merge12.rds",  update = FALSE)

context("merge duplicated rows")

tabl1 = mtcars %>% 
    tab_cells(am) %>% 
    tab_cols(total(), vs) %>% 
    tab_stat_cpct() %>% 
    tab_stat_cpct() %>% 
    tab_pivot()

tabl2 = mtcars %>% 
    tab_cells(am) %>% 
    tab_cols(total(), cyl) %>% 
    tab_stat_cpct() %>% 
    tab_pivot()

expect_equal_to_reference(
    tabl1 %merge% tabl2,
    "rds/merge13.rds",  update = FALSE)
expect_equal_to_reference(
    tabl2 %merge% tabl1,
    "rds/merge14.rds",  update = FALSE)
expect_equal_to_reference(
    tabl1 %merge% tabl1,
    "rds/merge15.rds",  update = FALSE)


expect_equal_to_reference(
    merge(tabl1, tabl1, by = "row_labels"),
    "rds/merge15.rds",  update = FALSE)

expect_equal_to_reference(
    merge(tabl1, tabl1, by = 1),
    "rds/merge15.rds",  update = FALSE)

expect_error(merge(tabl1, tabl1, by = 1:2))
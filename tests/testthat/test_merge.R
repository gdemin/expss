context("merge etable")

data("mtcars")


var_lab(mtcars$vs) = "vs"
var_lab(mtcars$am) = "am"
var_lab(mtcars$gear) = "gear"
var_lab(mtcars$carb) = "carb"

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


context("merge etable")

data("mtcars")


var_lab(mtcars$vs) = "vs"
var_lab(mtcars$am) = "am"
val_lab(mtcars$am) = c("automatic transmission" = 1, "manual transmission" = 0)
var_lab(mtcars$gear) = "gear"
var_lab(mtcars$carb) = "carb"


expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(list(unvr(carb)), list(unvr(am), total()),
                total_label = "#Total") 
    merge(freq, cross)


}), "rds/merge7.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(list(unvr(carb)), list(unvr(am), total()), total_label = "#Total")
    cross  %>%  merge(freq)


}), "rds/merge8.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars,{
    freq = fre(carb[carb!=2])
    cross = cro(list(unvr(carb[carb!=4])), list(unvr(am[carb!=4]), "#Total"), total_label = "#Total")
    freq %>% add_columns(cross)


}), "rds/merge9.rds",  update = FALSE)

# weird
expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(list(unvr(vs)), list(unvr(am), total()), total_label = "#Total")
    add_columns(cross, freq)


}), "rds/merge10.rds",  update = FALSE)

expect_equal_to_reference(with(mtcars,{
    freq1 = fre(gear)
    freq2 = fre(carb)
    merge(freq1, freq2, suffixes = c(" gear", " carb"))
}), "rds/merge11.rds",  update = FALSE)


context("merge simple_summary")
data("mtcars")


var_lab(mtcars$vs) = "vs"
var_lab(mtcars$am) = "am"
val_lab(mtcars$am) = c("automatic transmission" = 1, "manual transmission" = 0)
var_lab(mtcars$gear) = "gear"
var_lab(mtcars$carb) = "carb"

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
         merge(cro_mean(list(V1 = mpg, gear, carb), list(unvr(am), total())),  
             cro_mean(list(gear, carb), list(unvr(am), total())))
         
    ), 
    "rds/merge_simple_summary1.rds",  update = FALSE)


expect_equal_to_reference(
    with(mtcars, 
         merge(cro_mean(list(V1 = mpg, gear, carb), list(unvr(am), total())),
               cro_mean(list(gear, carb), list(unvr(vs), total())))
         
    ), 
    "rds/merge_simple_summary2.rds",  update = FALSE)




context("merge duplicated columns")

data(mtcars)
expect_equal_to_reference(merge(fre(mtcars$cyl[mtcars$mpg<20]), fre(mtcars$cyl)) ,
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
   merge(tabl1, tabl2),
    "rds/merge13.rds",  update = FALSE)
expect_equal_to_reference(
    merge(tabl2, tabl1),
    "rds/merge14.rds",  update = FALSE)
expect_equal_to_reference(
    merge(tabl1, tabl1),
    "rds/merge15.rds",  update = FALSE)


expect_equal_to_reference(
    merge(tabl1, tabl1, by = "row_labels"),
    "rds/merge15.rds",  update = FALSE)

expect_equal_to_reference(
    merge(tabl1, tabl1, by = 1),
    "rds/merge15.rds",  update = FALSE)

expect_error(merge(tabl1, tabl1, by = 1:2))
context("merge etable")

data("mtcars")

mtcars = modify(mtcars, {
    var_lab(vs) = "vs"
    var_lab(am) = "am"
    var_lab(gear) = "gear"
    var_lab(carb) = "carb"})

expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(vs, am, gear, carb), "Count", total_row_position = "none")
     percents = table_cpct(list(vs, am, gear, carb), "Column N %", total_row_position = "none")
     merge(counts, percents)
     }), "rds/merge1.rds")

### weird
expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(vs, am, gear, am), "Count", total_row_position = "none")
    percents = table_cpct(list(vs, am, vs, carb), "Column N %", total_row_position = "none")
    merge(counts, percents)
}), "rds/merge2.rds")

expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(vs, am), "Count", total_row_position = "none")
    percents = table_cpct(list(gear, carb), "Column N %", total_row_position = "none")
    merge(counts, percents)
}), "rds/merge3.rds")

expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(am, gear, carb), "Count", total_row_position = "none")
    percents = table_cpct(list(vs, am, gear, carb), "Column N %", total_row_position = "none")
    merge(counts, percents)
}), "rds/merge4.rds")

####
expect_equal_to_reference(
with(mtcars, {
    counts = table_cases(list(vs, am, gear, carb), "Count", total_row_position = "above")
    percents = table_cpct(list(vs, gear, carb), "Column N %", total_row_position = "above")
    merge(counts, percents)

}), "rds/merge4a.rds")

expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(vs, gear, carb), "Count", total_row_position = "none")
    percents = table_cpct(list(vs, am, gear, carb), "Column N %", total_row_position = "none")
    merge(counts, percents)
}), "rds/merge5.rds")

expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(vs, gear, carb), "Count")
    merge(counts, counts)
}), "rds/merge6.rds")

context("%merge%")

expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(vs, am, gear, carb), "Count", total_row_position = "none")
    percents = table_cpct(list(vs, am, gear, carb), "Column N %", total_row_position = "none")
    counts %merge% percents
}), "rds/merge1.rds")

### weird
expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(vs, am, gear, am), "Count", total_row_position = "none")
    percents = table_cpct(list(vs, am, vs, carb), "Column N %", total_row_position = "none")
    counts %merge% percents
}), "rds/merge2.rds")

expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(vs, am), "Count", total_row_position = "none")
    percents = table_cpct(list(gear, carb), "Column N %", total_row_position = "none")
    counts %merge% percents
}), "rds/merge3.rds")

expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(am, gear, carb), "Count", total_row_position = "none")
    percents = table_cpct(list(vs, am, gear, carb), "Column N %", total_row_position = "none")
    counts %merge% percents
}), "rds/merge4.rds")

####
expect_equal_to_reference(
    with(mtcars, {
        counts = table_cases(list(vs, am, gear, carb), "Count", total_row_position = "above")
        percents = table_cpct(list(vs, gear, carb), "Column N %", total_row_position = "above")
        counts %merge% percents

    }), "rds/merge4a.rds")

expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(vs, gear, carb), "Count", total_row_position = "none")
    percents = table_cpct(list(vs, am, gear, carb), "Column N %", total_row_position = "none")
    counts %merge% percents
}), "rds/merge5.rds")

expect_equal_to_reference(with(mtcars, {
    counts = table_cases(list(vs, gear, carb), "Count")
    counts %merge% counts
}), "rds/merge6.rds")


context("merge simple_table")

data("mtcars")

mtcars = modify(mtcars, {
    var_lab(vs) = "vs"
    var_lab(am) = "am"
    val_lab(am) = c("automatic transmission" = 1, "manual transmission" = 0)
    var_lab(gear) = "gear"
    var_lab(carb) = "carb"})


expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(carb, am)
    freq %merge% cross


}), "rds/merge7.rds")

expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(carb, am)
    cross %merge% freq


}), "rds/merge8.rds")

expect_equal_to_reference(with(mtcars,{
    freq = fre(carb[carb!=2])
    cross = cro(carb[carb!=4], am[carb!=4])
    freq %merge% cross


}), "rds/merge9.rds")

# weird
expect_equal_to_reference(with(mtcars,{
    freq = fre(carb)
    cross = cro(vs, am)
    cross %merge% freq


}), "rds/merge10.rds")

expect_equal_to_reference(with(mtcars,{
    freq1 = fre(gear)
    freq2 = fre(carb)
    merge(freq1, freq2, suffixes = c(" gear", " carb"))
}), "rds/merge11.rds")


context("merge summary_table")
data("mtcars")

mtcars = modify(mtcars, {
    var_lab(vs) = "vs"
    var_lab(am) = "am"
    val_lab(am) = c("automatic transmission" = 1, "manual transmission" = 0)
    var_lab(gear) = "gear"
    var_lab(carb) = "carb"})

expect_equal_to_reference(
    with(mtcars, 
         merge(cro_mean(list(mpg, gear, carb), am), 
               cro_mean(list(gear, carb), am)
         )
    ), 
    "rds/merge_summary_table1.rds")


expect_equal_to_reference(
    with(mtcars, 
         merge(cro_mean(list(mpg, gear, carb), am), 
               cro_mean(list(gear, carb), vs)
         )
    ), 
    "rds/merge_summary_table2.rds")


expect_equal_to_reference(
    with(mtcars, 
         cro_mean(list(mpg, gear, carb), am)  %merge%  
               cro_mean(list(gear, carb), am)
         
    ), 
    "rds/merge_summary_table1.rds")


expect_equal_to_reference(
    with(mtcars, 
         cro_mean(list(mpg, gear, carb), am) %merge% 
               cro_mean(list(gear, carb), vs)
         
    ), 
    "rds/merge_summary_table2.rds")


context("left join")

a = dtfrm(x = 1:5, y = 11:15)
b = dtfrm(x = 3:7, z = 13:17)
d = dtfrm(t = 3:7, z = 13:17)
expect_identical(a %merge% b, merge(a, b, all.x = TRUE, all.y = FALSE))
expect_error(a %merge% d)


context("merge duplicated columns")

data(mtcars)
expect_equal_to_reference(fre(mtcars$cyl[mtcars$mpg<20]) %merge% fre(mtcars$cyl) ,
                           "rds/merge12.rds")


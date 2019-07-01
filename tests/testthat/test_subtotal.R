context("subtotal")

a = 1:7
expect_known_value(cro(net(a, Bottom = 1:2, Top = 6:7, position = "below")),
                   "rds/subtotal1.rds", update = FALSE)
expect_known_value(cro(net(a, Bottom = 1 %thru% 2, Top = greater(5), position = "below")),
                   "rds/subtotal1.rds", update = FALSE)
expect_known_value(cro(net(a, Bottom = 1:2, Top = 6:7, position = "above")),
                   "rds/subtotal2.rds", update = FALSE)
expect_known_value(cro(net(a, Bottom = 1:2, Top = 6:7, position = "top")),
                   "rds/subtotal3.rds", update = FALSE)
expect_known_value(cro(net(a, Bottom = 1:2, Top = 6:7, position = "bottom")),
                   "rds/subtotal4.rds", update = FALSE)

expect_known_value(cro(subtotal(a, Bottom = 1:2, Top = 6:7, position = "below")),
                   "rds/subtotal5.rds", update = FALSE)
expect_known_value(cro(subtotal(a, Bottom = 1:2 ~ 2.1, Top = 6:7 ~ 7.1, position = "below")),
                   "rds/subtotal5.rds", update = FALSE)
expect_known_value(cro(subtotal(a, Bottom = 1:2, Top = 6:7, position = "above")),
                   "rds/subtotal6.rds", update = FALSE)
expect_known_value(cro(subtotal(a, Bottom = 1:2, Top = 6:7, position = "top")),
                   "rds/subtotal7.rds", update = FALSE)
expect_known_value(cro(subtotal(a, Bottom = 1:2, Top = 6:7, position = "bottom")),
                   "rds/subtotal8.rds", update = FALSE)

val_lab(a) = c(Three = 3)
expect_known_value(cro(net(a, 2:3, position = "below")),
                   "rds/subtotal9.rds", update = FALSE)
expect_known_value(cro(subtotal(a, 2:3, position = "above", new_label = "first")),
                   "rds/subtotal10.rds", update = FALSE)
expect_known_value(cro(net(a, 2:3, position = "top", prefix = "LAST ", new_label = "last")),
                   "rds/subtotal11.rds", update = FALSE)
expect_known_value(cro(subtotal(a, 2:3, position = "bottom")),
                   "rds/subtotal12.rds", update = FALSE)


b = rev(a)
val_lab(b) = c(Seven = 7)
var_lab(a) = "My 'a'"
var_lab(b) = "My 'b'"

my_df = sheet(a, b)

expect_known_value(
    cro(net(my_df, 1:2, Top = 6:7, position = "bottom", prefix = "NET ", new_label = "range")),
    "rds/subtotal12a.rds", update = FALSE)

expect_known_value(
    cro(subtotal(as.list(my_df), 1:2, Top = 6:7, position = "above")),
    "rds/subtotal12b.rds", update = FALSE)

my_letters = c("a", "b", "c", "d", "e", "f")
var_lab(my_letters) = "MY LETTERS"
expect_known_value(cro(subtotal(my_letters, c("b", "c", "d"), c("a", "e"), position = "below")),
                   "rds/subtotal13.rds", update = FALSE)

expect_known_value(cro(subtotal(my_letters, perl("b|c|d"), contains("a") | contains("e"), position = "below")),
                   "rds/subtotal13.rds", update = FALSE)


expect_known_value(cro(subtotal(my_letters, c("b", "c", "d"), c("a", "e"), position = "above", prefix = "NET ")),
                   "rds/subtotal14.rds", update = FALSE)

expect_known_value(cro(subtotal(my_letters, c("b", "c", "d"), c("a", "e"), position = "top", new_label = "range")),
                   "rds/subtotal15.rds", update = FALSE)
expect_known_value(cro(subtotal(my_letters, c("b", "c", "d"), c("a", "e"), position = "bottom", 
                                prefix = "NET ", new_label = "first")),
                   "rds/subtotal16.rds", update = FALSE)

expect_known_value(cro(subtotal(my_letters, "My bcd" = c("b", "c", "d"), "My ae" = c("a", "e"), 
                                position = "top", new_label = "range")),
                   "rds/subtotal17.rds", update = FALSE)
expect_known_value(
    cro(subtotal(my_letters, "My bcd" = c("b", "c", "d"), "My ae" = c("a", "e"), 
                 position = "top", new_label = "range")),
                   "rds/subtotal18.rds", update = FALSE)

expect_known_value(
    cro(net(my_letters, c("b", "c", "d"), "My ae" = c("a", "e"), 
                 position = "top", new_label = "range")),
    "rds/subtotal18a.rds", update = FALSE)

data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (lb/1000)",
                      qsec = "1/4 mile time",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      gear = c(
                          One = 1,
                          Two = 2,
                          Three = 3,
                          Four = 4,
                          Five = 5
                      ),
                      carb = "Number of carburetors"
)

expect_equal_to_reference(
mtcars %>% 
    tab_cells(mpg) %>% 
    tab_net_cells("Low mpg" = less(mean(mpg)), "High mpg" = greater_or_equal(mean(mpg))) %>% 
    tab_cols(total(), carb) %>% 
    tab_stat_cases() %>% 
    tab_pivot()
, "rds/subtotal19.rds", update = FALSE)

expect_equal_to_reference(
mtcars %>% 
    tab_cells(mpg) %>% 
    tab_net_cells("Low mpg" = less(mean(mpg)), "High mpg" = greater_or_equal(mean(mpg))) %>% 
    tab_cols(gear) %>%
    tab_net_cols(1:2, 3:4, "5 and more" = greater(4)) %>% 
    tab_stat_cases() %>% 
    tab_pivot()
, "rds/subtotal20.rds", update = FALSE)

expect_equal_to_reference(
mtcars %>% 
    tab_cells(mpg) %>% 
    tab_rows(gear) %>%
    tab_subtotal_rows(1:2, 3:4, "5 and more" = greater(4), position = "above", prefix = "NET ", new_label = "range") %>% 
    tab_stat_mean() %>% 
    tab_pivot()
, "rds/subtotal21.rds", update = FALSE)

#####
expect_equal_to_reference(
    mtcars %>% 
        tab_cells(mpg) %>% 
        tab_subtotal_cells("Low mpg" = less(mean(mpg)), "High mpg" = greater_or_equal(mean(mpg)), position = "top") %>% 
        tab_cols(total(), carb) %>% 
        tab_stat_cases() %>% 
        tab_pivot()
    , "rds/subtotal22.rds", update = FALSE)

expect_equal_to_reference(
    mtcars %>% 
        tab_cells(mpg) %>% 
        tab_net_cells("Low mpg" = less(mean(mpg)), "High mpg" = greater_or_equal(mean(mpg))) %>% 
        tab_cols(gear) %>%
        tab_subtotal_cols(1:2, 3:4, "5 and more" = greater(4), position = "bottom") %>% 
        tab_stat_cases() %>% 
        tab_pivot()
    , "rds/subtotal23.rds", update = FALSE)

expect_equal_to_reference(
    mtcars %>% 
        tab_cells(mpg) %>% 
        tab_rows(gear) %>%
        tab_net_rows(1:2, 3:4, "5 and more" = greater(4), position = "above", prefix = "NET ", new_label = "first") %>% 
        tab_stat_mean() %>% 
        tab_pivot()
    , "rds/subtotal24.rds", update = FALSE)


categ = mrset(v1 = c(1, 2, 3, 4), v2 = c(NA, NA, 1, 2))

val_lab(categ) = c(
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4
)

var_lab(categ) = "My multiple"

expect_equal_to_reference(
    cro(subtotal(categ, 1:2, 3:4))
    , "rds/subtotal25.rds", update = FALSE)
expect_equal_to_reference(
    cro(subtotal(categ, 1:2, 3:4, position = "bottom", prefix = "SUBTOTAL "))
    , "rds/subtotal26.rds", update = FALSE)
expect_equal_to_reference(
    cro(net(categ, 1:2, "ThreeFour" = 3:4, new_label = "range"))
    , "rds/subtotal27.rds", update = FALSE)

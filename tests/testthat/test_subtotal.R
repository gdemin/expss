context("subtotal")


a = 1:7
expect_known_value(cro(net(a, Bottom = 1:2, Top = 6:7, position = "below")),
                   "rds/subtotal1.rds", update = FALSE)
expect_known_value(cro(net(a, Bottom = function(x) x<3, Top = 6:7, position = "below")),
                   "rds/subtotal1.rds", update = FALSE)
expect_known_value(cro(net(a, Bottom = 1 %thru% 2, Top = greater(5), position = "below")),
                   "rds/subtotal1.rds", update = FALSE)
expect_known_value(cro(net(a, Bottom = 1:2, Top = 6:7, position = "above")),
                   "rds/subtotal2.rds", update = FALSE)
expect_known_value(cro(net(a, Top = 6:7, Bottom = 1:2, position = "top")),
                   "rds/subtotal3.rds", update = FALSE)
expect_known_value(cro(net(a, Bottom = 1:2, Top = 6:7, position = "bottom")),
                   "rds/subtotal4.rds", update = FALSE)

expect_known_value(cro(subtotal(a, Bottom = 1:2, Top = 6:7, position = "below")),
                   "rds/subtotal5.rds", update = FALSE)
expect_error(cro(subtotal(a, Bottom = 1:2 ~ 2.1, Top = 6:7 ~ 7.1, position = "below")))
expect_known_value(cro(subtotal(a, Bottom = 1:2, Top = 6:7, position = "above")),
                   "rds/subtotal6.rds", update = FALSE)
expect_known_value(cro(subtotal(a, Top = 6:7, Bottom = 1:2, position = "top")),
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


var_lab(a) = "My 'a'"
expect_known_value(cro(subtotal(a, Bottom = 1:2, Top = 6:7, 'My new cat' = 98:99, position = "above")),
                   "rds/subtotal8a.rds", update = FALSE)

expect_known_value(cro(subtotal(a, Bottom = 1:2, Top = 6:7, 'My new cat' = gt(90), position = "above")),
                   "rds/subtotal8b.rds", update = FALSE)

expect_known_value(cro(subtotal(a, Bottom = 1:2, Top = 6:7, 'My new cat' = gt(90), position = "top")),
                   "rds/subtotal8c.rds", update = FALSE)

expect_known_value(cro(subtotal(a, Bottom = 1:2, Top2 = 6:7, Top3 = 5:7, 'My new cat' = gt(90), position = "below")),
                   "rds/subtotal8d.rds", update = FALSE)

expect_known_value(cro(net(a, Bottom = 1:2, Top2 = 6:7, Top3 = 5:7, 'My new cat' = gt(90), position = "below")),
                   "rds/subtotal8e.rds", update = FALSE)

b = rev(a)
val_lab(b) = c(Seven = 7)
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

expect_known_value(cro(subtotal(my_letters, function(x) x %in% c("b", "c", "d"), c("a", "e"), position = "below")),
                   "rds/subtotal13.rds", update = FALSE)

expect_known_value(cro(subtotal(my_letters, perl("b|c|d"), contains("a") | contains("e"), position = "below")),
                   "rds/subtotal13.rds", update = FALSE)


expect_known_value(cro(subtotal(my_letters,  c("a", "e"),  c("b", "c", "d"),position = "above", prefix = "NET ")),
                   "rds/subtotal14.rds", update = FALSE)

expect_known_value(cro(subtotal(my_letters, c("a", "e"), c("b", "c", "d"), position = "top", new_label = "range")),
                   "rds/subtotal15.rds", update = FALSE)
expect_known_value(cro(subtotal(my_letters, c("a", "e"),  c("b", "c", "d"), position = "bottom", 
                                prefix = "NET ", new_label = "first")),
                   "rds/subtotal16.rds", update = FALSE)

expect_known_value(cro(subtotal(my_letters, "My bcd" = c("b", "c", "d"), "My ae" = c("a", "e"), 
                                position = "top", new_label = "range")),
                   "rds/subtotal17.rds", update = FALSE)


expect_known_value(cro(subtotal(my_letters, "My bcd" = c("b", "c", "d"), "My ae" = c("a", "e"), c("x", "y", "z"),
                                position = "above", new_label = "range")),
                   "rds/subtotal18.rds", update = FALSE)

expect_known_value(cro(subtotal(
    set_var_lab(factor(my_letters), var_lab(my_letters)),
    "My bcd" = c("b", "c", "d"), "My ae" = c("a", "e"), c("x", "y", "z"),
                                position = "above", new_label = "range")),
                   "rds/subtotal18.rds", update = FALSE)


expect_known_value(
    cro(net(my_letters, "My ae" = c("a", "e"), c("b", "c", "d"),  
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

expect_known_value(
mtcars %>% 
    tab_cells(mpg) %>% 
    tab_net_cells("Low mpg" = less(mean(mpg)), "High mpg" = greater_or_equal(mean(mpg))) %>% 
    tab_cols(total(), carb) %>% 
    tab_stat_cases() %>% 
    tab_pivot()
, "rds/subtotal19.rds", update = FALSE)

expect_known_value(
mtcars %>% 
    tab_cells(mpg) %>% 
    tab_net_cells("Low mpg" = less(mean(mpg)), "High mpg" = greater_or_equal(mean(mpg))) %>% 
    tab_cols(gear) %>%
    tab_net_cols(1:2, 3:4, "5 and more" = greater(4)) %>% 
    tab_stat_cases() %>% 
    tab_pivot()
, "rds/subtotal20.rds", update = FALSE)

expect_known_value(
mtcars %>% 
    tab_cells(mpg) %>% 
    tab_rows(gear) %>%
    tab_subtotal_rows(1:2, 3:4, "5 and more" = greater(4), position = "above", prefix = "NET ", new_label = "range") %>% 
    tab_stat_mean() %>% 
    tab_pivot()
, "rds/subtotal21.rds", update = FALSE)

#####
expect_known_value(
    mtcars %>% 
        tab_cells(mpg) %>% 
        tab_subtotal_cells("Low mpg" = less(mean(mpg)), "High mpg" = greater_or_equal(mean(mpg)), position = "top") %>% 
        tab_cols(total(), carb) %>% 
        tab_stat_cases() %>% 
        tab_pivot()
    , "rds/subtotal22.rds", update = FALSE)

expect_known_value(
    mtcars %>% 
        tab_cells(mpg) %>% 
        tab_net_cells("Low mpg" = less(mean(mpg)), "High mpg" = greater_or_equal(mean(mpg))) %>% 
        tab_cols(gear) %>%
        tab_subtotal_cols(1:2, 3:4, "5 and more" = greater(4), position = "bottom") %>% 
        tab_stat_cases() %>% 
        tab_pivot()
    , "rds/subtotal23.rds", update = FALSE)

expect_known_value(
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

expect_known_value(
    cro(subtotal(categ, 1:2, 3:4))
    , "rds/subtotal25.rds", update = FALSE)

expect_known_value(
    cro(subtotal(categ, 1:2, function(x) x>2))
    , "rds/subtotal25.rds", update = FALSE)

expect_known_value(
    cro(subtotal(categ, 1:2, 3:4, position = "bottom", prefix = "SUBTOTAL "))
    , "rds/subtotal26.rds", update = FALSE)

expect_known_value(
    cro(subtotal(categ, lte(2), gte(3), position = "bottom", prefix = "SUBTOTAL "))
    , "rds/subtotal26.rds", update = FALSE)

expect_known_value(
    cro(net(categ, 1:2, "ThreeFour" = 3:4, new_label = "range"))
    , "rds/subtotal27.rds", update = FALSE)

expect_known_value(
    cro(subtotal(categ, c(1, 3),  c(2, 4), position = "above"))
    , "rds/subtotal27a.rds", update = FALSE)

expect_known_value(
    cro(subtotal(categ, c(1, 3),  c(2, 4), position = "above"))
    , "rds/subtotal27a.rds", update = FALSE)

# expect_known_value(
#     categ %>% 
#         tab_cells(mrset(v1, v2)) %>% 
#         tab_subtotal_cells("Net One" = c(1, 3),  "Net Two" = c(2, 4), 
#                            position = "above", 
#                            prefix = "NET ", 
#                            new_label = "first") %>% 
#         tab_stat_cases() %>% 
#         tab_pivot()
#     , "rds/subtotal27b.rds", update = FALSE)

expect_known_value(
    cro(subtotal(categ, c(1, 3),  c(2, 4), position = "top"))
    , "rds/subtotal27c.rds", update = FALSE)

expect_known_value(
    cro(subtotal(categ, ONE_THREE = c(1, 3),  position = "below"))
    , "rds/subtotal27d.rds", update = FALSE)

expect_known_value(
    cro(subtotal(categ, c(1, 2),  c(3, 4, 5), position = "below"))
    , "rds/subtotal27e.rds", update = FALSE)

################
suppressWarnings(RNGversion("3.5.0"))
set.seed(311265)

brand = sample(c(1:9),100, replace=TRUE)
gender = sample(1:2, 100, replace=TRUE)
wave = rep(1, 50)
data2018 = data.frame(gender, brand, wave)

brand = sample(c(1:9, 15, 21),50, replace=TRUE)
gender = sample(1:2, 50, replace=TRUE)
wave = rep(2,50)
data2019 = data.frame(gender, brand, wave)

data = rbind(data2018, data2019)

val_lab(data$gender) = c("female"=2, "male"=1)
val_lab(data$brand) = c("AA1" = 1, "AA2" = 2, "AA3" = 3, "AA4"=15, "AA5"=21, "BB1" = 4, "BB2" = 5, "BB3" = 6, "CC1" = 7, "CC2" = 8, "CC3" = 9)
val_lab(data$wave) <- c("Wave 2018"=1, "Wave 2019"=2)

expect_known_value(
    data %>%
        tab_cols(wave %nest% gender) %>%
        tab_cells(brand) %>%
        tab_subtotal_cells(c(1:3,15,21), 4:6, 7:9, position = "above", prefix = "GROUP ", new_label = "range") %>%
        tab_stat_cases() %>%
        tab_pivot()
    
    , "rds/subtotal28.rds", update = FALSE)


expect_known_value(
    data %>%
        tab_cols(wave %nest% gender) %>%
        tab_cells(brand) %>%
        tab_subtotal_cells(c(3, 15, 21,2,1), 7:9, position = "above", prefix = "GROUP ", new_label = "range") %>%
        tab_stat_cases() %>%
        tab_pivot() 
    , "rds/subtotal29.rds", update = FALSE)


expect_known_value(
    data %>%
        tab_cols(wave %nest% gender) %>%
        tab_cells(brand) %>%
        tab_subtotal_cells(c(1:3,15,21), 4:6, 7:9, position = "below", prefix = "GROUP ", new_label = "range") %>%
        tab_stat_cases() %>%
        tab_pivot()
    
    , "rds/subtotal30.rds", update = FALSE)


expect_known_value(
    data %>%
        tab_cols(wave %nest% gender) %>%
        tab_cells(brand) %>%
        tab_subtotal_cells(c(3, 15, 21,2,1), 7:9, position = "below", prefix = "GROUP ", new_label = "range") %>%
        tab_stat_cases() %>%
        tab_pivot() 
    , "rds/subtotal31.rds", update = FALSE)


expect_known_value(
    data %>%
        tab_cols(wave %nest% gender) %>%
        tab_cells(brand) %>%
        tab_subtotal_cells(c(1:3,15,21), 4:6, 7:9, position = "top", prefix = "GROUP ", new_label = "range") %>%
        tab_stat_cases() %>%
        tab_pivot()
    
    , "rds/subtotal32.rds", update = FALSE)


expect_known_value(
    data %>%
        tab_cols(wave %nest% gender) %>%
        tab_cells(brand) %>%
        tab_subtotal_cells(c(3, 15, 21,2,1), 7:9, position = "bottom", prefix = "GROUP ", new_label = "range") %>%
        tab_stat_cases() %>%
        tab_pivot() 
    , "rds/subtotal33.rds", update = FALSE)






context("experimental")
data = readRDS("rds/data.rds")

default_dataset(data)

.compute({
    reg[1:10] = NA
})

expect_error(.recode(q8_1 %to% q8_99, (1:NROW(data)<11) ~ NA))
.recode(q8r_1 %to% q8r_99, (1:NROW(data)<11) ~ NA)
# .if_val(q8r_1 %to% q8r_99, (1:NROW(data)<11) ~ NA)

expect_warning(.set_var_lab(q8r_1, "Используемые услуги"))
expect_equal_to_reference(.fre(reg), "rds/fre_real1.rds")
expect_equal_to_reference(.fre(s1), "rds/fre_real2.rds")
expect_equal_to_reference(.fre(q8r_1 %to% q8r_99), "rds/fre_real3.rds")


expect_equal_to_reference(.cro(reg, s1), "rds/cro_real1.rds")
expect_equal_to_reference(.cro_cpct(reg, s1), "rds/cro_real2.rds")
expect_equal_to_reference(.cro_tpct(q8r_1 %to% q8r_99, reg), "rds/cro_real4t.rds")

#### with weight
expect_equal_to_reference(.fre(reg, weight = weight1), "rds/fre_real1w.rds")
expect_equal_to_reference(.cro_rpct(q8r_1 %to% q8r_99, s1, weight = weight1), "rds/cro_real6wr.rds")


data(iris)
default_iris = iris
default_dataset(default_iris)

expect_error(.recode(colnames(iris), other ~ tolower))




data(iris)
default_iris = iris
default_dataset(default_iris)

.if_val(Species, from = "setosa", to = "versicolor")
.recode(Species, from = "virginica", to = "versicolor")
iris$Species[iris$Species == "setosa"] = "versicolor"
iris$Species[iris$Species == "virginica"] = "versicolor"
expect_identical(default_iris, iris)
expect_warning(.set_val_lab(vars_pattern("^Sepal"), c("Hard to say"=99)))
.if_val(vars(perl("^Sepal")), 4 %thru% hi ~ 1)
val_lab(iris$Sepal.Length) = c("Hard to say"=99)
iris$Sepal.Length[iris$Sepal.Length>=4] = 1
val_lab(iris$Sepal.Width) = c("Hard to say"=99)
iris$Sepal.Width[iris$Sepal.Width>=4] = 1
expect_identical(default_iris, iris)

data("mtcars")

default_mtcars = mtcars
default_dataset(default_mtcars)

.compute({
    mpg_by_am = ave(mpg, am, FUN = mean)
    hi_low_mpg = ifs(mpg<mean(mpg) ~ 0, mpg>mean(mpg) ~ 1)
    var_lab(hi_low_mpg) = "Miles per gallon"
    val_lab(hi_low_mpg) = num_lab("
                                 0 Low
                                 1 High
                                 ")
})

.apply_var_labs(vs = "Engine")
.apply_val_labs(vs = c("V-engine" = 0)) 
expect_warning(.add_val_lab(vs, c("Straight engine" = 1)))

.apply_labels(am = "Transmission",
              am = c(automatic = 0,  manual=1)
)

mtcars = within(mtcars,{
    mpg_by_am = ave(mpg, am, FUN = mean)
    hi_low_mpg = ifs(mpg<mean(mpg) ~ 0, mpg>mean(mpg) ~ 1)
    var_lab(hi_low_mpg) = "Miles per gallon"
    val_lab(hi_low_mpg) = num_lab("
                                 0 Low
                                 1 High
                                 ")
    
    vs = set_var_lab(vs, "Engine")
    vs = set_val_lab(vs, c("V-engine" = 0)) 
    vs = add_val_lab(vs, c("Straight engine" = 1))
    
    am = set_var_lab(am, "Transmission")
    am = set_val_lab(am, c(automatic = 0,  manual=1))
    
})

nn = ncol(mtcars)

expect_identical(mtcars[, c(1:(nn-2), nn, nn-1)], default_mtcars)
expect_identical(.fre(vs),fre(mtcars$vs))
expect_warning(.with(fre(vs)))
expect_identical(.calc(fre(vs)),fre(mtcars$vs))
expect_identical(.cro(am, vs), cro(mtcars$am, mtcars$vs))
expect_identical(.cro_cpct(am, vs), cro_cpct(mtcars$am, mtcars$vs))
var_lab(mtcars$mpg_by_am) = "mpg_by_am"
expect_identical(.cro_cpct(mpg_by_am, hi_low_mpg), cro_cpct(mtcars$mpg_by_am, mtcars$hi_low_mpg))

############
rm(mtcars)
data("mtcars")

default_mtcars = mtcars
default_dataset(default_mtcars)

.do_if(vs == 0, {
    mean_mpg = mean(mpg)
    vs_0 = 1
    mpg_by_am = ave(mpg, am, FUN = mean)
    hi_low_mpg = ifs(mpg<mean(mpg) ~ 0, TRUE ~ 1)
})


mtcars = within(mtcars,{
    hi_low_mpg = NA
    hi_low_mpg[vs==0] = ifelse(mpg[vs==0]<mean(mpg[vs==0]), 0,1)
    mpg_by_am = NA
    mpg_by_am[vs==0] = ave(mpg[vs==0], am[vs==0], FUN = mean)
    vs_0 = ifelse(vs == 0, 1, NA)
    mean_mpg = NA
    mean_mpg[vs == 0] = mean(mpg[vs==0])
})



expect_identical(mtcars, default_mtcars)


.modify({
    am = NULL
    hi_low_mpg = NULL
})


mtcars$am = NULL
mtcars$hi_low_mpg = NULL

expect_identical(mtcars, default_mtcars)
expect_error({.modify_if("a", {
    vs_0 = NULL
    
})})

.modify_if(vs == 0, {
    vs_0 = NULL

})

mtcars$vs_0 = NULL

expect_identical(mtcars, default_mtcars)

.modify({
    sxsxs = NULL
    ggttt = NULL
})

expect_identical(mtcars, default_mtcars)
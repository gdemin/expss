context("experimental")
data = readRDS("rds/data.rds")

default_dataset(data)

.compute({
    reg[1:10] = NA
})

expect_error(.recode(q8_1 %to% q8_99, (1:NROW(data)<11) ~ NA))
.recode(q8r_1 %to% q8r_99, (1:NROW(data)<11) ~ NA)

.set_var_lab(q8r_1, "Используемые услуги")
expect_equal_to_reference(.fre(reg), "rds/fre_real1.rds")
expect_equal_to_reference(.fre(s1), "rds/fre_real2.rds")
# expect_equal_to_reference(.fre(q8r_1 %to% q8r_99), "rds/fre_real3.rds")


expect_equal_to_reference(.cro(reg, s1), "rds/cro_real1.rds")
expect_equal_to_reference(.cro_cpct(reg, s1), "rds/cro_real2.rds")
expect_equal_to_reference(.cro_tpct(q8r_1 %to% q8r_99, reg), "rds/cro_real4t.rds")

#### with weight
expect_equal_to_reference(.fre(reg, weight = weight1), "rds/fre_real1w.rds")
expect_equal_to_reference(.cro_rpct(q8r_1 %to% q8r_99, s1, weight = weight1), "rds/cro_real6wr.rds")


data(iris)
default_iris = iris
default_dataset(default_iris)

expect_error(.recode(colnames(iris), . ~ tolower))

.filter(Species == "setosa")
expect_identical(default_iris, iris[iris$Species == "setosa", ])


data("mtcars")

default_mtcars = mtcars
default_dataset(default_mtcars)

.compute({
    mpg_by_am = ave(mpg, am, FUN = mean)
    hi_low_mpg = ifs(mpg<mean(mpg) ~ 0, mpg>mean(mpg) ~ 1)
    var_lab(hi_low_mpg) = "Miles per gallon"
    val_lab(hi_low_mpg) = ml_left("
                                 0 Low
                                 1 High
                                 ")
})

.set_var_lab(vs, "Engine")
.set_val_lab(vs, c("V-engine" = 0)) 
.add_val_lab(vs, c("Straight engine" = 1))

.set_var_lab(am, "Transmission")
.set_val_lab(am, c(automatic = 0,  manual=1))

mtcars = within(mtcars,{
    mpg_by_am = ave(mpg, am, FUN = mean)
    hi_low_mpg = ifs(mpg<mean(mpg) ~ 0, mpg>mean(mpg) ~ 1)
    var_lab(hi_low_mpg) = "Miles per gallon"
    val_lab(hi_low_mpg) = ml_left("
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
expect_identical(.with(fre(vs)),fre(mtcars$vs))
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
    hi_low_mpg = ifs(mpg<mean(mpg) ~ 0, other = 1)
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








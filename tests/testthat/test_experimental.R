context("experimental")

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

.apply_val_labs(
    Sepal.Length = c("Hard to say"=99),
    Sepal.Width = c("Hard to say"=99)
)
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
.compute({
    add_val_lab(vs) = c("Straight engine" = 1)
})

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
expect_identical(.calc(fre(vs)),fre(mtcars$vs))
expect_identical(.cro(am, vs), cro(mtcars$am, mtcars$vs))
expect_identical(.cro_cases(am, vs), cro_cases(mtcars$am, mtcars$vs))
expect_identical(.cro_cpct(am, vs), cro_cpct(mtcars$am, mtcars$vs))
expect_identical(.cro_mean_sd_n(mpg, vs), .cro_mean_sd_n(list(mpg = mtcars$mpg), mtcars$vs))
var_lab(mtcars$mpg_by_am) = "mpg_by_am"
expect_identical(.cro_cpct(list(unvr(mpg_by_am)), list(unvr(hi_low_mpg))), 
                 cro_cpct(list(unvr(mtcars$mpg_by_am)), list(unvr(mtcars$hi_low_mpg))))

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

expect_error(
    .modify_if(vs == 0, {
        vs_0 = NULL
        
    })
)


.modify({
    sxsxs = NULL
    ggttt = NULL
})

expect_identical(mtcars, default_mtcars)

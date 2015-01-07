context("factor.with_labels")

no_lab = rep(1:2,3)
vec_with_lab = no_lab
var_lab(vec_with_lab) = "Fruits"
val_lab(vec_with_lab) = c(Apple=1,Bananas=2)

expect_identical(as.factor(unlab(vec_with_lab)),factor(no_lab))
expect_identical(as.factor(unvr(vec_with_lab)),factor(no_lab,levels = 1:2,labels= c("Apple","Bananas")))
expect_identical(as.factor(vec_with_lab),factor(no_lab,levels = 1:2,labels= c("Fruits|Apple","Fruits|Bananas")))
expect_identical(as.factor(unvl(vec_with_lab)),factor(no_lab,levels = 1:2,labels= c("Fruits|1","Fruits|2")))



context("factor.with_labels - some values without labels")
no_lab = c(no_lab,5:6)
vec_with_lab = c(vec_with_lab,5:6)

expect_identical(as.factor(unlab(vec_with_lab)),
                 factor(no_lab))
expect_identical(as.factor(unvr(vec_with_lab)),
                 factor(no_lab,levels = c(1:2,5:6),labels= c("Apple","Bananas","5","6")))
expect_identical(as.factor(vec_with_lab),
                 factor(no_lab,levels =  c(1:2,5:6),labels= c("Fruits|Apple","Fruits|Bananas","Fruits|5","Fruits|6")))
expect_identical(as.factor(unvl(vec_with_lab)),
                factor(no_lab,levels =  c(1:2,5:6),labels= c("Fruits|1","Fruits|2","Fruits|5","Fruits|6")))

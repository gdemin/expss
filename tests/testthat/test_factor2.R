context("factor2.labelled")

no_lab = rep(1:2,3)
vec_with_lab = no_lab
var_lab(vec_with_lab) = "Fruits"
val_lab(vec_with_lab) = c(Apple=1,Bananas=2)

expect_identical(as.factor2(unlab(vec_with_lab)),factor(no_lab))
expect_identical(as.factor2(unvr(vec_with_lab)),factor(no_lab,levels = 1:2,labels= c("Apple","Bananas")))
expect_identical(as.factor2(vec_with_lab),factor(no_lab,levels = 1:2,labels= c("Fruits|Apple","Fruits|Bananas")))
expect_identical(as.factor2(unvl(vec_with_lab)),factor(no_lab,levels = 1:2,labels= c("Fruits|1","Fruits|2")))

### Common usage ###

test_ds = data.frame(total = 1, s2b = sample(2:3,100,replace = TRUE))
test_ds = unlab(test_ds)
val_lab(test_ds$s2b) = c('18 - 26' = 2, '27 - 35' = 3)

# head(factor(test_ds$s2b))

expect_identical(levels(factor2(test_ds$s2b)), names(val_lab(test_ds$s2b)))


context("factor.labelled - some values without labels")
no_lab = c(no_lab,5:6)
vec_with_lab = c(vec_with_lab,5:6)

expect_identical(as.factor2(unlab(vec_with_lab)),
                 factor(no_lab))
expect_identical(as.factor2(unvr(vec_with_lab)),
                 factor(no_lab,levels = c(1:2,5:6),labels= c("Apple","Bananas","5","6")))
expect_identical(as.factor2(vec_with_lab),
                 factor(no_lab,levels =  c(1:2,5:6),labels= c("Fruits|Apple","Fruits|Bananas","Fruits|5","Fruits|6")))
expect_identical(as.factor2(unvl(vec_with_lab)),
                factor(no_lab,levels =  c(1:2,5:6),labels= c("Fruits|1","Fruits|2","Fruits|5","Fruits|6")))


context("ordered, as.ordered")

expect_identical(as.ordered2(unlab(vec_with_lab)),
                 ordered(no_lab))
expect_identical(ordered2(unvr(vec_with_lab)),
                 ordered(no_lab,levels = c(1:2,5:6),labels= c("Apple","Bananas","5","6")))
expect_identical(as.ordered2(vec_with_lab),
                 ordered(no_lab,levels =  c(1:2,5:6),labels= c("Fruits|Apple","Fruits|Bananas","Fruits|5","Fruits|6")))
expect_identical(as.ordered2(unvl(vec_with_lab)),
                 ordered(no_lab,levels =  c(1:2,5:6),labels= c("Fruits|1","Fruits|2","Fruits|5","Fruits|6")))

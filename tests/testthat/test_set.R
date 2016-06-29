context("set")

data(iris)
ir = iris
ir_test = iris

ir_test[,c("var1", "var2", "var3")] = NA

default_dataset(ir)

.set("var`1:3`")

expect_identical(ir, ir_test)

w = iris

w = modify(w, 
           set("var`1:3`")
            )

expect_identical(w, ir_test)


w = iris

expect_error(
    modify(w, {
           short = NA
           bad = if_val(short, NA ~ Species)
}
))

w_test = iris 
w_test$short = NA
w_test$good = as.character(w_test$Species)

expect_identical(
    modify(w, {
        set("short")
        good = if_val(short, NA ~ as.character(Species))
    }
    ), w_test)


data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir

ir_test[,subst("v`1:4`r")] = ir[,subst("v`1:4`")]

default_dataset(ir)

.set("v`1:4`r", v1 %to% v4)

expect_identical(ir, ir_test)
###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir

ir_test[,subst("v`1:4`r")] = ir[,subst("v`1:4`")]

ir = modify(ir, {
    set("v`1:4`r", v1 %to% v4)
    
}
)

expect_identical(ir, ir_test)

#########

data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir

ir_test[,subst("v`1:4`r")] = NA
ir_test[ir_test$Species == "setosa",subst("v`1:4`r")] = ir[ir_test$Species == "setosa",subst("v`1:4`")]

ir = modify_if(ir, Species == "setosa", {
    set("v`1:4`r", v1 %to% v4)
    
    }
)

expect_identical(ir, ir_test)

#########

data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir
default_dataset(ir)
ir_test[,subst("v`1:4`r")] = NA
ir_test[ir_test$Species == "setosa",subst("v`1:4`r")] = ir[ir_test$Species == "setosa",subst("v`1:4`")]

.modify_if(Species == "setosa", {
    set("v`1:4`r", v1 %to% v4)
    
}
)

expect_identical(ir, ir_test)


#########

data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir
default_dataset(ir)
ir_test[,subst("v`1:4`r")] = NA
ir_test[ir_test$Species == "setosa",subst("v`1:4`r")] = ir[ir_test$Species == "setosa",subst("v`1:4`")]

.do_if(Species == "setosa", {
    set("v`1:4`r", v1 %to% v4)
    
}
)

expect_identical(ir, ir_test)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir
default_dataset(ir)
ir_test[,subst("v`1:4`r")] = ir[,subst("v`1:4`")]

.compute({
    set("v`1:4`r", v1 %to% v4)
    
}
)

expect_identical(ir, ir_test)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir
default_dataset(ir)
ir_test[,subst("v`1`r")] = 1L 
ir_test[,subst("v`2`r")] = 2L
ir_test[,subst("v`3`r")] = 3L
ir_test[,subst("v`4`r")] = 4L

.compute({
    set("v`1:4`r", t(1:4))
    
}
)

expect_identical(ir, ir_test)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir
default_dataset(ir)
ir_test[,subst("v`1:4`r")] = 1L:150L 


.compute({
    set(subst("v`1:4`r"), 1:150)
    
}
)

expect_identical(ir, ir_test)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir
default_dataset(ir)


expect_error(
.compute({
    set("v`1:4`r", 1:2)
    
}
)
)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir
default_dataset(ir)


expect_error(
    .compute({
        set("v`1:4`r", t(1:2))
        
    }
    )
)


###########
if(FALSE){
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir
i = 1:4
default_dataset(ir)
ir_test[,subst("v`1`r")] = 1L 
ir_test[,subst("v`2`r")] = 2L
ir_test[,subst("v`3`r")] = 3L
ir_test[,subst("v`4`r")] = 4L
i = 1:4
.compute({
    set("v`i`r", t(1:4))
    
}
)

expect_identical(ir, ir_test)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir
i = 1:4
default_dataset(ir)
ir_test[,subst("v`1`r")] = 1L 
ir_test[,subst("v`2`r")] = 2L
ir_test[,subst("v`3`r")] = 3L
ir_test[,subst("v`4`r")] = 4L
i = 1:4
.set("v`i`r", t(1:4))

expect_identical(ir, ir_test)
}


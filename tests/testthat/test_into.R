context("%into%_2")

data(iris)

w = iris


w_test = iris 
w_test$short = NA
w_test$good = as.character(w_test$Species)

# expect_identical(
#     modify(w, {
#         NA %into% short
#         good = if_val(short, NA ~ as.character(Species))
#     }
#     ), w_test)



###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir

ir_test[,subst("v`1:4`r")] = ir[,subst("v`1:4`")]

ir = modify(ir, {
    v1 %to% v4 %into% subst("v`1:4`r")
    
}
)

expect_identical(ir, ir_test)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir

ir_test[,subst("vr`1:4`")] = ir[,subst("v`1:4`")]

name1  = "vr1"
name2  = "vr4"

ir_res = modify(ir, {
    v1 %to% v4 %into% ((name1) %to% (name2))
    
}
)

expect_identical(ir_res, ir_test)

ir_res = modify(ir, {
    v1 %to% v4 %into% (..$name1 %to% ..$name2)
    
}
)

expect_identical(ir_res, ir_test)

ir_res = modify(ir, {
    v1 %to% v4 %into% (..[name1] %to% ..[name2])
    
}
)

expect_identical(ir_res, ir_test)

###########

data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir

ir_test[,subst("vr`1:4`")] = ir[,subst("v`1:4`")]

ir = modify(ir, {
    v1 %to% v4 %into% (vr1 %to% vr4)
    
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
    v1 %to% v4 %into% subst("v`1:4`r")
    
    }
)

expect_identical(ir, ir_test)

#########


###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir
ir_test[,subst("v`1`r")] = 1L 
ir_test[,subst("v`2`r")] = 2L
ir_test[,subst("v`3`r")] = 3L
ir_test[,subst("v`4`r")] = 4L

ir = compute(ir, {
    t(1:4) %into% subst("v`1:4`r")
}
)

expect_identical(ir, ir_test)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir

ir_test[,subst("v`1:4`r")] = 1L:150L 


ir = compute(ir, {
    1:150 %into% subst("v`1:4`r")
}
)

expect_identical(ir, ir_test)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir


expect_error({
ir = compute(ir, {
    1:2 %into% subst("v`1:4`r")
    
})
}
)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir


expect_error({
    ir = compute(ir, {
        t(1:2) %into% subst("v`1:4`r")
        
    }
    )
}
)


###########
# if(FALSE){
data(iris)
ir = iris
colnames(ir)[1:4] = subst("v`1:4`")
ir_test = ir
i = 1:4

ir_test[,subst("v`1`r")] = 1L 
ir_test[,subst("v`2`r")] = 2L
ir_test[,subst("v`3`r")] = 3L
ir_test[,subst("v`4`r")] = 4L
i = 1:4
ir = compute(ir, {
    t(1:4) %into% subst("v`i`r")
    
}
)

expect_identical(ir, ir_test)

###########

# }


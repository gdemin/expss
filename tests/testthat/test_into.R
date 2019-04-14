context("%into%_2")
suppressWarnings(RNGversion("3.5.0"))

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
colnames(ir)[1:4] = text_expand("v{1:4}")
ir_test = ir

ir_test[,text_expand("v{1:4}r")] = ir[,text_expand("v{1:4}")]

ir = modify(ir, {
    v1 %to% v4 %into% text_expand("v{1:4}r")
    
}
)

expect_identical(ir, ir_test)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = text_expand("v{1:4}")
ir_test = ir

ir_test[,text_expand("vr{1:4}")] = ir[,text_expand("v{1:4}")]

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
    v1 %to% v4 %into% (..[(name1)] %to% ..[(name2)])
    
}
)

expect_identical(ir_res, ir_test)

###########

data(iris)
ir = iris
colnames(ir)[1:4] = text_expand("v{1:4}")
ir_test = ir

ir_test[,text_expand("vr{1:4}")] = ir[,text_expand("v{1:4}")]

ir = modify(ir, {
    v1 %to% v4 %into% (vr1 %to% vr4)
    
}
)

expect_identical(ir, ir_test)

#########

data(iris)
ir = iris
colnames(ir)[1:4] = text_expand("v{1:4}")
ir_test = ir

ir_test[,text_expand("v{1:4}r")] = NA
ir_test[ir_test$Species == "setosa",text_expand("v{1:4}r")] = ir[ir_test$Species == "setosa",text_expand("v{1:4}")]

ir = modify_if(ir, Species == "setosa", {
    v1 %to% v4 %into% text_expand("v{1:4}r")
    
    }
)

expect_identical(ir, ir_test)

#########


###########
data(iris)
ir = iris
colnames(ir)[1:4] = text_expand("v{1:4}")
ir_test = ir
ir_test[,text_expand("v{1}r")] = 1L 
ir_test[,text_expand("v{2}r")] = 2L
ir_test[,text_expand("v{3}r")] = 3L
ir_test[,text_expand("v{4}r")] = 4L

ir = compute(ir, {
    t(1:4) %into% text_expand("v{1:4}r")
}
)

expect_identical(ir, ir_test)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = text_expand("v{1:4}")
ir_test = ir

ir_test[,text_expand("v{1:4}r")] = 1L:150L 


ir = compute(ir, {
    1:150 %into% text_expand("v{1:4}r")
}
)

expect_identical(ir, ir_test)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = text_expand("v{1:4}")
ir_test = ir


expect_error({
ir = compute(ir, {
    1:2 %into% text_expand("v{1:4}r")
    
})
}
)

###########
data(iris)
ir = iris
colnames(ir)[1:4] = text_expand("v{1:4}")
ir_test = ir


expect_error({
    ir = compute(ir, {
        t(1:2) %into% text_expand("v{1:4}r")
        
    }
    )
}
)


###########
# if(FALSE){
data(iris)
ir = iris
colnames(ir)[1:4] = text_expand("v{1:4}")
ir_test = ir
i = 1:4

ir_test[,text_expand("v{1}r")] = 1L 
ir_test[,text_expand("v{2}r")] = 2L
ir_test[,text_expand("v{3}r")] = 3L
ir_test[,text_expand("v{4}r")] = 4L
i = 1:4
ir = compute(ir, {
    t(1:4) %into% text_expand("v{i}r")
    
}
)

expect_identical(ir, ir_test)

###########

# }

tbl1 = sheet(
    id = c(1, 2, 3),
    num_a = c(1, NA, 4),
    num_b = c(NA, 99, 100),
    col_c = c("d", "e", NA)
)

tbl2 = tbl1

if_na(tbl2[ ,qc(num_a, num_b)]) = 0
expect_identical(
    compute(tbl1, {
        
        vars(perl("^num_")) %>% if_na(0) %into% perl("^num_")
        
    }), tbl2
)

tbl2[ ,qc(num_a, num_b)] = tbl2[ ,qc(num_b, num_a)]

expect_identical(
    compute(tbl1, {
        
        vars(perl("^num_")) %>% if_na(0) %into% c(fixed("num_b"), perl("^num_"))
        
    }), tbl2
)

#####################

set.seed(123)
a = as.data.frame(matrix(rnorm(30), ncol = 15))


b = compute(a, {
    
    recode(V1 %to% V10, other ~ copy) %into% (Vr2 %to% Vr11)
    
})

new_var = setNames(a[, 1:10], paste0("Vr", 2:11))

expect_identical(b, cbind(a, new_var))
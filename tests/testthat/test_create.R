context("create")

data(iris)
ir = iris
ir_test = iris

ir_test[,c("var1", "var2", "var3")] = NA

default_dataset(ir)

.create(subst("var`1:3`"))

expect_identical(ir, ir_test)

w = iris

w = modify(w, 
           create(subst("var`1:3`"))
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
        create("short")
        good = if_val(short, NA ~ as.character(Species))
    }
    ), w_test)
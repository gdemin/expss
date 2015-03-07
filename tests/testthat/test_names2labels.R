context("names2labels")

vec = 1:7

expect_identical(names2labels(vec),vec)

mat = matrix(1:9,ncol=3)

expect_identical(names2labels(mat),mat)

var_lab(mat) = "matrix"

out_mat =  mat
colnames(out_mat) = paste0("matrix|",1:3)
expect_identical(names2labels(mat),out_mat)

colnames(mat) = letters[1:3]
out_mat =  mat
colnames(out_mat) = paste0("matrix|",letters[1:3])
expect_identical(names2labels(mat),out_mat)

df = as.data.frame(mat)

expect_identical(names2labels(df),df)

var_lab(df$a) = "column a"

out_df = df
colnames(out_df)[1] = "column a"

expect_identical(names2labels(df),out_df)

colnames(out_df)[1] = "column a (a)"

expect_identical(names2labels(df, keep_names = TRUE),out_df)

var_lab(df$a) = "column a"
var_lab(df$b) = "column b"
var_lab(df$c) = "column c"

out_df = df
colnames(out_df) = paste0("column ",letters[1:3])
expect_identical(names2labels(df),out_df)

lst = as.list(df)
out_lst = as.list(out_df)
expect_identical(names2labels(lst),out_lst)

df = df[,FALSE]
out_df = out_df[,FALSE]
expect_identical(names2labels(df),out_df)


data(mtcars)
mtcars = within(mtcars,{
                var_lab(cyl) = "Number of cylinders"
                var_lab(disp) = "Displacement (cu.in.)"
                var_lab(hp) = "Gross horsepower"
                var_lab(drat) = "Rear axle ratio"
                var_lab(wt) = "Weight (lb/1000)"
                var_lab(qsec) = "1/4 mile time"
                var_lab(vs) = "V/S"
                var_lab(am) = "Transmission (0 = automatic, 1 = manual)"
                var_lab(gear) = "Number of forward gears"
                var_lab(carb) = "Number of carburetors"
})


expect_equal_to_reference(summary(lm(mpg ~ ., data = names2labels(mtcars))),"rds/lm_names2labels_1.rds")
expect_equal_to_reference(summary(lm(mpg ~ ., data = names2labels(mtcars, keep_names = TRUE))),"rds/lm_names2labels_2.rds")

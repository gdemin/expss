context("values2labels")

vec = 1:7

expect_identical(values2labels(vec),vec)

mat = matrix(1:9,ncol=3)

expect_identical(values2labels(mat),mat)



out_mat =  mat
expect_identical(values2labels(mat),out_mat)



mat[,3] = NA
out_mat[,3] = NA
expect_identical(values2labels(mat),out_mat)
expect_identical(values2labels(numeric(0)),numeric(0))



df = data.frame(a=1:3,b = 3:1,d=letters[1:3],e=NA,stringsAsFactors = FALSE)

val_lab(df$a) = c(a=1,b=2)
val_lab(df$b) = c(a=45)
val_lab(df$d) = c(a="b",b="d",e="c")
val_lab(df$e) = c(a=1,b=2)

var_lab(df$b) = "Column b"

out_df = unvl(df)

out_df$a[1] = "a"
out_df$a[2] = "b"
out_df$b = as.character(out_df$b)
var_lab(out_df$b) = "Column b"
out_df$d[2] = "a"
out_df$d[3] = "e"
out_df$e = as.character(out_df$e)

expect_identical(values2labels(df),out_df)

aaa = c(1:3, 3.5)
val_lab(aaa) = c(a=1, b = 2)

expect_identical(class(values2labels(aaa)), "character") 


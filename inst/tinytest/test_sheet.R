context("sheet")

df1 = data.frame(a = letters[1:3], "This is my long name" = 1:3, 
                 check.names = FALSE,
                 stringsAsFactors = FALSE
                 )
df2 = sheet(a = letters[1:3], "This is my long name" = 1:3)

expect_identical(df1, df2)



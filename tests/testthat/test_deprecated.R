context("deprecated")

data(iris)

test_iris = iris
test_iris[, paste0(letters[1], seq_len(1))] = 1
test_iris[, paste0(letters[2], seq_len(2))] = 2
test_iris[, paste0(letters[3], seq_len(3))] = 3


res_iris = suppressWarnings(do_repeat(iris, i = c(1, 2, 3),
                     {
                         set(paste0(letters[i], seq_len(i)), i)
                     }))


expect_identical(res_iris, test_iris)

expect_error(modify(dfs, {set = 42}))
expect_error(modify_if(dfs2,test %in% 2:4,  {set = 42}))

data(iris)
ir = iris
ir_test = iris

ir_test[,c("var1", "var2", "var3")] = NA

default_dataset(ir)

suppressWarnings({
.set("var`1:3`")
})

expect_identical(ir, ir_test)
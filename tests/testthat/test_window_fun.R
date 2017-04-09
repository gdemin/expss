context("window_fun")

expect_error(window_fun(1:3))
expect_error(window_fun(1:10, 1:2, mean))
expect_error(window_fun(1:10, mean, 1:2))
expect_error(window_fun(1:10, rep(1:2, 5), mean, sum))

expect_identical(window_fun(1:3, mean), ave(1:3))
expect_identical(window_fun(1:3, "total", mean), ave(1:3))
data(warpbreaks)
expect_identical(
    window_fun(warpbreaks$breaks, warpbreaks$wool, mean),
    ave(warpbreaks$breaks, warpbreaks$wool, FUN = mean)
)
expect_identical(
    window_fun(warpbreaks$breaks, warpbreaks$tension, function(x) mean(x, trim = 0.1)),
    ave(warpbreaks$breaks, warpbreaks$tension, FUN = function(x) mean(x, trim = 0.1))
)

expect_identical(
    window_fun(1:10, rep(1:2, 5), function(x) paste(x, collapse = ",")),
    c("1,3,5,7,9",  "2,4,6,8,10", "1,3,5,7,9",  "2,4,6,8,10", "1,3,5,7,9",  "2,4,6,8,10",
      "1,3,5,7,9",  "2,4,6,8,10", "1,3,5,7,9",  "2,4,6,8,10")
)

expect_identical(
    window_fun(1:10, rep(1:2, 5), "total", function(x) paste(x, collapse = ",")),
    c("1,3,5,7,9",  "2,4,6,8,10", "1,3,5,7,9",  "2,4,6,8,10", "1,3,5,7,9",  "2,4,6,8,10",
      "1,3,5,7,9",  "2,4,6,8,10", "1,3,5,7,9",  "2,4,6,8,10")
)

if(capabilities('long.double')){
    # library(testthat)
    # library(expss)
    options(width = 1000)
    options(covr = TRUE)
    testthat::test_check("expss")
}
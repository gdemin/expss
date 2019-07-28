if(capabilities('long.double')){
    library(testthat)
    library(expss)
    options(covr = TRUE)
    options(width = 1000)
    testthat::test_check("expss")
}
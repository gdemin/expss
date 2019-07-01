if(capabilities('long.double')){
    library(testthat)
    library(expss)
    options(covr = FALSE)
    options(width = 1000)
    testthat::test_check("expss")
}
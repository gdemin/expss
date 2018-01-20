if(capabilities('long.double')){
    library(testthat)
    library(expss)
    options(width = 1000)
    options(covr = TRUE)
    test_check("expss")
}
if( require(testthat) && 
    require(dplyr)  && 
    require(stats) && 
    require(utils) && 
    require(foreign) && 
    require(expss)) {
    lapply(dir("testthat/", full.names = TRUE, pattern = "\\.R$"), function(path) {
        data("iris")
        data("mtcars")
        source(path, chdir = TRUE, local = TRUE)
        rm(list = ls())
    })
}

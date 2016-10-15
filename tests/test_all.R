library(testthat) 
library(dplyr) 
library(stats)  
library(utils)  
library(foreign)  
library(expss) 
options(covr = TRUE)
lapply(dir("testthat/", full.names = TRUE, pattern = "\\.R$"), function(path) {
    data("iris")
    data("mtcars")
    source(path, chdir = TRUE, local = TRUE)
    rm(list = ls())
})


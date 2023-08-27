if(capabilities('long.double')){
    if ( requireNamespace("tinytest", quietly=TRUE) ){
        options(covr = FALSE)
        options(width = 1000)
        data.table::setDTthreads(2)
        tinytest::test_package("expss")
    }

}
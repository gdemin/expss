if(capabilities('long.double')){
    if ( requireNamespace("tinytest", quietly=TRUE) ){
        options(covr = TRUE)
        options(width = 1000)
        data.table::setDTthreads(2)
        expect_output_file = function(expr, filepath){
            output = capture.output(eval.parent(substitute(expr)))
            reference = readLines(filepath)
            tinytest::expect_equal(output, reference)
        }
        context = function(text){
            cat("\nCONTEXT:", text, "\n")
            invisible(NULL)
        }
        expss::expss_round_half_to_even(TRUE)
        expss::expss_digits(NULL)
        tinytest::test_package("expss", remove_side_effects=FALSE)
    }

}

if(FALSE){
    # for manual testinf
    options(covr = TRUE)
    expect_output_file = function(expr, filepath){
        output = capture.output(eval.parent(substitute(expr)))
        reference = readLines(filepath)
        tinytest::expect_equal(output, reference)
    }
    
    context = function(text){
        cat("\nCONTEXT:", text, "\n")
        invisible(NULL)
    }
    
    library(expss)
    expss_round_half_to_even(TRUE)
    expss_digits(NULL)
    tinytest::test_all(remove_side_effects=FALSE)
    # 
    # res = covr::package_coverage(quiet = FALSE, type = "all", combine_types = TRUE, clean = FALSE)
    # res
    # covr::zero_coverage(res)
}
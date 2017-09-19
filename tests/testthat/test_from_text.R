context("from_text")

result = sheet(
    a = 1:3,
    b = c(1.5, 2.5, 3),
    c = c("a", "b", "c")
)


test = from_text("
                  a b   c
          \t      1 1.5 a
# commented line
                  2 2.5 b
                  3 3   c
                  
                  ")


expect_identical(test, result)


test = from_text_csv("
                 a,b,c
                 \t      1,1.5,a
                 # commented line
                 2,2.5,b

                 3,3,c
                 
                 ")


expect_identical(test, result)

test = from_text_csv2("
                 a;b;c
                     \t      1;1,5;a
                     # commented line
                     2;2,5;b
                     
                     3;3;c
                     
                     ")

expect_identical(test, result)

test = from_text_tab("
                 a\tb\tc
                     \t      1\t1.5\ta
                     # commented line
                     2\t2.5\tb
                     
                     3\t3\tc
                     
                     ")


expect_identical(test, result)

test = from_text_tab2("
                      a\tb\tc
                      \t      1\t1,5\ta
                      # commented line
                      2\t2,5\tb
                      
                      3\t3\tc
                      
                      ")

expect_identical(test, result)

test = from_text_tab("
        a	b	c
                     1	1.5	a
                     2	2.5	b
                     3	3	c
                     ")
expect_identical(test, result)

test = from_text("
        a	b	c
                     1	1.5	a
                     2	2.5	b
                     3	3	c
                     ")
expect_identical(test, result)

result = sheet(
    V1 = 1:3,
    V2 = c(1.5, 2.5, 3),
    V3 = c("a", "b", "c")
)


test = from_text("
                 
                 \t      1 1.5 a
                 # commented line
                 2 2.5 b
                 3 3   c
                 
                 ", header = FALSE)

expect_identical(test, result)


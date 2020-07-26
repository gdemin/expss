# example with dplyr
context("dplyr count_if")
suppressWarnings(RNGversion("3.5.0"))

if(suppressWarnings(require(dplyr, quietly = TRUE))){
    set.seed(123)
    df2 = as.data.frame(
        matrix(sample(c(1:10,NA),30,replace = TRUE),10)
    )
    result = data.frame(df2,
                        exact=c(0,1,2,0,1,1,0,0,0,0),
                        greater=c(1,1,0,1,0,1,0,1,0,0),
                        range=c(0,2,3,1,1,1,2,1,1,1),
                        na=c(1,0,0,1,1,0,0,0,0,1),
                        not_na=c(2,3,3,2,2,3,3,3,3,2)
    )
    expect_equal(df2  %>% mutate(exact = count_row_if(8,V1,V2,V3),
                                 greater = count_row_if(gt(8),V1,V2,V3),
                                 range = count_row_if(5:8,V1,V2,V3),
                                 na = count_row_if(is.na,V1,V2,V3),
                                 not_na = count_row_if(not_na,V1,V2,V3)),
                 result)
    
    expect_equal(df2  %>% mutate(exact = count_row_if(8,V1,V2,V3),
                                 greater = count_row_if(gt(8),V1,V2,V3),
                                 range = count_row_if(5:8,V1,V2,V3),
                                 na = count_row_if(is.na,V1,V2,V3),
                                 not_na = count_row_if(not_na,V1,V2,V3)),
                 result)
} else {
    cat("dplyr not found\n")
}

context("if_na as_tibble")
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    
    
    a = as_tibble(data.frame(a = 1:4, b = 5:8, d = 10:13))
    
    b = a
    expect_identical(if_na(a, 2), b)
    
    a[1,1] = NA
    b[1,1] = 2
    
    expect_identical(if_na(a, 2L), b)
    
    a[4,1] = NA 
    b[4,1] = 2
    expect_identical(if_na(a, 2L), b)
    
    b[1,1] = 4L
    b[4,1] = 1L
    b$a = as.integer(b$a)
    expect_equal(if_na(a, 4:1), b)
    
    a[1,3] = NA
    b[1,3] = 4L
    b$d = as.integer(b$d)
    expect_equal(if_na(a, 4:1), b)
    
    
} else {
    cat("dplyr not found\n")
}

# make data.frame 
set.seed(123)
group = sample(1:3, 30, replace = TRUE)
param = runif(30)
param[sample(30, 10)] = NA # place 10 NA's
df = data.frame(group, param)

context("if_na help dplyr")
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    # replace NA's with group means
    df_clean = df %>% group_by(group) %>% 
        mutate(
            param = if_na(param, mean(param, na.rm = TRUE))
        )
    
    df = within(df, {
        param[group==1 & is.na(param)] = mean(param[group==1], na.rm = TRUE)
        param[group==2 & is.na(param)] = mean(param[group==2], na.rm = TRUE)
        param[group==3 & is.na(param)] = mean(param[group==3], na.rm = TRUE)
    })
    
    expect_identical(as.data.frame(df_clean), df)
} else {
    cat("dplyr not found\n")
}

context("if_val dplyr")
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    
    
    
    x = c(1,3,1,3,NA)
    y = c(8,8,8,9,9)
    z = c(4,4,4,5,5)
    # 
    dfs = data.frame(
        x = c(2,4,2,4,NA),
        y = c(18,18,18,19,19),
        z = c(14,14,14,15,15)
        
    )  %>% as_tibble()
    
    dfs  = dfs %>% mutate(
        w = if_val(x, from_to(c(gt(2), other), to = list(y, copy)))
        # zzz = predict(lm(x ~ y))
        # w = ifelse(x>2, y , x)
    )
    
    expect_identical(dfs$w, c(2, 18, 2, 19, NA))
    
    dfs$x = NULL
    dfs$w = NULL
    dfs  = dfs %>% mutate(
        w = if_val(x, from_to(c(gt(2), other), to = list(y, copy)))
    )
    expect_identical(dfs$w, c(1, 18, 1, 19, NA))
    dfs  = dfs %>% mutate(
        w = if_val(y, 18 ~ 1, 19 ~ 2)
    )
    expect_identical(dfs$w, c(1, 1, 1, 2, 2))
} else {
    cat("dplyr not found\n")
}

context("if_val 'from, to' notation dplyr")

if(suppressWarnings(require(dplyr, quietly = TRUE))){
    
    x = c(1,3,1,3,NA)
    y = c(8,8,8,9,9)
    z = c(4,4,4,5,5)
    
    dfs = data.frame(
        x = c(2,4,2,4,NA),
        y = c(18,18,18,19,19),
        z = c(14,14,14,15,15)
        
    )
    
    dfs  = dfs %>% mutate(
        w = if_val(x, from_to(list(gt(2), other), to = list(y, copy)))
    )
    
    expect_identical(dfs$w, c(2, 18, 2, 19, NA))
    
    dfs$x = NULL
    dfs$w = NULL
    dfs  = dfs %>% mutate(
        w = if_val(x, from_to(list(gt(2), other), to = list(y, copy)))
    )
    expect_identical(dfs$w, c(1, 18, 1, 19, NA))
} else {
    cat("dplyr not found\n")
}

context("vlookup as_tibble")
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    
    
    dict = as_tibble(data.frame(num=1:26, small=letters, cap=LETTERS, stringsAsFactors = FALSE))
    # rownames(dict) = paste0('rows',1:26)
    expect_identical(vlookup_df(1:3, dict), dict[1:3,])
    
    expect_identical(vlookup(c(45, 1:3, 58, NA), dict, result_column='cap'), c(NA, "A", "B", "C", NA, NA))
    expect_identical(vlookup_df(c('z', 'd', 'f', 'd'), dict, lookup_column = 'small'), dict[c(26, 4, 6, 4),])
    # expect_identical(vlookup_df(c('rows1', 'rows5', 'rows2', 'rows2'), dict, result_column = c("small", "cap"), lookup_column = 'row.names'),
    #                  dict[c(1, 5, 2, 2), c("small", "cap")])
    
} else {
    cat("dplyr not found\n")
}


data(iris)

if(suppressWarnings(require(dplyr, quietly = TRUE))){
    expect_identical(as_tibble(iris) %n_d% "Species", as_tibble(iris)[, -5]) # remove column Species
    expect_identical(as_tibble(iris) %n_i% perl("^Sepal"), as_tibble(iris)[, 1:2])
    # leave column "Species" and columns which start with "Sepal" 
    expect_identical(as_tibble(iris) %n_i% (perl("^Sepal")|"Species"), as_tibble(iris)[, c(1:2,5)]) 
} else {
    cat("dplyr not found\n")
}

# example with dplyr
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    set.seed(123)
    df2 = as.data.frame(
        matrix(sample(c(1:10,NA),30,replace = TRUE),10)
    )
    result = data.frame(df2,
                        exact=c(0,8,16,0,8,8,0,0,0,0),
                        greater=c(10,9,0,10,0,10,0,10,0,0),
                        range=c(0,13,21,7,8,8,12,7,7,6),
                        na=c(0,0,0,0,0,0,0,0,0,0),
                        not_na=c(14,22,21,17,10,19,15,18,15,8)
    )
    expect_equal(df2  %>% mutate(exact = sum_row_if(8,V1,V2,V3),
                                 greater = sum_row_if(gt(8),V1,V2,V3),
                                 range = sum_row_if(5:8,V1,V2,V3),
                                 na = sum_row_if(is.na,V1,V2,V3),
                                 not_na = sum_row_if(not_na,V1,V2,V3)),
                 result)
} else {
    cat("dplyr not found\n")
}


context("mis_val as_tibble")
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    
    
    a = as_tibble(data.frame(a=1:5,b=5:1))
    
    b = a
    b[a$a>3,"a"] = NA
    b[a$b>3,"b"] = NA
    expect_equal(mis_val(a, gt(3)),b)
    expect_equal(mis_val(a, 4:5),b)
    
    
    b = a
    b[1:2,] = NA
    
    expect_equal(mis_val(a, when(c(TRUE, TRUE, FALSE,FALSE,FALSE))),b)
    

} else {
    cat("dplyr not found\n")
}



# example with dplyr
context("modify dplyr")
dfs = data.frame(
    zz = 42,
    b_3 = 44,
    aa = 10 %r% 5,
    b_ = 20 %r% 5,
    b_1 = 11 %r% 5,
    b_2 = 12 %r% 5,
    b_4 = 14 %r% 5,
    b_5 = 15 %r% 5 
)

result_dfs = dfs
result_dfs$b_total = with(dfs, sum_row(b_1, b_2, b_4, b_5))
set.seed(1)
result_dfs$b_total = with(dfs, sum_row(b_1, b_2, b_4, b_5))
# result_dfs$random_numer = runif(nrow(dfs))

dfs2 = dfs
dfs2$test = 1:5

result_dfs2 = dfs2
# result_dfs2$a_total = ifelse(dfs2$test %in% 2:4, sum_row(a_1, a_2, a_4, a_5), NA)
result_dfs2$b_total = ifelse(dfs2$test %in% 2:4, with(dfs, sum_row(b_1, b_2, b_4, b_5)), NA)
result_dfs2$aa = ifelse(dfs2$test %in% 2:4, result_dfs2$aa+1, result_dfs2$aa)


if(suppressWarnings(require(dplyr, quietly = TRUE))){
    
    expect_identical(
        as_tibble(dfs) %>% modify( {
            b_total = rowSums(vars(from("b_1") & to("b_5")))
        }), 
        as_tibble(result_dfs)
    )
    
    expect_identical(
        as_tibble(dfs) %>% modify( {
            b_total = rowSums(vars(b_1 %to% b_5))
        }), 
        as_tibble(result_dfs)
    )
    
    expect_identical(
        as_tibble(dfs) %>% modify( {
            b_total = sum_row(b_1 %to% b_5)
        }), 
        as_tibble(result_dfs)
    )
    
    
} else {
    cat("dplyr not found\n")
}

set.seed(1)
result_dfs2[result_dfs2$test %in% 2:4, "random_numer"] = runif(3) 
result_dfs2$random_numer = NULL
context("modify_if dplyr")
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    expect_identical(
        modify_if(as_tibble(dfs2), test %in% 2:4,
                  {
                      b_total = sum_row(b_1 %to% b_5)
                      aa = aa + 1
                  }), 
        as_tibble(result_dfs2)
    )
    
    expect_error(
        modify_if(as_tibble(dfs2), test %in% 2:4,
                  {
                      a_total = sum_row(a_1 %to% a_5)
                      b_total = sum_row(b_1 %to% b_5)
                      aa = aa + 1
                  })
    )
    
} else {
    cat("dplyr not found\n")
}


# example with dplyr
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    set.seed(123)
    df2 = as.data.frame(
        matrix(sample(c(1:10,NA),30,replace = TRUE),10)
    )
    result = data.frame(df2,
                        exact=c(0,8,16,0,8,8,0,0,0,0)/c(0,1,2,0,1,1,0,0,0,0),
                        greater=c(10,9,0,10,0,10,0,10,0,0)/c(1,1,0,1,0,1,0,1,0,0),
                        range=c(0,13,21,7,8,8,12,7,7,6)/c(0,2,3,1,1,1,2,1,1,1),
                        na=c(0,0,0,0,0,0,0,0,0,0)/c(0,0,0,0,0,0,0,0,0,0),
                        not_na=c(14,22,21,17,10,19,15,18,15,8)/c(2,3,3,2,2,3,3,3,3,2)
    )
    expect_equal(df2  %>% mutate(exact = mean_row_if(8,V1,V2,V3),
                                 greater = mean_row_if(gt(8),V1,V2,V3),
                                 range = mean_row_if(5:8,V1,V2,V3),
                                 na = mean_row_if(is.na,V1,V2,V3),
                                 not_na = mean_row_if(not_na,V1,V2,V3)),
                 result)
} else {
    cat("dplyr not found\n")
}


set.seed(123)
group = sample(1:3, 30, replace = TRUE)
param = runif(30)
param[sample(30, 10)] = NA # place 10 NA's
df = data.frame(group, param)

if(suppressWarnings(require(dplyr, quietly = TRUE))){
    # replace NA's with group means
    df_clean = df %>% group_by(group) %>% 
        mutate(
            param = if_val(param, from_to(list(NA, other), to = list(mean_col(param), copy)))
        )
    
    df = within(df, {
        param[group==1 & is.na(param)] = mean(param[group==1], na.rm = TRUE)
        param[group==2 & is.na(param)] = mean(param[group==2], na.rm = TRUE)
        param[group==3 & is.na(param)] = mean(param[group==3], na.rm = TRUE)
    })
    
    expect_identical(as.data.frame(df_clean), df)
} else {
    cat("dplyr not found\n")
}


context("if_val with NA as_tibble")
if(suppressWarnings(require(dplyr, quietly = TRUE))){
    
    
    a = as_tibble(data.frame(a = 1:4, b = 5:8, d = 10:13))
    
    b = a
    #rownames(b) = rownames(data.frame(a = 1:4, b = 5:8, d = 10:13))
    expect_equal(recode(a, NA ~ 2, other ~ copy), b)
    
    a[1,1] = NA
    b[1,1] = 2
    
    expect_equal(recode(a, NA ~ 2L, other ~ copy), b)
    
    a[4,1] = NA 
    b[4,1] = 2
    expect_equal(recode(a, NA ~ 2L, other ~ copy), b)
    
    b[1,1] = 4L
    b[4,1] = 1L
    b$a = as.integer(b$a)
    expect_equal(recode(a, NA ~ 4:1, other ~ copy), b)
    
    a[1,3] = NA
    b[1,3] = 4L
    b$d = as.integer(b$d)
    expect_equal(recode(a, NA ~ 4:1, other ~ copy), b)
    
    b[1,1] = 3
    b[4,1] = 3
    b[1,3] = 1
    b$a = as.integer(b$a)
    b$d = as.integer(b$d)
    expect_error(if_val(a, NA ~ t(3:2), other ~ copy))
    expect_error(if_val(a, NA ~ 3:2, other ~ copy))
    
    
    b[1,1] = 4
    b[4,1] = 1
    b[1,3] = -1
    
    
} else {
    cat("dplyr not found\n")
}
#' @export
'%a%' = function(e1, e2){
    append(e1, e2)
}

#' @export
'%m%' = function(e1, e2){
    rep(e1, e2)
}

#' @export
'%d%' = function(e1, e2){
    if (is.function(e2)){
        e1[!e2(e1)]
    } else {
        e1[!(e1 %in% e2)]
    }
}

#' @export
'%i%' = function(e1, e2){
    if (is.function(e2)){
        e1[e2(e1)]
    } else {
        e1[e1 %in% e2]
    }
}

#' @export
'%x%' = function(e1, e2){
    c(e1[!(e1 %in% e2)],e2[!(e2 %in% e1)])
}
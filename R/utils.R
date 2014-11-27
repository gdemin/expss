## similar to stopifnot but with message

stopifnot_message = function(cond,message){
    if (!isTRUE(cond)) stop(message)
    invisible()
}
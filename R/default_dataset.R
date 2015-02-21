


#' @export
default_dataset = local({
    dataset = NULL
    dataset_name = NULL
    function(x){
        if(missing(x)){
            stopif(is.null(dataset),"Default dataset isn't defined. Use 'default_dataset(dataset_name)'.")
            cat("Default dataset:",dataset_name,"\n")
            return(dataset)
        } else {
            if (!is.null(x)){
                if("formula" %in% class(x)){
                    dataset <<- x
                    dataset_name <<- all.vars(x)[1]
                } else {
                    dataset_name <<- as.character(substitute(x))
                    dataset <<- as.formula(paste0("~",dataset_name),env = parent.frame())
                }
                cat("Set default dataset to",dataset_name,"\n")
                return(invisible(dataset))
            } else {
                dataset_name <<- NULL
                dataset <<- NULL
                cat("Default dataset disabled.\n")
                return(invisible(NULL))
            }
        }
    }
})


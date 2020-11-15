#' Get or set reference to default dataset. Experimental feature.
#' 
#' Use data.frame or data.frame name to set it as default. Use NULL as an
#' argument to disable default dataset. If argument is missing then function
#' will return reference to default dataset. Use \link{ref} to modify it. Also
#' see \link{.compute} for usage patterns.
#' 
#' @param x data.frame or data.frame name which we want to make default for some operations. 
#' @return formula reference to default dataset or NULL
#' @seealso \link{ref} 
#' @examples
#' 
#' data(iris)
#' default_iris = iris
#' default_dataset(default_iris) # set default dataset
#' 
#' .compute({
#'     new_col = 1
#'     Sepal.Length = Sepal.Length*2 
#' })
#' 
#' # for comparison
#' 
#' iris$new_col = 1
#' iris$Sepal.Length = iris$Sepal.Length*2 
#' identical(iris, default_iris) # should be TRUE
#' 
#' default_dataset(NULL) # disable default dataset
#' 
#' @export
default_dataset = local({
    dataset = NULL
    dataframe_name = NULL
    function(x){
        .Deprecated("", 
                    msg = "'default_dataset' functionality is deprecated because it seems no one need it.\n
                    If you use 'default_dataset' please file an issue at https://github.com/gdemin/expss/issues/")
        if(missing(x)){
            stopif(is.null(dataset),"Default dataset isn't defined. Use 'default_dataset(dataframe_name)'.")
            message(paste0("Default dataset: '",dataframe_name,"'\n"))
            return(dataset)
        } else {
            
            if (!is.null(x)){
                if("formula" %in% class(x)){
                    dataset <<- x
                    dataframe_name <<- all.vars(x)[1]
                } else {
                    dataframe_name <<- as.character(substitute(x))
                    dataset <<- stats::as.formula(paste0("~",dataframe_name),env = parent.frame())
                }
                cat(paste0("Set default dataset to '",dataframe_name,"'\n"))
                return(invisible(dataset))
            } else {
                dataframe_name <<- NULL
                dataset <<- NULL
                cat("Default dataset disabled.\n")
                return(invisible(NULL))
            }
        }
    }
})


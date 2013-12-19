### =========================================================================
### FaGFileViews methods
### =========================================================================

setMethod(FaGFileViews, c(fileRanges="GRanges"), 
          function(filePaths=character(0),
                   fileIndices=filePaths,
                   fileSamples=DataFrame(row.names=
                     make.unique(basename(filePaths))),
                   fileRanges,
                   fileExperiment=list(),
                   yieldSize=NA_integer_,
                   .views_on_file=new.env(parent=emptyenv()), ...)
{
    new("FaGFileViews", ..., filePaths=filePaths, fileIndices=fileIndices,
        fileSamples=fileSamples, fileRanges=fileRanges,
        fileExperiment=fileExperiment, yieldSize=yieldSize,
        .views_on_file=.views_on_file)
})

setMethod(FaGFileViews, c(fileRanges="missing"), 
          function(filePaths=character(0),
                   fileIndices=filePaths,
                   fileSamples=DataFrame(row.names=
                     make.unique(basename(filePaths))),
                   fileRanges,
                   fileExperiment=list(),
                   yieldSize=NA_integer_,
                   .views_on_file=new.env(parent=emptyenv()), ...)
{
    if (length(filePaths) != 0L)
    {
        ## Guess ranges from Fa index files 
        pathsOk <- sapply(filePaths, function(fl) {
            file.exists(fl) && !file.info(fl)$isdir
        })
        if (all(pathsOk)) {
            rngs <- lapply(filePaths, scanFaIndex)
            rngs <- reduce(do.call(c, rngs))
            if (!length(rngs))
                stop("Rsamtools internal: could not determine fileRanges")
        } else {
            warning("some files do not exist; fileRanges not defined")
            fileRanges <- GRanges()
        }
    } else {
        fileRanges <- GRanges()
    }
    FaGFileViews(filePaths=filePaths, fileIndices=fileIndices, 
                fileSamples=fileSamples, fileRanges=fileRanges, 
                fileExperiment=fileExperiment, yieldSize=yieldSize,
                .views_on_file=.views_on_file, ...)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### scanFa() and countFa() methods.
###

setMethod(scanFa, c("FaGFileViews", "missing"),
          function(file, param, ...)
{
    param <- .GFileViews_which(file, fileRanges(file), missing(param))
    fun <- function(fileViews, param, ...)
        scanFa(file=filePaths(fileViews), param=param, ...)
    .GFileViews_delegate("scanFa", file, fun, ..., param=param)
})

setMethod(countFa, "FaGFileViews",
          function(file, ...)
{
    fun <- function(fileViews, ..., verbose)
        countFa(file=filePaths(fileViews), ...)
    .GFileViews_delegate("countFa", file, fun, ...)
})

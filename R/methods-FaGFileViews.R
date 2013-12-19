### =========================================================================
### FaFileViews methods
### =========================================================================

setMethod(FaFileViews, c(fileRanges="GRanges"), 
          function(filePaths=character(0),
                   fileIndices=filePaths,
                   fileSamples=DataFrame(row.names=
                     make.unique(basename(filePaths))),
                   fileRanges,
                   fileExperiment=list(),
                   yieldSize=NA_integer_,
                   .views_on_file=new.env(parent=emptyenv()), ...)
{
    new("FaFileViews", ..., filePaths=filePaths, fileIndices=fileIndices,
        fileSamples=fileSamples, fileRanges=fileRanges,
        fileExperiment=fileExperiment, yieldSize=yieldSize,
        .views_on_file=.views_on_file)
})

setMethod(FaFileViews, c(fileRanges="missing"), 
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
    FaFileViews(filePaths=filePaths, fileIndices=fileIndices, 
                fileSamples=fileSamples, fileRanges=fileRanges, 
                fileExperiment=fileExperiment, yieldSize=yieldSize,
                .views_on_file=.views_on_file, ...)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### scanFa() and countFa() methods.
###

setMethod(scanFa, c("FaFileViews", "missing"),
          function(file, param, ...)
{
    param <- .FileViews_which(file, fileRanges(file), missing(param))
    fun <- function(fileViews, param, ...)
        scanFa(file=filePaths(fileViews), param=param, ...)
    .FileViews_delegate("scanFa", file, fun, ..., param=param)
})

setMethod(countFa, "FaFileViews",
          function(file, ...)
{
    fun <- function(fileViews, ..., verbose)
        countFa(file=filePaths(fileViews), ...)
    .FileViews_delegate("countFa", file, fun, ...)
})

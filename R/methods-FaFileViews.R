### =========================================================================
### FaFileViews methods
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructors. 
###

setMethod(FaFileViews, "FaFileList", 
          function(filePaths,
                   fileIndices=filePaths,
                   fileSamples=DataFrame(row.names=
                     make.unique(basename(filePaths))),
                   fileRanges=GRanges(),
                   fileExperiment=list(),
                   byFile=TRUE,
                   yieldSize=NA_integer_,
                   .views_on_file=new.env(parent=emptyenv()), ...)
{
    new("FaFileViews", ...,
        fileList=.FileList(
            listData=(list(path=path(filePaths), index=index(filePaths)))),
        fileSamples=fileSamples, fileRanges=fileRanges,
        fileExperiment=fileExperiment, byFile=byFile,
        yieldSize=yieldSize, .views_on_file=.views_on_file)
})

setMethod(FaFileViews, "character", 
          function(filePaths,
                   fileIndices=filePaths,
                   fileSamples=DataFrame(row.names=
                     make.unique(basename(filePaths))),
                   fileRanges=GRanges(),
                   fileExperiment=list(),
                   byFile=TRUE,
                   yieldSize=NA_integer_,
                   .views_on_file=new.env(parent=emptyenv()), ...)
{
    new("FaFileViews", ...,
        fileList=.FileList(
            listData=(list(path=filePaths, index=filePaths))),
        fileSamples=fileSamples, fileRanges=fileRanges, 
        fileExperiment=fileExperiment, byFile=byFile, 
        yieldSize=yieldSize, .views_on_file=.views_on_file)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### scanFa() and countFa() methods.
###

setMethod(scanFa, c("FaFileViews", "missing"),
          function(file, param, ...)
{
    param <- fileRanges(file)
    fun <- function(file, param, ...)
        scanFa(file=file[1], param=param, ...)
    .delegate("scanFa", fun, file, ..., param=param)
})

setMethod(countFa, "FaFileViews",
          function(file, ...)
{
    fun <- function(file, ..., verbose)
        countFa(file=file[1], ...)
    .delegate("countFa", fun, file, ...)
})

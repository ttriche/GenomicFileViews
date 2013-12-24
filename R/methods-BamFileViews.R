### =========================================================================
### BamFileViews methods
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity. 
###

setMethod(.validity, "BamFileViews", 
    function(object) {
        msg <- NULL
        if (length(fileIndices(object)) != length(filePaths(object)))
            msg <- c(msg, 
                     "length(fileIndices(object)) != length(filePaths(object))")
        if (is.null(msg)) TRUE else msg
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructors. 
###

setMethod(BamFileViews, "BamFileList", 
          function(filePaths,
                   fileIndices=character(),
                   fileSamples=DataFrame(row.names=
                     make.unique(basename(path(filePaths)))),
                   fileRanges=GRanges(),
                   fileExperiment=list(),
                   byFile=TRUE,
                   yieldSize=NA_integer_,
                   .views_on_file=new.env(parent=emptyenv()), ...)
{
    new("BamFileViews", ..., 
        fileList=.FileList(
            listData=(list(path=path(filePaths), index=index(filePaths)))),
        fileSamples=fileSamples, fileRanges=fileRanges,
        fileExperiment=fileExperiment, byFile=byFile,
        yieldSize=yieldSize, .views_on_file=.views_on_file)
})

setMethod(BamFileViews, "character", 
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
    new("BamFileViews", ..., 
        fileList=.FileList(
            listData=(list(path=filePaths, index=filePaths))),
        fileSamples=fileSamples, fileRanges=fileRanges,
        fileExperiment=fileExperiment, byFile=byFile,
        yieldSize=yieldSize, .views_on_file=.views_on_file)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### scanBam() and countBam() methods.
###

setMethod(scanBam, "BamFileViews",
          function(file, index=file, ...,
                   param=ScanBamParam(what=scanBamWhat()))
{
    if (length(param))
        if (!identical(fileRanges(file), bamWhich(param)))
            warning("'fileRanges(file)' and 'bamWhich(param)' differ; ",
                    "using fileRanges(file)")
    bamWhich(param) <- fileRanges(file) 
    fun <- function(file, ..., verbose)
        scanBam(file=file[1], index=file[2], ...)
    .delegate("scanBam", fun, file, ..., param=param)
})

setMethod(countBam, "BamFileViews",
          function(file, index=file, ..., param=ScanBamParam())
{
    fun <- function(file, ..., verbose)
        countBam(file=file[1], index=file[2], ...)
    .delegate("countBam", fun, file, ..., param=param)
})


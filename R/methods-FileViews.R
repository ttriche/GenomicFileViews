### =========================================================================
### FileViews (VIRTUAL) 
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity. 
###

setMethod(.validity, "FileViews", 
    function(object) 
{
        msg <- NULL
        if (length(filePaths(object)) != nrow(fileSamples(object)))
            msg <- c(msg,
            "length(filePaths(object)) != nrow(fileSamples(object))")
        if (!byFile(object) && !length(fileRanges(object)))
            msg <- c(msg,
            "if byFile(object)=FALSE fileRanges(object) must be present")
        if (is.null(msg)) TRUE else msg
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and Setters.
###

fileList <- function(x, ...) 
    slot(x, "fileList")

`fileList<-` <-
    function(x, ..., value) initialize(x, fileList=value)

filePaths <- function(x, ...) 
    setNames(slot(x, "fileList")$path, names(x))

`filePaths<-` <-
    function(x, ..., value) 
{ 
    slot(x, "fileList")$path <- value
    validObject(x)
    x
}

fileIndices <- function(x, ...) 
    setNames(slot(x, "fileList")$index, names(x))

`fileIndices<-` <-
    function(x, ..., value) 
{ 
    slot(x, "fileList")$index <- value
    validObject(x)
    x
}

`fileDirname<-` <-
    function(x, ..., value)
        initialize(x,
            fileList=.FileList(listData=(list(
                path=file.path(value, basename(filePaths(x))), 
                index=file.path(value, basename(fileIndices(x)))))))

fileSamples <-
    function(x, ...) slot(x, "fileSamples")

`fileSamples<-` <-
    function(x, ..., value) initialize(x, fileSamples=value)
 
fileRanges <-
    function(x, ...) slot(x, "fileRanges")

`fileRanges<-` <-
    function(x, ..., value) initialize(x, fileRanges=value)

fileExperiment <-
    function(x, ...) slot(x, "fileExperiment")

`fileExperiment<-` <-
    function(x, ..., value) initialize(x, fileExperiment=value)

byFile <-
    function(x, ...) slot(x, "byFile")

`byFile<-` <-
    function(x, ..., value) initialize(x, byFile=value)

setMethod(yieldSize, "FileViews",
    function(object, ...) slot(object, "yieldSize"))

setReplaceMethod("yieldSize", "FileViews",
    function(object, ..., value)
{
    if (1L != length(value))
        stop("'value' must be length 1")
    slot(object, "yieldSize") <- as.integer(value)
    object
})

setMethod(names, "FileViews", 
    function(x) rownames(fileSamples(x))
)

setReplaceMethod("names", "FileViews", 
    function(x, value) 
{ 
    rownames(fileSamples(x)) <- value
    x
})

setMethod(dim, "FileViews", 
    function(x) c(length(fileRanges(x)), length(filePaths(x)))
)

setMethod(dimnames, "FileViews", 
    function(x) 
        list(names(fileRanges(x)), rownames(fileSamples(x)))
)

setReplaceMethod("dimnames", "FileViews", 
    function(x, value) 
{
        names(fileRanges(x)) <- value[[1]]
        rownames(fileSamples(x)) <- value[[2]]
        x
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[", c("FileViews", "ANY", "missing"),
    function(x, i, j, ..., drop=TRUE)
        initialize(x, fileRanges=fileRanges(x)[i,])
)

setMethod("[", c("FileViews", "missing", "ANY"),
    function(x, i, j, ..., drop=TRUE)
{
    if (is.character(j))
        j <- match(j, colnames(x))
    if (any(is.na(j)))
        stop("subscript 'j' out of bounds")
    initialize(x, 
               fileList=.FileList(listData=list(path=filePaths(x)[j],
                                    index=fileIndices(x)[j])),
               fileSamples=fileSamples(x)[j,,drop=FALSE])
})

setMethod("[", c("FileViews", "ANY", "ANY"),
    function(x, i, j, ..., drop=TRUE)
{
    if (is.character(i))
        j <- match(i, rownames(x))
    if (is.character(j))
        j <- match(j, colnames(x))
    if (any(is.na(i)))
        stop("subscript 'i' out of bounds")
    if (any(is.na(j)))
        stop("subscript 'j' out of bounds")
    initialize(x, 
        fileRanges=fileRanges(x)[i,],
        fileList=.FileList(listData=list(path=filePaths(x)[j],
                             index=fileIndices(x)[j])),
        fileSamples=fileSamples(x)[j,,drop=FALSE])
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Delegate.
###

.delegate <- function(what, fun, fileViews, ...)
{
    if (byFile(fileViews))
        delegateByFile(what, fun, fileViews, ...)
    else
        delegateByRange(what, fun, fileViews, ...)
}

delegateByFile <-
    function(what, fun, fileViews, ...)
{
    ## List of path/index pairs
    flist <- fileList(fileViews)
    index <- sapply(flist, function(i) length(i) > 0L)
    pairs <- lapply(seq_along(filePaths(fileViews)), 
                    function(i) sapply(flist[index], "[[", i)) 

    result <- bplapply(pairs, fun, ...)
    if (length(result) != length(filePaths(fileViews))) {
        stop(sprintf("'%s' failed on '%s'", what,
                     paste(setdiff(rownames(fileSamples(fileViews)), 
                           names(result)), collapse="' '")))
    }
    names(result) <- rownames(fileSamples(fileViews))
    do.call(new, 
        list("SimpleList", listData=result,
             metadata=list(samples=fileSamples(fileViews),
                           experiment=fileExperiment(fileViews))))
}

delegateByRange <-
    function(what, fun, fileViews, ...)
{
  ## I wrote it here one by one, however delegateByRange should allow n ranges at a time
  ## this requires an argument or slot from somewhere above specifying this number n
  
  rangeIndex <- seq_along(fileRanges(fileViews))
  result <- bplapply(rangeIndex, function(ri) {

    ## Subset by ranges
    subsetFileViews <- fileViews[ri,]
    
    ## List of path/index pairs
    flist <- fileList(subsetFileViews)
    index <- sapply(flist, function(i) length(i) > 0L)
    pairs <- lapply(seq_along(filePaths(subsetFileViews)), 
                    function(i) sapply(flist[index], "[[", i))   
    
    rangeResult <- lapply(pairs, fun, ...)
    if (length(rangeResult) != length(filePaths(subsetFileViews))) {
        stop(sprintf("'%s' failed on '%s'", what,
                     paste(setdiff(rownames(fileSamples(subsetFileViews)), 
                           names(rangeResult)), collapse="' '")))
    }
    names(rangeResult) <- rownames(fileSamples(subsetFileViews))
    rangeResult
  })
  do.call(new, list("SimpleList", listData=result,
                    metadata=list(samples=fileSamples(fileViews),
                      experiment=fileExperiment(fileViews))))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show.
###

setMethod(show, "FileViews", 
    function(object) 
{
    cat(class(object), "dim:",
        paste(dim(object), c("ranges", "samples"), collapse=" x "),
        "\n")
    cat("names:", BiocGenerics:::selectSome(names(object)), "\n")
    cat("detail: use filePaths(), fileSamples(), fileRanges(), ...",
        "\n")
})


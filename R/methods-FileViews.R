### =========================================================================
### FileViews (VIRTUAL) 
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity. 
###

setMethod(.validity, "FileViews", 
    function(object) {
        msg <- NULL
        if (length(fileIndices(object)) != length(filePaths(object)))
            msg <- c(msg,
                     "length(fileIndices(object)) != length(filePaths(object))")
        if (length(filePaths(object)) != nrow(fileSamples(object)))
            msg <- c(msg,
                     "length(filePaths(object)) != nrow(fileSamples(object))")
        if (is.null(msg)) TRUE else msg
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and Setters.
###

filePaths <-
    function(x) setNames(slot(x, "filePaths"), names(x))

fileIndices <-
    function(x) setNames(slot(x, "fileIndices"), names(x))

`fileDirname<-` <-
    function(x, ..., value) {
        initialize(x,
                   filePaths=file.path(value, basename(filePaths(x))),
                   fileIndices=file.path(value, basename(fileIndices(x))))
    }

fileSamples <-
    function(x) slot(x, "fileSamples")

`fileSamples<-` <-
    function(x, value) initialize(x, fileSamples=value)
 
fileRanges <-
    function(x) slot(x, "fileRanges")

`fileRanges<-` <-
    function(x, value) initialize(x, fileRanges=value)

fileExperiment <-
    function(x) slot(x, "fileExperiment")

setMethod(yieldSize, "FileViews",
    function(object, ...) slot(object, "yieldSize"))

setReplaceMethod("yieldSize", "FileViews",
    function(object, ..., value) {
        slot(object, "yieldSize") <- value
        object 
    }
)

setMethod(dim, "FileViews", 
    function(x) c(length(fileRanges(x)), length(filePaths(x)))
)

setMethod(names, "FileViews", 
    function(x) rownames(fileSamples(x))
)

setReplaceMethod("names", "FileViews", 
    function(x, value) { 
        rownames(fileSamples(x)) <- value
        x
    }
)

setMethod(dimnames, "FileViews", 
    function(x) 
        list(names(fileRanges(x)), rownames(fileSamples(x)))
)

setReplaceMethod("dimnames", "FileViews", 
    function(x, value) {
        names(fileRanges(x)) <- value[[1]]
        rownames(fileSamples(x)) <- value[[2]]
        x
    }
)

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
        initialize(x, filePaths=filePaths(x)[j],
                   fileIndices=fileIndices(x)[j],
                   fileSamples=fileSamples(x)[j,,drop=FALSE])
    }
)

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
        initialize(x, fileRanges=fileRanges(x)[i,],
                   filePaths=filePaths(x)[j],
                   fileIndices=fileIndices(x)[j],
                   fileSamples=fileSamples(x)[j,,drop=FALSE])
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Split and Delegate.
###

.FileViews_which <- function(file, param, missing)
{
    grange <- fileRanges(file)
    which <- split(ranges(grange), seqnames(grange))
    if (is(file, "BamFileViews")) { 
        if (!missing && !identical(which, bamWhich(param)))
            warning("'fileRanges(file)' and 'bamWhich(param)' differ; ",
                    "using fileRanges(file)")
    }
    which
}

.FileViews_delegate <-
    function(what, fileViews, fun, ...)
{
    result <- bplapply(fileViews, fun, ...)
    if (length(result) != ncol(fileViews)) {
        stop(sprintf("'%s' failed on '%s'", what,
                     paste(setdiff(names(fileViews), names(result)),
                                   collapse="' '")))
    }
    names(result) <- names(fileViews)
    do.call(new, list("SimpleList", listData=result,
                      elementMetadata=fileSamples(fileViews)))
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
    }
)


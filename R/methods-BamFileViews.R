### =========================================================================
### BamFileViews methods
### =========================================================================

setMethod(BamFileViews, c(fileRanges="GRanges"), 
          function(filePaths=character(0),
                   fileIndices=filePaths,
                   fileSamples=DataFrame(row.names=
                     make.unique(basename(filePaths))),
                   fileRanges,
                   fileExperiment=list(),
                   yieldSize=NA_integer_,
                   .views_on_file=new.env(parent=emptyenv()), ...)
{
    new("BamFileViews", ..., filePaths=filePaths, fileIndices=fileIndices,
        fileSamples=fileSamples, fileRanges=fileRanges,
        fileExperiment=fileExperiment, yieldSize=yieldSize,
        .views_on_file=.views_on_file)
})

setMethod(BamFileViews, c(fileRanges="missing"), 
          function(filePaths=character(0),
                   fileIndices=filePaths,
                   fileSamples=DataFrame(row.names=
                     make.unique(basename(filePaths))),
                   fileRanges,
                   fileExperiment=list(),
                   yieldSize=NA_integer_,
                   .views_on_file=new.env(parent=emptyenv()), 
                   ..., auto.range=FALSE)
{
    if (length(filePaths) != 0L && auto.range)
    {
        ## Guess ranges from BAM file headers
        pathsOk <- sapply(filePaths, function(fl) {
            file.exists(fl) && !file.info(fl)$isdir
        })
        if (all(pathsOk)) {
            rngs <- lapply(scanBamHeader(filePaths), "[[", "targets")
            nms <- unique(unlist(lapply(rngs, names), use.names=FALSE))
            ends <- sapply(nms, function(nm, rngs) {
                idx <- sapply(rngs, function(rng, nm) {
                    nm %in% names(rng)
                }, nm)
                if (sum(idx) > 0)
                    max(sapply(rngs[idx], "[[", nm))
                else
                    stop("Rsamtools internal: could not determine fileRanges")
            }, rngs)
            fileRanges <- GRanges(names(ends), IRanges(1L, ends))
        } else {
            warning("some files do not exist; fileRanges not defined")
            fileRanges <- GRanges()
        }
    } else {
        fileRanges <- GRanges()
    }
    BamFileViews(filePaths=filePaths, fileIndices=fileIndices, 
                 fileSamples=fileSamples, fileRanges=fileRanges, 
                 fileExperiment=fileExperiment, yieldSize=yieldSize,
                 .views_on_file=.views_on_file, ...)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### scanBam() and countBam() methods.
###

setMethod(scanBam, "BamFileViews",
          function(file, index=file, ...,
                   param=ScanBamParam(what=scanBamWhat()))
{
    if (!missing(index))
        warning("using fileIndices(file) for 'index'")
    bamWhich(param) <- .FileViews_which(file, param, missing(param))
    fun <- function(fileViews, ..., verbose)
        scanBam(file=filePaths(fileViews),
                index=fileIndices(fileViews), ...)
    .FileViews_delegate("scanBam", file, fun, ..., param=param)
})

setMethod(countBam, "BamFileViews",
          function(file, index=file, ..., param=ScanBamParam())
{
    if (!missing(index))
        warning("using fileIndices(file) for 'index'")
    bamWhich(param) <- .FileViews_which(file, param, missing(param))
    fun <- function(fileViews, ..., verbose)
        countBam(file=filePaths(fileViews),
                 index=fileIndices(fileViews), ...)
    .FileViews_delegate("countBam", file, fun, ..., param=param)
})


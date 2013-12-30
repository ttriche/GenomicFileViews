### =========================================================================
### BigWigFileViews methods
### =========================================================================

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity. 
###

setMethod(.validity, "BigWigFileViews", 
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

setMethod(BigWigFileViews, "character", 
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
    new("BigWigFileViews", ..., 
        fileList=.FileList(
            listData=(list(path=filePaths, index=filePaths))),
        fileSamples=fileSamples, fileRanges=fileRanges,
        fileExperiment=fileExperiment, byFile=byFile,
        yieldSize=yieldSize, .views_on_file=.views_on_file)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### coverage() and summary() methods.
###

setMethod(coverage, "BigWigFileViews",
          function(x, ...)
          {
            fun <- function(file, ..., which, verbose) {
              # import.bw gives an RleList which an Rle for each sequence,
              # then we subset by the GRanges 'which', which gives an
              # Rle for each GRange in fileRanges(x)
              # BUT: these have lost their GRange info, they only
              # have the name of the sequence
              import.bw(BigWigFile(file[1]), asRle=TRUE, which=which)[which]
            }
            .delegate("coverage", fun, x, ..., which=fileRanges(x))
          })

# TODO: this is just testing code, take the first range only
setMethod(summary, "BigWigFileViews",
          function(object, ...)
          {
            fun <- function(file, ..., verbose) {
              z <- try(summary(BigWigFile(file[1]), ...),
                       silent=TRUE)
              if (inherits(z,"try-error")) {
                return(0) 
              } else {
                # TODO: this is just testing code, take the first range only
                return(as.numeric(z[[1]]))
              }
            }
            .delegate("summary", fun, object, ..., ranges=fileRanges(object))
          })



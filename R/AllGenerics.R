setGeneric("BamFileViews",
           function(filePaths,
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges=GRanges(),
                    fileExperiment=list(), 
                    byFile=TRUE,
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("BamFileViews"),
           signature="filePaths")

setGeneric("FaFileViews",
           function(filePaths,
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges=GRanges(),
                    fileExperiment=list(),
                    byFile=TRUE,
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("FaFileViews"),
           signature="filePaths")

setGeneric("TabixFileViews",
           function(filePaths,
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges=GRanges(),
                    fileExperiment=list(),
                    byFile=TRUE,
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("TabixFileViews"),
           signature="filePaths")

setGeneric("VCFFileViews",
           function(filePaths,
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges=GRanges(),
                    fileExperiment=list(),
                    byFile=TRUE,
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("VCFFileViews"),
           signature="filePaths")

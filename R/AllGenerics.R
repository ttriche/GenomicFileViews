setGeneric("BamFileViews",
           function(filePaths=character(0),
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges,
                    fileExperiment=list(), 
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("BamFileViews"),
           signature="fileRanges")

setGeneric("FaFileViews",
           function(filePaths=character(0),
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges,
                    fileExperiment=list(),
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("FaFileViews"),
           signature="fileRanges")

setGeneric("TabixFileViews",
           function(filePaths=character(0),
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges,
                    fileExperiment=list(),
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("TabixFileViews"),
           signature="fileRanges")

setGeneric("VCFFileViews",
           function(filePaths=character(0),
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges,
                    fileExperiment=list(),
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("VCFFileViews"),
           signature="fileRanges")

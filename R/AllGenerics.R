setGeneric("BamGFileViews",
           function(filePaths=character(0),
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges,
                    fileExperiment=list(), 
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("BamGFileViews"),
           signature="fileRanges")

setGeneric("FaGFileViews",
           function(filePaths=character(0),
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges,
                    fileExperiment=list(),
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("FaGFileViews"),
           signature="fileRanges")

setGeneric("TabixGFileViews",
           function(filePaths=character(0),
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges,
                    fileExperiment=list(),
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("TabixGFileViews"),
           signature="fileRanges")

setGeneric("VCFGFileViews",
           function(filePaths=character(0),
                    fileIndices=filePaths,
                    fileSamples=DataFrame(row.names=
                      make.unique(basename(filePaths))),
                    fileRanges,
                    fileExperiment=list(),
                    yieldSize="NA_integer_",
                   .views_on_file="environment", ...)
           standardGeneric("VCFGFileViews"),
           signature="fileRanges")

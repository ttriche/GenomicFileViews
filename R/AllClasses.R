### =========================================================================
### All classes 
### =========================================================================

setGeneric(".validity", function(object) standardGeneric(".validity"))

setClass("GFileViews",
    representation("VIRTUAL",
        filePaths="character",
        fileIndices="character",
        fileSamples="DataFrame",
        fileRanges="GRanges",
        fileExperiment="list",
        yieldSize="integer",
        .views_on_file="environment"),
    prototype(
        yieldSize=NA_integer_),
    validity=.validity)

setClass("BamGFileViews", contains="GFileViews")

setClass("FaGFileViews", contains="GFileViews")

setClass("TabixGFileViews", contains="GFileViews")

setClass("VcfGFileViews", contains="GFileViews")

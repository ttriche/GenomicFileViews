### =========================================================================
### All classes 
### =========================================================================

setGeneric(".validity", function(object) standardGeneric(".validity"))

setClass("FileViews",
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

setClass("BamFileViews", contains="FileViews")

setClass("FaFileViews", contains="FileViews")

setClass("TabixFileViews", contains="FileViews")

setClass("VcfFileViews", contains="FileViews")

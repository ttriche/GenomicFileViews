### =========================================================================
### All classes 
### =========================================================================

setGeneric(".validity", function(object) standardGeneric(".validity"))

setClass("FileViews",
    representation("VIRTUAL",
        fileList="List",
        fileSamples="DataFrame",
        fileRanges="GRanges",
        fileExperiment="list",
        byFile="logical",
        yieldSize="integer",
        .views_on_file="environment"),
    prototype(
        delegateByFile=TRUE,
        yieldSize=NA_integer_),
    validity=.validity)

setClass("BamFileViews", contains="FileViews")

setClass("FaFileViews", contains="FileViews")

setClass("TabixFileViews", contains="FileViews")

setClass("VcfFileViews", contains="FileViews")

.FileList <- setClass(".FileList", contains="SimpleList")

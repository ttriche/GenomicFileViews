\name{BamGFileViews}
\Rdversion{1.1}
\docType{class}
\alias{BamGFileViews-class}
\alias{BamGFileViews}
\alias{BamGFileViews,missing-method}
\alias{BamGFileViews,GRanges-method}
% accessors
% methods
\alias{scanBam,BamGFileViews-method}
\alias{countBam,BamGFileViews-method}


\title{Views into a set of BAM files}
\description{

  Use \code{BamGFileViews()} to reference a set of disk-based BAM files to be
  processed (e.g., queried using \code{\link{scanBam}}) as a single
  \sQuote{experiment}.

}

\section{Usage:}{

## Constructor
BamGFileViews(filePaths=character(0),
     fileIndices=filePaths,
     fileSamples=DataFrame(row.names=make.unique(basename(filePaths))),
     fileRanges, fileExperiment = list(), yieldSize = NA_integer_,
     .views_on_file = new.env(parent=emptyenv()), ...)

## Accessors
filePaths(x)
fileSamples(x)
fileSamples(x) <- value
fileRanges(x)
fileRanges(x) <- value
fileExperiment(x)
yieldSize(x)
names(x)
names(x) <- value
dimnames(x) 
dimnames(x) <- value
fileDirname(x, ...) <- value
length(x)

## Input
scanBam(file, index = file, ..., 
        param = ScanBamParam(what=scanBamWhat()))
countBam(file, index = file, ..., param = ScanBamParam())

## Show
show(object)
}

\arguments{

  \item{filePaths}{A character() vector of BAM path names.}

  \item{fileIndices}{A character() vector of BAM index file path names,
    \emph{without} the \sQuote{.bai} extension.}

  \item{fileSamples}{A \code{\linkS4class{DataFrame}} instance with as
	many rows as \code{length(filePaths)}, containing sample information
	associated with each path.}

  \item{fileRanges}{A \code{\linkS4class{GRanges}}, or missing instance 
        with ranges defined on the spaces of the BAM files. Ranges are 
        \emph{not} validated against the BAM files.}

  \item{fileExperiment}{A list() containing additional information about
        the experiment.}

  \item{yieldSize}{An integer specifying number of records to process}

  \item{.views_on_file}{An enviornment. Currently under development}

  \item{auto.range}{If \code{TRUE} and all \code{filePaths} exist,
    populate the ranges with the union of ranges returned in the
    \code{target} element of \code{scanBamHeader}.}

  \item{...}{Additional arguments.}

  \item{x}{An instance of \code{BamGFileViews}.}

  \item{object}{An instance of \code{BamGFileViews}.}

  \item{value}{An object of appropriate type to replace content.}

  \item{i}{During subsetting, a logical or numeric index into
	\code{fileRanges}.}

  \item{j}{During subsetting, a logical or numeric index into
	\code{fileSamples} and \code{filePaths}.}

  \item{drop}{A logical(1), \emph{ignored} by all \code{BamGFileViews}
	subsetting methods.}

  %% input

  \item{file}{An instance of \code{BamGFileViews}.}

  \item{index}{A character vector of indices, corresponding to the
     \code{filePaths(file)}. }

  \item{param}{An optional \code{\linkS4class{ScanBamParam}} instance to
     further influence scanning or counting.}
}

\section{Objects from the Class}{

  Objects are created by calls of the form \code{BamGFileViews()}.

}
\section{Slots}{
  \describe{

  \item{filePaths}{A character() vector of BAM path names.}

  \item{fileIndices}{A character() vector of BAM index path names.}

  \item{fileSamples}{A \code{\linkS4class{DataFrame}} instance with as
	many rows as \code{length(filePaths)}, containing sample information
	associated with each path.}

  \item{fileRanges}{A \code{\linkS4class{GRanges}} instance with
	ranges defined on the spaces of the BAM files. Ranges are \emph{not}
	validated against the BAM files.}

  \item{fileExperiment}{A list() containing additional information about
	the experiment.}

  \item{yieldSize}{An integer specifying the data chunk size.}

  \item{.views_on_file}{An enviornment. Currently under construction.}

  }
}

\section{Functions and methods}{

  See 'Usage' for details on invocation.

  Constructor:
  \describe{

	\item{BamGFileViews:}{Returns a \code{BamGFileViews} object.}

  }

  Accessors:
  \describe{

	\item{filePaths}{Returns a character() vector of BAM path names.}

	\item{fileIndices}{Returns a character() vector of BAM index path
      names.}

	\item{fileSamples}{Returns a \code{\linkS4class{DataFrame}} instance
	  with as many rows as \code{length(filePaths)}, containing sample
	  information associated with each path.}

	\item{fileSamples<-}{Assign a \code{\linkS4class{DataFrame}} instance
	  with as many rows as \code{length(filePaths)}, containing sample
	  information associated with each path.}

	\item{fileRanges}{Returns a \code{\linkS4class{GRanges}} instance
	  with ranges defined on the spaces of the BAM files. Ranges are
	  \emph{not} validated against the BAM files.}

	\item{fileRanges<-}{Assign a \code{\linkS4class{GRanges}} instance
	  with ranges defined on the spaces of the BAM files. Ranges are
	  \emph{not} validated against the BAM files.}

	\item{fileExperiment}{Returns a list() containing additional
	  information about the experiment.}

    \item{names}{Return the column names of the \code{BamGFileViews}
      instance; same as \code{names(fileSamples(x))}.}

    \item{names<-}{Assign the column names of the \code{BamGFileViews}
      instance.}

    \item{dimnames}{Return the row and column names of the
      \code{BamGFileViews} instance.}

    \item{dimnames<-}{Assign the row and column names of the
      \code{BamGFileViews} instance.}

  }

  Methods:
  \describe{

    \item{"["}{Subset the object by \code{fileRanges} or \code{fileSamples}.}

    \item{scanBam}{Visit each path in \code{filePaths(file)}, returning
      the result of \code{scanBam} applied to the specified
      path. \code{fileRanges(file)} takes precedence over
      \code{bamWhich(param)}.}

    \item{countBam}{Visit each path in \code{filePaths(file)}, returning
      the result of \code{countBam} applied to the specified
      path. \code{fileRanges(file)} takes precedence over
      \code{bamWhich(param)}.}

    \item{length}{Display the length of the object (number of files).}

    \item{show}{Compactly display the object.}

  }

}
\author{Martin Morgan, Valerie Obenchain}

\examples{
  ## ---------------------------------------------------------------------
  ## A. BamGFileView Objects
  ## ---------------------------------------------------------------------

  fl <- system.file("extdata", "ex1.bam", package="Rsamtools",
                    mustWork=TRUE)
  rngs <- GRanges(seqnames = Rle(c("chr1", "chr2"), c(9, 9)),
                  ranges = c(IRanges(seq(10000, 90000, 10000), width=500),
                             IRanges(seq(100000, 900000, 100000), width=5000)),
                  Count = seq_len(18L))
  v <- BamGFileViews(c(fl, fl), fileRanges=rngs)
  v
  v[1:5,]
  fileRanges(v[c(1:5, 11:15),])
  fileDirname(v) <- getwd()
  v

  ## ---------------------------------------------------------------------
  ## B. countBam and scanBam 
  ## ---------------------------------------------------------------------
  #countBam(v)
  #scanBam(v)
}

\keyword{classes}
\keyword{methods}

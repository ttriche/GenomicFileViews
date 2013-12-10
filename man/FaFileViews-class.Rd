\name{FaFileViews}
\Rdversion{1.1}
\docType{class}
\alias{FaFileViews-class}
\alias{FaFileViews}
\alias{FaFileViews,missing-method}
\alias{FaFileViews,GRanges-method}
% accessors
% methods
\alias{scanFa,FaFileViews,missing-method}
\alias{countFa,FaFileViews-method}


\title{Views into a set of Fasta files}
\description{

  Use \code{FaFileViews()} to reference a set of disk-based Fasta files to be
  processed (e.g., queried using \code{\link{scanFa}}) as a single
  \sQuote{experiment}.

}

\section{Usage:}{

## Constructor
FaFileViews(filePaths=character(0),
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
names(x)
names(x) <- value
dimnames(x) 
dimnames(x) <- value
fileDirname(x, ...) <- value
length(x)

## Input
scanFa(file, param, ...)
countFa(file, ...)

## Show
show(object)
}

\arguments{

  \item{filePaths}{A character() vector of Fasta path names.}

  \item{fileIndices}{A character() vector of Fasta index file path names,
    \emph{without} the \sQuote{.bai} extension.}

  \item{fileSamples}{A \code{\linkS4class{DataFrame}} instance with as
	many rows as \code{length(filePaths)}, containing sample information
	associated with each path.}

  \item{fileRanges}{A \code{\linkS4class{GRanges}}, or missing instance 
        with ranges defined on the spaces of the Fasta files. Ranges are 
        \emph{not} validated against the Fasta files.}

  \item{fileExperiment}{A list() containing additional information about
	the experiment.}
  \item{yieldSize}{An integer specifying number of records to process}

  \item{.views_on_file}{An enviornment. Currently under development}

  \item{param}{Unused option for \code{FaFileViews} object. \code{fileRanges}
        are used to specify ranges to query.}

  \item{...}{Additional arguments.}

  \item{x}{An instance of \code{FaFileViews}.}

  \item{object}{An instance of \code{FaFileViews}.}

  \item{value}{An object of appropriate type to replace content.}

  \item{i}{During subsetting, a logical or numeric index into
	\code{fileRanges}.}

  \item{j}{During subsetting, a logical or numeric index into
	\code{fileSamples} and \code{filePaths}.}

  \item{drop}{A logical(1), \emph{ignored} by all \code{FaFileViews}
	subsetting methods.}

  %% input

  \item{file}{An instance of \code{FaFileViews}.}

  \item{index}{A character vector of indices, corresponding to the
     \code{filePaths(file)}. }
}

\section{Objects from the Class}{

  Objects are created by calls of the form \code{FaFileViews()}.

}
\section{Slots}{
  \describe{

  \item{filePaths}{A character() vector of Fasta path names.}

  \item{fileIndices}{A character() vector of Fasta index path names.}

  \item{fileSamples}{A \code{\linkS4class{DataFrame}} instance with as
	many rows as \code{length(filePaths)}, containing sample information
	associated with each path.}

  \item{fileRanges}{A \code{\linkS4class{GRanges}} instance with
	ranges defined on the spaces of the Fasta files. Ranges are \emph{not}
	validated against the Fasta files.}

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

	\item{FaFileViews:}{Returns a \code{FaFileViews} object.}

  }

  Accessors:
  \describe{

	\item{filePaths}{Returns a character() vector of Fasta path names.}

	\item{fileIndices}{Returns a character() vector of Fasta index path
      names.}

	\item{fileSamples}{Returns a \code{\linkS4class{DataFrame}} instance
	  with as many rows as \code{length(filePaths)}, containing sample
	  information associated with each path.}

	\item{fileSamples<-}{Assign a \code{\linkS4class{DataFrame}} instance
	  with as many rows as \code{length(filePaths)}, containing sample
	  information associated with each path.}

	\item{fileRanges}{Returns a \code{\linkS4class{GRanges}} instance
	  with ranges defined on the spaces of the Fasta files. Ranges are
	  \emph{not} validated against the Fasta files.}

	\item{fileRanges<-}{Assign a \code{\linkS4class{GRanges}} instance
	  with ranges defined on the spaces of the Fasta files. Ranges are
	  \emph{not} validated against the Fasta files.}

	\item{fileExperiment}{Returns a list() containing additional
	  information about the experiment.}

    \item{names}{Return the column names of the \code{FaFileViews}
      instance; same as \code{names(fileSamples(x))}.}

    \item{names<-}{Assign the column names of the \code{FaFileViews}
      instance.}

    \item{dimnames}{Return the row and column names of the
      \code{FaFileViews} instance.}

    \item{dimnames<-}{Assign the row and column names of the
      \code{FaFileViews} instance.}

  }

  Methods:
  \describe{

    \item{"["}{Subset the object by \code{fileRanges} or \code{fileSamples}.}

    \item{scanFa}{Visit each path in \code{filePaths(file)}, returning
      the result of \code{scanFa} applied to the specified
      path. \code{fileRanges(file)} takes precedence over
      \code{fileWhich(param)}.}

    \item{countFa}{Visit each path in \code{filePaths(file)}, returning
      the result of \code{countFa} applied to the specified
      path.}

    \item{length}{Display the length of the object (number of files).}

    \item{show}{Compactly display the object.}

  }

}
\author{Martin Morgan, Valerie Obenchain}

\examples{
  ## ---------------------------------------------------------------------
  ## A. FaFileView Objects
  ## ---------------------------------------------------------------------
  library(Rsamtools)
  fl <- system.file("extdata", "ce2dict1.fa", package="Rsamtools",
                    mustWork=TRUE)
  rngs <- scanFaIndex(fl)
  v <- FaFileViews(c(fl, fl), fileRanges=rngs)
  v
  v[1:2,]
  fileRanges(v)
  fileDirname(v) <- getwd()
  v
 
  ## ---------------------------------------------------------------------
  ## B. countFa() and scanFa() 
  ## ---------------------------------------------------------------------
  #countFa(v)
  #dna <- scanFa(v, param=rngs[1:2])
}

\keyword{classes}
\keyword{methods}

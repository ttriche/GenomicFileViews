\name{FileViews}
\Rdversion{1.1}
\docType{class}
\alias{FileViews-class}
\alias{FileViews}
% accessors
\alias{filePaths}
\alias{fileIndices}
\alias{fileSamples}
\alias{fileRanges}
\alias{fileExperiment}
\alias{fileDirname<-}
\alias{fileSamples<-}
\alias{fileRanges<-}
% methods
\alias{names,FileViews-method}
\alias{names<-,FileViews-method}
\alias{dim,FileViews-method}
\alias{dimnames,FileViews-method}
\alias{dimnames<-,FileViews,ANY-method}
\alias{[,FileViews,ANY,missing-method}
\alias{[,FileViews,missing,ANY-method}
\alias{[,FileViews,ANY,ANY-method}
\alias{[,FileViews,ANY,missing-method}
\alias{yieldSize,FileViews-method}
\alias{length,FileViews-method}
\alias{show,FileViews-method}


\title{Views into a set of files}
\description{

  \code{FileViews} is a VIRTUAL class used to reference a set of 
  disk-based files to be queried across views (ranges).

}

\usage{

## Accessors
filePaths(x)
fileIndices(x)
fileSamples(x)
fileSamples(x) <- value
fileRanges(x)
fileRanges(x) <- value
fileExperiment(x)
\S4method{yieldSize}{FileViews}(object)
fileDirname(x, ...) <- value

\S4method{names}{FileViews}(x)
\S4method{names}{FileViews}(x) <- value
\S4method{dimnames}{FileViews}(x)
\S4method{dimnames}{FileViews,ANY}(x) <- value

## Subset
\S4method{[}{FileViews,ANY,ANY}(x, i, j, \dots, drop=TRUE)
\S4method{[}{FileViews,ANY,missing}(x, i, j, \dots, drop=TRUE)
\S4method{[}{FileViews,missing,ANY}(x, i, j, \dots, drop=TRUE)

## Show
\S4method{show}{FileViews}(object)
}

\arguments{

  \item{filePaths}{A character() vector of file path names.}

  \item{fileIndices}{A character() vector of file index path names.}

  \item{fileSamples}{A \code{\linkS4class{DataFrame}} instance with as
        many rows as \code{length(filePaths)}, containing sample information
        associated with each path.}

  \item{fileRanges}{A \code{\linkS4class{GRanges}},
        \code{\linkS4class{RangedData}} or missing instance with ranges
        defined on the spaces of the files.}

  \item{fileExperiment}{A list() containing additional information about
        the experiment.}

  \item{...}{Additional arguments.}

  \item{x}{An instance of \code{FileViews}.}

  \item{object}{An instance of \code{FileViews}.}

  \item{value}{An object of appropriate type to replace content.}

  \item{i}{During subsetting, a logical or numeric index into
        \code{fileRanges}.}

  \item{j}{During subsetting, a logical or numeric index into
        \code{fileSamples} and \code{filePaths}.}

  \item{drop}{A logical(1), \emph{ignored} by all \code{FileViews}
        subsetting methods.}

}

\section{Objects from the Class}{

  \code{FileViews} is a VIRTUAL class not intended for instantiation by
  the user. The class serves as a parent for the concrete subclasses such 
  as \code{BamFileViews}, \code{FaFileViews}, \code{TabixFileViews} etc.

}
\section{Slots}{
  \describe{

  \item{filePaths}{A character() vector of file path names.}

  \item{fileIndices}{A character() vector of file index path names.}

  \item{fileSamples}{A \code{\linkS4class{DataFrame}} instance with as
        many rows as \code{length(filePaths)}, containing sample information
        associated with each path.}

  \item{fileRanges}{A \code{\linkS4class{GRanges}} instance with
        ranges defined on the spaces of the files.}

  \item{fileExperiment}{A list() containing additional information about
        the experiment.}

  \item{yieldSize}{An integer specifying the data chunk size.}

  \item{.views_on_file}{An enviornment. Currently under construction.}
  }
}

\section{Functions and methods}{

  Accessors:
  \describe{

    \item{filePaths}{Returns a character() vector of file path names.}

    \item{fileIndices}{Returns a character() vector of file index path
      names.}

    \item{fileSamples}{Returns a \code{\linkS4class{DataFrame}} instance
      with as many rows as \code{length(filePaths)}, containing sample
      information associated with each path.}

    \item{fileSamples<-}{Assign a \code{\linkS4class{DataFrame}} instance
      with as many rows as \code{length(filePaths)}, containing sample
      information associated with each path.}

    \item{fileRanges}{Returns a \code{\linkS4class{GRanges}} instance
      with ranges defined on the spaces of the files.}

    \item{fileRanges<-}{Assign a \code{\linkS4class{GRanges}} instance
      with ranges defined on the spaces of the files.}

    \item{fileExperiment}{Returns a list() containing additional
      information about the experiment.}

    \item{names}{Return the column names of the \code{FileViews}
      instance; same as \code{names(fileSamples(x))}.}

    \item{names<-}{Assign the column names of the \code{FileViews}
      instance.}

    \item{dimnames}{Return the row and column names of the
      \code{FileViews} instance.}

    \item{dimnames<-}{Assign the row and column names of the
      \code{FileViews} instance.}

  }

  Methods:
  \describe{

    \item{"["}{Subset the object by \code{fileRanges} or \code{fileSamples}.}

    \item{show}{Compactly display the object.}

    \item{length}{Display the length of the object (number of files).}

  }

}

\author{Martin Morgan <mtmorgan@fhcrc.org> and 
        Valerie Obenchain <vobencha@fhcrc.org>.}

\examples{
  ## See ?BamViews, ?TabixViews, ?FaViews or other subclasses.
}

\keyword{classes}
\keyword{methods}

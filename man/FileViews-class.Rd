\name{FileViews}
\docType{class}

% Class:
\alias{FileViews}
\alias{class:FileViews}
\alias{FileViews-class}

% Accessors:
\alias{fileList}
\alias{fileList<-}
\alias{filePaths}
\alias{filePaths<-}
\alias{fileIndices}
\alias{fileIndices<-}
\alias{fileSamples}
\alias{fileSamples<-}
\alias{fileRanges}
\alias{fileRanges<-}
\alias{fileExperiment}
\alias{fileExperiment<-}
\alias{fileDirname<-}
\alias{byFile}
\alias{byFile<-}

% Methods:
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
\alias{yieldSize<-,FileViews-method}
\alias{show,FileViews-method}

% Other: 
\alias{delegateByFile}
\alias{delegateByRange}

\title{Views into a set of files}

\description{
  \code{FileViews} is a VIRTUAL class used to reference a set of 
  disk-based files to be queried across views (ranges).
}

\section{Objects from the Class}{
  \code{FileViews} is a VIRTUAL class not intended for instantiation by
  the user. The class serves as a parent for the concrete subclasses such 
  as \code{BamFileViews}, \code{FaFileViews}, \code{TabixFileViews} etc.
}

\section{Slots}{

  \describe{
    \item{}{
      \code{fileList}: 
      List of of length >= 2 containing the file path and index names. 
      List names must include \sQuote{path} and \sQuote{index}.
    }
    \item{}{
      \code{fileSamples}:
        A \code{\linkS4class{DataFrame}} instance with as many rows as 
        \code{length(filePaths)}, containing sample information associated 
        with each path.
    }
    \item{}{
      \code{fileRanges}:
      A \code{\linkS4class{GRanges}} instance with ranges defined on the 
      spaces of the files.
    }
    \item{}{
      \code{fileExperiment}:
      A list() containing additional information about the experiment.
    }
    \item{}{
      \code{byFile}:
        A logical specifying if the code should be run in parallel by 
        file or by range.  When TRUE, files are distributed to workers 
        and the code is run on each file. When FALSE, distribution is by
        the ranges in fileRanges(object).

        The value of \code{byFile} dictates use of the \code{delegateByFile} 
        or \code{delegateByRange} functions.
        FIXME: More to be added here about the requirements of the
        \code{fun} argument.
    }
    \item{}{
      \code{yieldSize}:
        An integer specifying the data chunk size.
    }
    \item{}{
      \code{.views_on_file}:
        An environment. Under construction / future use.
    }
  }
}

\section{Accessors}{
  In the code snippets below, \code{x} is a FileViews object.

  \describe{
    item{}{
      \code{fileList(x)}, \code{fileList(x) <- value}:
      Get or set the fileList on \code{x}. \code{value} must be 
      a SimpleList of length >= 2. List names must include \sQuote{path}
      and \sQuote{index}.
    }
    \item{}{
      \code{filePaths}, \code{filePaths(x) <- value}:
      Get or set the filePaths on \code{x}. \code{value} must be
      a character() vector.
    }
    \item{}{
      \code{fileIndices}, \code{fileIndices(x) <- value}:
      Get or set the fileIndices on \code{x}. \code{value} must be
      a character() vector.
    }
    \item{}{
      \code{fileSamples}, \code{fileSamples(x) <- value}:
      Get or set the fileSamples on \code{x}. \code{value} must be
      a \code{\linkS4class{DataFrame}} instance with as many rows as 
      \code{length(filePaths)}, containing sample
      information associated with each path.
    }
    \item{}{
      \code{fileRanges}, \code{fileRanges(x) <- value}:
      Get or set the fileSamples on \code{x}. \code{value} must be
      a \code{\linkS4class{GRanges}} instance.
    }
    \item{}{
      \code{fileExperiment}, \code{fileExperiment(x) <- value}:
      Get or set the fileExperiment on \code{x}. \code{value} must be
      a list().
    }
    \item{}{
      \code{byFile}, \code{byFile(x) <- value}:
      Get or set the byFile value on \code{x}. \code{value} must be
      a logical.
    }
    \item{}{
      \code{yieldSize}, \code{yieldSize(x) <- value}:
      Get or set the yieldSize on \code{x}. \code{value} must be
      an integer.
    }
    \item{}{
      \code{names}, \code{names(x) <- value}:
      Get or set the names on \code{x}. These are the column
      names of the \code{FileViews} instance corresponding to
      the filePaths.
    }
    \item{}{
      \code{dimnames}, \code{dimnames(x) <- value}:
      Get or set the row and column names on \code{x}.
    }
  }
}

\section{Other operations}{
  In the code snippets below, \code{x} is a FileViews object.

  \describe{
    \item{}{
      \code{"["}:
      Subset the object by \code{fileRanges} or \code{fileSamples}.
    }
    \item{}{
      \code{show}:
      Compactly display the object.
    }
    \item{}{
      \code{delegateByFile}:
      Parallel computations are distributed by files in \code{filePaths}.
    }
    \item{}{
      \code{delegateByRange}:
      Parallel computations are distributed by ranges in \code{fileRanges}.
    }
  }
}

\seealso{
  \itemize{
    \item \link{BamFileViews-class}.
    \item \link{FaFileViews-class}.
  }
}

\author{Martin Morgan <mtmorgan@fhcrc.org> and 
        Valerie Obenchain <vobencha@fhcrc.org>.}

\examples{
  ## See ?BamFileViews, ?TabixFileViews, ?FaFileViews.
}

\keyword{classes}
\keyword{methods}

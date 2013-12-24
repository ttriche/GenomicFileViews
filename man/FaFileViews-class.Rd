\name{FaFileViews}
\docType{class}

% Class:
\alias{FaFileViews-class}
\alias{FaFileViews}
\alias{FaFileViews,character-method}
\alias{FaFileViews,FaFileList-method}

% Methods:
\alias{scanBam,FaFileViews-method}
\alias{countBam,FaFileViews-method}

\title{Views into a set of BAM files}

\description{
  Use \code{FaFileViews()} to reference a set of disk-based BAM files to be
  processed (e.g., queried using \code{\link{scanBam}}) as a single
  \sQuote{experiment}.
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{FaFileViews(filePaths, fileIndices=filePaths,
        fileSamples=DataFrame(row.names=make.unique(basename(filePaths))),
        fileRanges, fileExperiment = list(), byFile = TRUE,
        yieldSize = NA_integer_, 
        .views_on_file = new.env(parent=emptyenv()), ...)}:
      This constructor is a generic function with dispatch on argument
      \code{filePaths}. Methods exist for \link{FaFileList} and character 
      (vector of file names). 
    }
  }
}

\section{Accessors}{
  All accessor-like methods defined for \code{FileViews} objects
  work on \code{FaFileViews} objects. See ?\code{FileViews} for details.
  \itemize{
    \item{fileList(x); fileList(x) <- value}
    \item{filePaths(x); filePaths(x) <- value}
    \item{fileIndices(x); fileIndices(x0 <- value}
    \item{fileSamples(x); fileSamples(x) <- value}
    \item{fileExperiment(x); fileExpermient(x) <- value}
    \item{byFile(x); byFile(x) <- value}
    \item{yieldSize(x); yieldSize(x) <- value}
  }
}

\section{Subsetting}{
  \describe{
    \item{}{
      \code{"["}: Subset the object by \code{fileRanges} or \code{fileSamples}.
    }
  }
}

\section{Other methods}{
  In the code snippets below, \code{file} is a FaFileViews object.

  \describe{
    \item{}{
       \code{scanFa(file, param = ScanBamParam(what=scanBamWhat()))}:
       Visit each path in \code{filePaths(file)}, returning
       the result of \code{scanBam} applied to the specified
       path. \code{fileRanges(file)} takes precedence over
       \code{bamWhich(param)}.
    }
    \item{}{
       \code{countFa(file, ...)}:
       Visit each path in \code{filePaths(file)}, returning
       the result of \code{countBam} applied to the specified
       path. \code{fileRanges(file)} takes precedence over
       \code{bamWhich(param)}.
    }
  }
}

\section{Arguments}{
  \describe{
    \item{}{
       \code{filePaths}:
         A character() vector of BAM path names or a \code{\linkS4class{FaFileList}}.
    }
    \item{}{
      \code{fileIndices}:
        A character() vector of BAM index file path names, \emph{without} the 
        \sQuote{.bai} extension.
    }
    \item{}{
      \code{fileSamples}:
        A \code{\linkS4class{DataFrame}} instance with as many rows as 
        \code{length(filePaths)}, containing sample information associated with 
        each path.
    }
    \item{}{
      \code{fileRanges}:
        A \code{\linkS4class{GRanges}}, or missing instance with ranges defined 
        on the spaces of the BAM files. Ranges are \emph{not} validated against 
        the BAM files.
    }
    \item{}{
      \code{fileExperiment}:
        A list() containing additional information about the experiment.
    }
    \item{}{
      \code{byFile}:
        A logical specifying if parallel execution should be by file or by 
        range.
    }
    \item{}{
      \code{yieldSize}: An integer specifying number of records to process
    }
    \item{}{
        \code{.views_on_file}: An enviornment; currently under development
    }
    \item{}{
      \code{...}: Additional arguments.
    }
    \item{}{
      \code{x}, \code{object}: An instance of \code{FaFileViews}.
    }
    \item{}{
      \code{value}: An object of appropriate type to replace content.
    }
    \item{}{
      \code{i}: 
      During subsetting, a logical or numeric index into \code{fileRanges}.
    }
    \item{}{
      \code{j}: 
      During subsetting, a logical or numeric index into \code{fileSamples} 
      and \code{filePaths}.
    }
    \item{}{
      \code{file}: An instance of \code{FaFileViews}.
    }
    \item{}{
      \code{index}: Not used.
    }
    \item{}{
      \code{param}:
      Unused option for \code{FaFileViews} object. \code{fileRanges}
      are used to specify ranges to query.
    }
  }
}

\section{Slots}{
  \describe{
  Inherited from \code{FileViews} class:
    \itemize{
      \item filePaths
      \item fileIndices
      \item fileSamples
      \item fileRanges
      \item fileExperiment
      \item byFile 
      \item yieldSize
      \item .views_on_file
    }
  }
}

\seealso{
  \itemize{
    \item \link{FileViews-class}.
  }
}

\author{Martin Morgan <mtmorgan@fhcrc.org> and 
        Valerie Obenchain <vobencha@fhcrc.org>.}

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
  ## B. countFa and scanFa 
  ## ---------------------------------------------------------------------
  v <- FaFileViews(c(fl, fl), fileRanges=rngs)
  countFa(v)
  scanFa(v)
}

\keyword{classes}
\keyword{methods}

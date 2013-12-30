\name{BigWigFileViews}
\docType{class}

% Class:
\alias{BigWigFileViews-class}
\alias{BigWigFileViews}
\alias{BigWigFileViews,character-method}

% Methods:
\alias{coverage,BigWigFileViews-method}
\alias{summary,BigWigFileViews-method}

\title{Views into a set of BigWig files}

\description{
  Use \code{BigWigFileViews()} to reference a set of disk-based BAM files to be
  processed (e.g., queried using \code{\link{coverage}}) as a single
  \sQuote{experiment}.
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{BigWigFileViews(filePaths, fileIndices=filePaths,
        fileSamples=DataFrame(row.names=make.unique(basename(filePaths))),
        fileRanges, fileExperiment = list(), byFile = TRUE,
        yieldSize = NA_integer_, 
        .views_on_file = new.env(parent=emptyenv()), ...)}:
      This constructor is a generic function with dispatch on argument
      \code{filePaths}. Methods exist for character vector of file names. 
    }
  }
}

\section{Accessors}{
  All accessor-like methods defined for \code{FileViews} objects
  work on \code{BigWigFileViews} objects. See ?\code{FileViews} for details.
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
  In the code snippets below, \code{file} is a BigWigFileViews object.

  \describe{
    \item{}{
       \code{coverage(file, index = file, ...)}:
       Visit each path in \code{filePaths(file)}, returning
       the result of \code{coverage} applied to the specified
       path.
    }
    \item{}{
       \code{summary(file, index = file, ...)}:
       Visit each path in \code{filePaths(file)}, returning
       the result of \code{summary} applied to the specified
       path.
    }
  }
}

\section{Arguments}{
  \describe{
    \item{}{
       \code{filePaths}:
         A character() vector of BigWig path names.
    }
    \item{}{
      \code{fileIndices}:
        A character() vector of BigWig index file path names.
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
        on the spaces of the BigWig files. Ranges are \emph{not} validated against 
        the BigWig files.
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
      \code{x}, \code{object}: An instance of \code{BigWigFileViews}.
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
      \code{file}: An instance of \code{BigWigFileViews}.
    }
    \item{}{
      \code{index}: Not used.
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

\author{Michael Love <michaelisaiahlove@gmail.com>}

\examples{
  ## ---------------------------------------------------------------------
  ## A. BigWigFileView Objects
  ## ---------------------------------------------------------------------

  if (.Platform$OS.type != "windows") {
    fl <- system.file("tests", "test.bw", package = "rtracklayer")
  
    rngs <- GRanges(seqnames = Rle(c("chr2", "chr19"), c(4, 2)),
                    ranges = c(IRanges(c(200,250,500,550),width=100),
                               IRanges(c(1450,1750),width=100)))
    v <- BigWigFileViews(c(fl, fl), fileRanges=rngs)
    v
    v[1:3,]
    v[,2]
    v[1:3,2]
    fileRanges(v[c(1:2, 5:6),])

  ## ---------------------------------------------------------------------
  ## B. coverage and summary 
  ## ---------------------------------------------------------------------
    res <- coverage(v)
    res[[1]]
    res <- summary(v)
    res[[1]]
  } 
}

\keyword{classes}
\keyword{methods}

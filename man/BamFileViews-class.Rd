\name{BamFileViews}
\docType{class}

% Class:
\alias{BamFileViews-class}
\alias{BamFileViews}
\alias{BamFileViews,character-method}
\alias{BamFileViews,BamFileList-method}

% Methods:
\alias{scanBam,BamFileViews-method}
\alias{countBam,BamFileViews-method}

\title{Views into a set of BAM files}

\description{
  Use \code{BamFileViews()} to reference a set of disk-based BAM files to be
  processed (e.g., queried using \code{\link{scanBam}}) as a single
  \sQuote{experiment}.
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{BamFileViews(filePaths, fileIndices=filePaths,
        fileSamples=DataFrame(row.names=make.unique(basename(filePaths))),
        fileRanges, fileExperiment = list(), byFile = TRUE,
        yieldSize = NA_integer_, 
        .views_on_file = new.env(parent=emptyenv()), ...)}:
      This constructor is a generic function with dispatch on argument
      \code{filePaths}. Methods exist for \link{BamFileList} and character 
      (vector of file names). 
    }
  }
}

\section{Accessors}{
  All accessor-like methods defined for \code{FileViews} objects
  work on \code{BamFileViews} objects. See ?\code{FileViews} for details.
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
  In the code snippets below, \code{file} is a BamFileViews object.

  \describe{
    \item{}{
       \code{scanBam(file, index = file, ..., 
           param = ScanBamParam(what=scanBamWhat()))}:
       Visit each path in \code{filePaths(file)}, returning
       the result of \code{scanBam} applied to the specified
       path. \code{fileRanges(file)} takes precedence over
       \code{bamWhich(param)}.
    }
    \item{}{
       \code{countBam(file, index = file, ..., param = ScanBamParam())}:
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
         A character() vector of BAM path names or a \code{\linkS4class{BamFileList}}.
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
      \code{x}, \code{object}: An instance of \code{BamFileViews}.
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
      \code{file}: An instance of \code{BamFileViews}.
    }
    \item{}{
      \code{index}: Not used.
    }
    \item{}{
      \code{param}:
      An optional \code{\linkS4class{ScanBamParam}} instance to further 
      influence scanning or counting.
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
  ## A. BamFileView Objects
  ## ---------------------------------------------------------------------

  fl <- system.file("extdata", "ex1.bam", package="Rsamtools",
                    mustWork=TRUE)
  rngs <- GRanges(seqnames = Rle(c("chr1", "chr2"), c(9, 9)),
                  ranges = c(IRanges(seq(10000, 90000, 10000), width=500),
                             IRanges(seq(100000, 900000, 100000), width=5000)),
                  Count = seq_len(18L))
  v <- BamFileViews(c(fl, fl), fileRanges=rngs)
  v
  v[1:5,]
  fileRanges(v[c(1:5, 11:15),])
  fileDirname(v) <- getwd()
  v

  ## ---------------------------------------------------------------------
  ## B. countBam and scanBam 
  ## ---------------------------------------------------------------------
  v <- BamFileViews(c(fl, fl))
  countBam(v)
  scanBam(v)
}

\keyword{classes}
\keyword{methods}

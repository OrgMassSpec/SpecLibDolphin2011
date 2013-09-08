\name{LibraryReport}
\alias{LibraryReport}
\title{Mass Spectral Library Report}
\description{Generate a report for a mass spectral library.}
\usage{LibraryReport(spectra = dolphin2011.spec,
                          metadata = dolphin2011.meta,
                          structures = paste(system.file(package = "SpecLibDolphin2011"), "/extdata/struct/", sep = ""),
                          pdf = TRUE,
                          pdf.file = "SpecLibDolphin2011.pdf",
                          pdf.title = "SpecLibDolphin2011 Library",
                          x.min = 40)
}

\arguments{
   \item{spectra}{data frame containing the mass spectra. See below for
    required column names.}
    \item{metadata}{data frame containing the metadata. See below for
    required column names.}
    \item{structures}{character string of directory containing the structure image files.}
    \item{pdf}{logical indicating if a PDF report should be generated.}
    \item{pdf.file}{character string. The file name of the PDF
    report. See \code{\link{pdf}}.}
    \item{pdf.title}{character string. Title string to embed in the PDF
    file. See \code{\link{pdf}}.} 
    \item{x.min}{numeric value.  The minimum m/z in the plot.}
}
\details{See the OrgMassSpecR - Mass Spectral Libraries vignette for an
  overview and examples.

  m/z values printed within the spectrum generally refer to the most
  intense peak within the cluster. m/z values printed in the table
  generally refer to the monoisotopic ion. Marks printed above the
  spectrum indicate the identified ions. If the identity includes the
  word \dQuote{interference}, the color of the mark is grey.

  The x-axis range is set using the argument \code{x.min} and (if
  present) the molecular weight calculated from the \code{formula}
  listed in the metadata plus 3\%.

  \code{LibraryReport} checks for the column names listed below.  These
  columns must be present, but in most cases their contents may be
  \code{NA}.  The contents are printed, so the types (character,
  numeric, ...) can be user defined.  The exception within
  \code{metadata} is the column \code{filename}, which must contain
  character strings or \code{NA} (factors are coerced to
  characters). \code{NA}s in \code{metadata$filename} will be skipped
  when generating the report.  Additional columns in \code{metadata} and
  \code{spectra} are ignored by LibraryReport.

  An error will be generated if (1) \code{metadata} does not contain the
  column names listed below, (2) \code{spectra} does not contain the
  column names listed below, or (3) if \code{metadata$filename \%in\%
  unique(spectra$filename)} is not all \code{TRUE} (excluding \code{NAs}
  in metadata$filename).

  The directory containing the structure images must contain either .pgm
  or .ppm files.  Filenames must correspond to
  \code{metadata$filename}s.  Absent structure images will be skipped
  (spectrum and metatdata is plotted, structure is not).

  Required columns in \code{spectra}:

  \itemize{
    \item \code{filename}: character string matching the corresponding
    row in \code{metadata} and element name in\code{structures}.
    \item \code{mz}: centroid \emph{m/z} value
    \item \code{intensity}: peak intensity 
    \item \code{display}: logical indicating if the \emph{m/z} value
    should be displayed over the peak.
    \item \code{identity}: identity of the fragment ion.
  }

  Required columns in \code{metadata}:

  \itemize{
    \item \code{compound}: compound name.
    \item \code{filename}: character string matching the corresponding
    spectrum in \code{spectra} and element name in \code{structures}.
    \item \code{sample}: sample name.
    \item \code{instrument}: type of instrument and operating conditions.
    \item \code{category.1}: user defined category.
    \item \code{category.2}: user defined category.
    \item \code{category.3}: user defined category.
    \item \code{rt.1D}: retention time, in seconds, in the first
    dimension.
    \item \code{rt.2D}: retention time, in seconds, in the second
    dimension.
    \item \code{formula}: elemental formula.  Must be readable by
    \code{link{ListFormula}}.
    \item \code{comment}: user comment.
  }
  
}
\value{A PDF report of the mass spectra.  If PDF = FALSE, the report is
  generated using the graphics device.
}
\examples{

## Not run

## Example code to make filenames consistent, where
## x is a data frame with old and new filenames.

## To change structure filenames
\dontrun{
file.rename(paste(x$old.filename, ".pgm", sep = ""),
            paste(x$new.filename, ".pgm", sep = ""))
}

## To rename filenames in lib.dolphin2011.spec
\dontrun{
spec <- lib.dolphin2011.spec
for(i in 1:nrow(x)) {
  spec$filename[spec$filename == x$old.filename[i]] <- x$new.filename[i]
}
}

}

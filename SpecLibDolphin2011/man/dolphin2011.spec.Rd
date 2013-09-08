\name{dolphin2011.spec}
\alias{dolphin2011.spec}
\alias{dolphin2011.meta}
\docType{data}
\title{Package data.}
\description{Metadata and spectral data for SpecLibDolphin2011.}
\usage{dolphin2011.spec}
\format{\code{dolphin2011.spec} is a data frame containing the mass spectra.  \code{dolphin2011.meta} is a data frame containing the metadata.  Structure images (\code{.pgm} or \code{.ppm} files) are in \code{/inst/extdata/struct}.

  The \emph{m/z} labels in the mass spectrum refer to the most
  abundant peak in the cluster.  The peak \emph{m/z} values listed in
  the table refer to the monoisotopic peak in the cluster.  \dQuote{M}
  refers to the molecular ion. Additional information is in \code{\link{LibraryReport}}.
}
\seealso{\code{\link{LibraryReport}}}
\examples{
\dontrun{
# Write .msp file

WriteMspFile(spectra = dolphin2011.spec,
             metadata = dolphin2011.meta,
             filename = "SpecLibDolphin2011.txt",
             comment = "Contaminants in common dolphin (Delphinus delphis) blubber; 2006 Massachusetts stranding; Hoh E, et al., Environmental Science and Technology, 2012, 46 (15), 8001-8008. DOI: 10.1021/es301139q.")
}
}
\keyword{datasets} 


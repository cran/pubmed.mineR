\name{HGNC-class}
\Rdversion{1.1}
\docType{class}
\alias{HGNC-class}

\title{HGNC Class for package.}
\description{\code{"HGNC"} }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("HGNC", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{HGNCID}:}{Object of class \code{"character"}}
    \item{\code{ApprovedSymbol}:}{Object of class \code{"character"}}
    \item{\code{ApprovedName}:}{Object of class \code{"character"}}
    \item{\code{Status}:}{Object of class \code{"character"}}
    \item{\code{PreviousSymbols}:}{Object of class \code{"character"}}
    \item{\code{Aliases}:}{Object of class \code{"character"} }
    \item{\code{Chromosome}:}{Object of class \code{"character"}}
    \item{\code{AccessionNumbers}:}{Object of class \code{"character"}}
    \item{\code{RefSeqIDs}:}{Object of class \code{"character"}}
  }
}
\author{ Dr.S.Ramachandran, Ab Rauf Shah}
\seealso{
\code{\linkS4class{Abstracts}}
}
\examples{
showClass("HGNC")
}
\keyword{classes}

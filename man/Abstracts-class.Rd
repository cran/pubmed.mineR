\name{Abstracts-class}
\Rdversion{1.1}
\docType{class}
\alias{Abstracts-class}

\title{Class \code{"Abstracts"} Absract Class}
\description{S4 Class with three slots Journal, Abstract, PMID to store abstracts from PubMed}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Abstracts", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{Journal}:}{Object of class \code{"character"}to store Journals of the abstracts from PubMed}
    \item{\code{Abstract}:}{Object of class \code{"character"}to store Abstracts from the PubMed}
    \item{\code{PMID}:}{Object of class \code{"numeric"}to store PMIDs of abstracts from PubMed}
  }
}
\section{Methods}{
No methods defined with class "Abstracts" in the signature.
}
\author{ Dr.S.Ramachandran, Ab Rauf Shah }
\seealso{
\code{\link{searchabsL} \link{getabs} \link{contextSearch} \link{Genewise} \link{Yearwise} \link{combineabs} \link{subabs} \link{readabs}} 
}
\examples{
showClass("Abstracts")
}
\keyword{classes}

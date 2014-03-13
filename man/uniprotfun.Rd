\name{uniprotfun}
\alias{uniprotfun}
\title{To get information about gene from the UniProt.}
\description{\code{uniprotfun} will access the UniProt data for a given gene as per HGNC approved gene symbols}
\usage{
uniprotfun(y)
}
\arguments{
  \item{y}{HGNC approved gene symbol as character}
}
\details{ This function retrieves data from the UniProt. At present uniprotfun() works with only HGNC approved gene symbols. }
\value{ A text file written with filename as the 'query' name.}
\author{ Dr.S.Ramachandran }
\examples{
\dontrun{uniprotfun(x)}
}
\keyword{Function}


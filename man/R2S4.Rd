\name{R2S4}
\alias{R2S4}
\title{S4 Converter}
\description{\code{R2S4} reads tab delimited text file with headers Journal, Abstract PMID into the object of class "Abstracts".}
\usage{
R2S4(x)
}
\arguments{
  \item{x}{A tab delimited text file}
}
\details{ This function is necessary for the conversion of a text file into S4 object of class 'Abstracts'. }
\value{ An S4 object of class Abstracts }
\author{ S.Ramachandran, Jyoti Sharma }
\examples{
\dontrun{R2S4("filename.txt")}
}
\keyword{Function}


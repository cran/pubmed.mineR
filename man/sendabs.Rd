\name{sendabs}
\alias{sendabs}
\title{To send abstracts}
\description{\code{sendabs} will send the abstracts into a tab delimited text file with the fields Journal, Abstract, and PMID.}
\usage{
sendabs(object, x)
}
\arguments{
  \item{object}{An S4 object of class 'Abstracts'}
  \item{x}{"filename.txt" to write the abstracts}
}
\details{A general writing function for object of class 'Abstracts'}
\value{A tab delimited text file with headers Journal, Abstract, PMID.}
\author{Dr.S.Ramachandran}
\examples{
\dontrun{sendabs(x,"abs.txt")}
## here 'x' is the S4 object of class 'Abstracts' and 
## 'abs.txt' is the file where abstracts will be written.
}
\keyword{Functions}

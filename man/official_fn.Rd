\name{official_fn}
\alias{official_fn}
\title{
To extract the official gene symbol.
}
\description{
\code{official_fn} is used to fetch the official gene symbol from HGNC.
}
\usage{
official_fn(genes, abs, filename, terms)
}
\arguments{
  \item{genes}{
\code{genes} is output of gene_atomization, a table containing gene with its frequency.
}
  \item{abs}{
\code{abs} an S4 object of class Abstracts.
}
  \item{filename}{
\code{filename} specifies the name of output file.
}
  \item{terms}{
\code{terms} query term to be search in the abstracts. 
This is to restrict our search for particular theme from the abstracts
}
}
\value{
It will return a text file containing corresponding official gene symbol.
}
\author{
S.Ramachandran
}
\examples{
\dontrun{
official_fn(genes, diabetes_abs, "prevsym.txt", c("diabetic nephropathy", "DN")
}
## genes output of gene_atomization()
}
\keyword{function}

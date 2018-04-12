\name{official_fn}
\alias{official_fn}
\title{
To extract the sentences containing official gene symbol from abstracts.
}
\description{
\code{official_fn} is used to fetch the sentences containing official gene symbol from HGNC.
}
\usage{
official_fn(genes, abs, filename, terms)
}
\arguments{
  \item{genes}{
\code{genes} is output of gene_atomization, or a table containing HGNC gene symbols in first column with its frequency in second column.
}
  \item{abs}{
\code{abs} an S4 object of class Abstracts.
}
  \item{filename}{
\code{filename} specifies the name of output file.
}
  \item{terms}{
\code{terms} second query term to be searched in the same sentence (co-occurrence) of abstracts. 

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
official_fn(genes, diabetes_abs, "genes", c("diabetic nephropathy", "DN"))
}
## genes output of gene_atomization()
}
\keyword{function}

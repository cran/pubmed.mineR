\name{names_fn}
\alias{names_fn}
\title{
To extract the gene names from HGNC. 
}
\description{
\code{names_fn} matches the gene symbols to gene names and extract from HGNC.
}
\usage{
names_fn(genes, data, abs, filename, terms)
}
\arguments{
  \item{genes}{
\code{genes} is output of gene_atomization, a table containing gene with its frequency.
}
  \item{data}{
\code{data} is HGNC data
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
It returns an output file containing genes with their corresponding gene names.
}

\author{
S.Ramachandran
}
\examples{
\dontrun{
names_fn(genes, HGNCdata, diabetes_abs, "prevsym.txt", c("diabetic nephropathy", "DN")
}
## genes output of gene_atomization()
}
\keyword{function}

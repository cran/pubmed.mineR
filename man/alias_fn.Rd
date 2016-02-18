\name{alias_fn}
\alias{alias_fn}
\title{
To Find Alias of the Genes.
}
\description{
\code{alias_fn} will return the gene alias from the Abstracts using HGNC data.
}
\usage{
alias_fn(genes, data, abs, filename, terms)
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
It returns an output file containing genes with their corresponding aliases.
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{prevsymbol_fn}}
}
\examples{
\dontrun{
alias_fn(genes, HGNCdata, diabetes_abs, "prevsym.txt", c("diabetic nephropathy", "DN")
}
## genes output of gene_atomization()
}
\keyword{function}


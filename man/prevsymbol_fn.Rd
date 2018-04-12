\name{prevsymbol_fn}
\alias{prevsymbol_fn}
\title{
To extract the sentences containing Previous symbols of HGNC genes.
}
\description{
\code{prevsymbol_fn} will return the sentences containing previous symbols of the genes from the abstracts using HGNC data.
}
\usage{
prevsymbol_fn(genes, data, abs, filename, terms)
}
\arguments{
  \item{genes}{
\code{genes}  is output of gene_atomization, or a table containing HGNC gene symbols in first column with its frequency in second column.
}
  \item{data}{
\code{data} is HGNC data
 table with all 49 features (columns) available from the web site https://www.genenames.org/
}
  \item{abs}{
\code{abs} an S4 object of class Abstracts.
}
  \item{filename}{
\code{filename} specify the name of output file
}
  \item{terms}{
\code{terms} second query term to be searched in the same sentence (co-occurrence) of abstracts.
}
}
\value{
It returns a text file containing gene symbol with corresponding previous symbols.
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{names_fn}}, \code{\link{official_fn}}
}
\examples{
\dontrun{
prevsymbol_fn(genes, data, diabetes_abs, "prevsym", c("diabetic nephropathy", "DN"))
}
}
\keyword{function}

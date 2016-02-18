\name{prevsymbol_fn}
\alias{prevsymbol_fn}
\title{
To Find Previous symbols of genes.
}
\description{
\code{prevsymbol_fn} will return the previous symbols of the genes from the abstracts using HGNC data.
}
\usage{
prevsymbol_fn(genes, data, abs, filename, terms)
}
\arguments{
  \item{genes}{
\code{genes} a character vector containing genes
}
  \item{data}{
\code{data} HGNC data (downloadable from the site)
}
  \item{abs}{
\code{abs} an S4 object of class Abstracts.
}
  \item{filename}{
\code{filename} specify the name of output file
}
  \item{terms}{
\code{terms} query terms to be search from the abstracts.
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
prevsymbol_fn("SIRT1", HGNCdata, diabetes_abs, "prevsym.txt", c("diabetic nephropathy", "DN")
}
}
\keyword{function}

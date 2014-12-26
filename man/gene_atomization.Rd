\name{gene_atomization}
\alias{gene_atomization}
\title{To Extract Genes from the Abstracts}
\description{\code{gene_atomization} will automatically fetch the genes (HGNC approved Symbol) from the text and report their frequencies. presently only HGNC approved symbols are used.}
\usage{
gene_atomization(m)
}
\arguments{
  \item{m}{An S4 object of class Abstracts}
}
\details{ The function writes a text file with file name "data_table.txt". The function gene_atomization() is used to obtain the name of genes along with their frequencies of occurence. }
\value{ A tab delimited table containing gene name and their frequencies of occurence.}
\author{ Dr.S.Ramachandran }
\examples{
\dontrun{gene_atomization(x)}
## here x is an S4 object of class 'Abstracts'containing the abstracts
}
\keyword{Function}


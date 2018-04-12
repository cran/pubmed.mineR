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
\value{ A tab delimited table containing gene name and their frequencies of occurrence.}
\author{ S.Ramachandran, Jyoti Rani}
\examples{
\dontrun{gene_atomization(myabs)}
## here myabs is an S4 object of class 'Abstracts'containing the abstracts
## uses older version of HGNC data (https://www.genenames.org/) by default. 
## users may also use other functions such as official_fn and related 
## family of functions for deeper data mining.
}
\keyword{Function}


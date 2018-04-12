\name{get_gene_sentences}
\alias{get_gene_sentences}
\title{
To extract the sentences for genes from the corpus.
}
\description{
\code{get_gene_sentences} is used to extract the exact sentence in which query gene is discussed.
}
\usage{
get_gene_sentences(genes, abs, filename)
}
\arguments{
  \item{genes}{
\code{genes} a character vector containing the gene symbols.
}
  \item{abs}{
\code{abs} an S4 object of class Abstracts
}
  \item{filename}{
\code{filename} specifies the output file name.
}
}
\value{
an output file containing the sentences for given gene. 
}
\author{
S.Ramachandran
}
\examples{
\dontrun{get_gene_sentences("RBP4", abstracts, "RBP4_sentence.txt")}
}
\keyword{Function}

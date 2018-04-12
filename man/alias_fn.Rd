\name{alias_fn}
\alias{alias_fn}
\title{
To extract sentences containing Alias of the Human Genes from Pubmed abstracts.
}
\description{
\code{alias_fn} This function returns the sentences containing alias of gene and the user given terms from the Abstracts using HGNC gee data table.
 In this sense this function is a 2 Dimensional search.}
\usage{
alias_fn(genes, data, abs, filename, terms)
}
\arguments{
  \item{genes}{
\code{genes} a table containing genes (official symbol,first column) with its frequency of occurrence (second column)could be an output of gene_atomization function and subsequently subsetting the table using for example the code genes_table = subset(t2diababs_genes, select = c("Gene_symbol","Freq")). Alternatively, a custome gene table can be supplied with two columns, the first one being the column for Gene symbols and the second one being the Frequency of occurrence. If Frequency of occurrence is not available then a dummy value of 1 can be set.
}
  \item{data}{
\code{data} is HGNC data
 table with all 49 features (columns) available from the web site https://www.genenames.org/}
  \item{abs}{
\code{abs} an S4 object of class Abstracts.
}
  \item{filename}{
\code{filename} specifies the name of output file. Please note that the term alias will be suffixed to the given filename.
}
  \item{terms}{
\code{terms} query term(s) to be search in the abstracts, could be a vector of terms.
}
}
\value{An output file containing sentences with aliases of genes.For convenience both the official symbol and the corresponding alias are written in the output. The PMID of the corresponding Abstract containing the extracted sentence also appears just before the sentence. Note that multiple sentences from different abstracts are clubbed together under one gene alias that appears in those sentences.
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{prevsymbol_fn}}
}
\examples{
\dontrun{alias_fn(genes,data,myabs,"nephro_",c("diabetic nephropathy","kidney disease"))
}
## genes output of gene_atomization()
}
\keyword{function}


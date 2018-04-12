\name{names_fn}
\alias{names_fn}
\title{
To extract the sentences in asbtracts containing gene names from HGNC. 
}
\description{
\code{names_fn} matches the gene symbols to gene names and extract from HGNC.
}
\usage{
names_fn(genes, data, abs, filename, terms)
}
\arguments{
  \item{genes}{
\code{genes} is output of gene_atomization or a table containing HGNC gene symbols in first column with its frequency in second column.
}
  \item{data}{
\code{data} is HGNC data
 table with all 49 features (columns) available from the web site https://www.genenames.org/
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
It returns an output file containing genes with their corresponding gene names and sentences with co-occurrences if any.
}

\author{
S.Ramachandran
}
\examples{
\dontrun{
names_fn(genes, data, diabetes_abs, "names", c("diabetic nephropathy", "DN"))
}
## genes output of gene_atomization()
}
\keyword{function}

\name{co_occurrence_advance}
\alias{co_occurrence_advance}
\title{Extracts multiple sentence with co-occurrence of two sets of terms)
}
\description{
Extracts single or multiple sentences with co-occurrence of given terms}
\usage{
co_occurrence_advance(abstract, term1, term2, n)
}
\arguments{
  \item{abstract}{
an S4 object of class Abstracts
}
  \item{term1}{
a character vector of terms
}
  \item{term2}{
a character vector of terms
}
  \item{n}{
A numeric value, which can be 0,1,2. 
}
}
\details{
Sentences with co-occurrence of two terms will be extracted along with the 
corresponding PMIDs. 
The output will be a data frame. In regard to the argument n, when the value 
is 0 then
the co-occurrence is sought in the same sentence. When the 
value is 1, then the co-occurrence is sought in two consecutive
sentences, namely, first term in the first sentence and second term
in the next sentence. When the value is 2, then the co-occurrence
is sought in two sentences separated by a sentence without either term1
or term2. 
}
\value{
It will return a data frame object containing PMID,sentences and the terms 
pairs. 
}
\author{
Shashwat Badoni
Surabhi Seth
}

\seealso{
\code{\link{co_occurrence_fn}}
}
\examples{
\dontrun{co_occurrence_advance(myabs,"resistance", c("genes","genetic"), 2}}

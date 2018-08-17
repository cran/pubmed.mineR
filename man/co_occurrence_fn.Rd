\name{co_occurrence_fn}
\alias{co_occurrence_fn}
\title{Extracts sentences with co-occurrence of two sets of terms}
\description{\code{co_occurrence_fn} will automatically extract sentences with co-occurrence of two sets of terms.}
\usage{
co_occurrence_fn(terms1, abs, filename, terms2)
}
\arguments{
  \item{terms1}{a character vector of terms.}
  \item{abs}{an S4 object of class Abstracts}
  \item{filename}{a single character,  filename}
  \item{terms2}{a character vector of terms.}
}
\details{Sentences with co-occurrence of two terms will be extracted along with the corresponding PMIDs. The data will be written in a text file with the user given filename and the word co_occurrence will be suffixed to it.}
\value{A text file.}
\author{S.Ramachandran}
\examples{
\dontrun{co_occurrence_fn("resistance",myabs,"resistance_genetic",c("genes","genetic")}
##
}
\keyword{Function}

\name{space_quasher}
\alias{space_quasher}
\title{Removes extra spaces between words.}
\description{\code{space_quasher} will automatically remove extra spaces between words. Therefore only one space between any pair of words will be left}
\usage{
space_quasher(x)
}
\arguments{
  \item{x}{x is a text with single or multiple sentences given within double quotes.}
}
\details{The extra spaces between words in sentences is quashed to one via space_quasher().}
\value{Sentences(s) in which extra spaces between any pair of words are quashed to one.}
\author{S.Ramachandran}
\examples{
\dontrun{space_quasher("I am       a ghostbuster. I have the tools    required to  hunt ghosts")}
##here pubmed_result.txt is the text file of abstracts saved from PubMed.
}
\keyword{Function}

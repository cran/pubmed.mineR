\name{contextSearch}
\alias{contextSearch}
\title{For Context Search}
\description{\code{contextSearch} is a method to extract the sentences containing a given query term}
\usage{
contextSearch(object, y)
}
\arguments{
  \item{object}{An S4 object of Class Abstracts containing text abstracts}
  \item{y}{a character vector of term(s)}
}
\details{It takes object of class Abstracts and query term(s) as arguments and returns a text and latex file of the sentences containing query term.
The latex file can be further converted into PDF by using the system command in R i.e. system("pdflatex filename.tex"). pdflatex is a shell command in Linux to convert the latex file into PDF. In the pdf file the terms are written in bold face type to enable ease of reading}
\value{contextSearch() will write two files one is a text file named "companion.txt", and other is a Latex file. If the single term is given in query then file name comes with the term name. If multiple terms are used then the file name will be "combined.tex" }
\author{Dr.S.Ramachandran}
\examples{
\dontrun{contextSearch(x, "diabetes")}
## here 'x' is S4 object of class 'Abstracts', and query term is 'diabetes'.
}
\keyword{Function}

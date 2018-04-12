\name{find_intro_conc_html}
\alias{find_intro_conc_html}
\title{To find the introduction and conclusion from the abstracts.}
\description{it helps to fetch the introduction and conclusion part from the abstracts.}
\usage{
find_intro_conc_html(y, themes, all)
}
\arguments{
  \item{y}{and S4 object of class Abstracts }
  \item{themes}{a character vector containing terms to be search in the abstracts}
  \item{all}{is logical, if true, will include title and author otherwise only abstracts will be considered.}
}
\details{\code{find_intro_conc_html}provides an HTML file containing space separated introduction and conclusion part from the abstracts of given query term as well as gives a link directly to PubMed for the resulting PMID.}
\value{an HTML file.}
\author{S.Ramachandran, Jyoti Rani}
\seealso{\code{\link{input_for_find_intro_conc_html}}
}
\examples{\dontrun{test = find_intro_conc_html(abs, "diet", all=FALSE)}
## here 'abs' is an S4 object of class Abstracts
## and 'diet' is a term to be search from the abstracts
## this function works for small size of corpus, say about 30-40 abstracts}
\keyword{Function}

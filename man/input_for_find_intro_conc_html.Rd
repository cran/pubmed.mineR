\name{input_for_find_intro_conc_html}
\alias{input_for_find_intro_conc_html}
\title{fetch the abstracts using E-utilities.}
\description{it helps in searching and fetching the abstracts from E-utilities using PMIDs.}
\usage{
input_for_find_intro_conc_html(y, all)
}
\arguments{\item{y}{an S4 object of class Abstracts}
\item{all}{is logical if true, will include title and author otherwise only abstracts.}}
\details{it takes ans S4 object as input and uses its PMIDs to fetch the abstracts from E-utilities. The output will be used as inout for find_intro_conc_html as it contains neat data i.e. abstracts only.}
\value{a list containing abstracts and PMID}
\references{literature/http:/eutils.ncbi.nlm.nih.gov/}
\author{S.Ramachandran, Jyoti Sharma}
\seealso{\code{\link{find_intro_conc_html}}}
\examples{\dontrun{test=input_for_find_intro_conc_html(abs)}
## here 'abs' is an S4 object of class Abstracts.}
\keyword{Function}

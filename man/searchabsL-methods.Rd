\name{searchabsL-methods}
\docType{methods}
\alias{searchabsL-methods}
\alias{searchabsL,Abstracts-method}
\title{Searching Abstracts}
\description{\code{searchabsL} will automatically search the abstracts from the data for the given terms or their combination of several terms.}
\section{Methods}{
\describe{

\item{\code{signature(object = "Abstracts")}}{
searchabsL will search the abstracts for the given term or combinations of several terms. In this method the argument "include" uses the boolean operator 'OR' and is liberal whereas the 'restrict' and 'exclude' use the boolean operator 'AND' to specify additional filters. If the restriction to individual terms are desired then they can be individually searched and then the multiple abstracts can be combined using combineasb() function.
}
}}
\keyword{methods}


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_articles.R
\name{get_articles}
\alias{get_articles}
\title{Gets the artciles from the top news search results for the company name}
\usage{
get_articles(company_name, n = 10)
}
\arguments{
\item{company_name}{name of the company}

\item{n}{number of articles}
}
\value{
character
}
\description{
Gets the artciles from the top news search results for the company name
}
\examples{
get_articles("Debenhams")
get_articles("Vodafone")
}

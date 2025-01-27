% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topic_coherence.R
\name{ask_palmetto}
\alias{ask_palmetto}
\title{Get topic coherence from Palmetto remote or local}
\usage{
ask_palmetto(solution, depth = "full", coherence = "npmi", backend = "web")
}
\arguments{
\item{solution}{A wide tibble where each column contains a top-N terms.}

\item{depth}{The number of top terms to use to use for coherence estimation. If \code{backend = "web"}, then the number cannot be less than 3 and more than 10.}

\item{coherence}{A name of coherence metric. Valid names: "ca", "cp", "cv", "npmi", "uci", "umass". Default is \code{npmi}.}

\item{backend}{Path to Palmetto either \code{"web"} or a string with an URL. Example: http://localhost:7777/service/.}
}
\value{
A wide tibble with requested coherence scores.
}
\description{
Get topic coherence from Palmetto remote or local
}
\examples{
df1 <- tibble::tibble(col1 = c("apple", "banana", "cherry"), col2 = c("dog", "cat", "mouse"))

ask_palmetto(df1)
}

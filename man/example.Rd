% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{example}
\alias{example}
\title{Example sentence in CONLL format}
\format{
A data frame containing 13 rows and 9 columns. The columns are as follows:
\describe{
\item{SENTID}{sentence id}
\item{TOKEN}{token}
\item{POS.TT}{part of speech generated by treetagger}
\item{LEMMA}{lemma}
\item{POS.MELT}{part of speech generated by MElt}
\item{DESC}{extra part of speech information (i.e. morpholgical tags)}
\item{POSITION}{position in the sentence}
\item{DEP_TYPE}{type of dependency relation}
\item{DEP_ON}{head of dependency}
}
}
\usage{
data(example)
}
\description{
An data frame which contains an example sentence in CONLL format
}
\keyword{datasets}

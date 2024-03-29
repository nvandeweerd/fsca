% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{test.sents}
\alias{test.sents}
\title{Example sentences in CONLL format}
\format{
A list containing 11 data frames. Each data frame has the following columns:
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
data(test.sents)
}
\description{
An list of example sentences in CONLL format
}
\keyword{datasets}

## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ------------------------------------------------------------------------
head(read.table("syntaxe-wiki.txt.outmalt"))

## ------------------------------------------------------------------------
library(fsca)
list <- getParse("syntaxe-wiki.txt.outmalt")
head(list[[1]])

## ------------------------------------------------------------------------
getUnits(list[[1]], paste.tokens = TRUE)

## ------------------------------------------------------------------------
getMeasures(list)


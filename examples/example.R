#example.csv is a csv containing a sentence in CONLL format
example <- read.csv("data/example.csv", stringsAsFactors = FALSE)

#Run function (full analysis)
synt.units.fcn(example)

#The output of the function is a list which can be subset
str(synt.units.fcn(example)[["VERB_PHRASES"]])
# List of 3
# $ NUMBER : int 3
# $ LENGTHS: int [1:3] 11 8 5
# $ TOKENS :List of 3
# ..$ : chr [1:11] "prétendent" "qu'" "il" "est" ...
# ..$ : chr [1:8] "est" "impossible" "de" "rééduquer" ...
# ..$ : chr [1:5] "rééduquer" "un" "tel" "jeune" ...

#subset further to return the counts, lengths or tokens
synt.units.fcn(example)[["VERB_PHRASES"]][["NUMBER"]]
synt.units.fcn(example)[["VERB_PHRASES"]][["LENGTHS"]]
synt.units.fcn(example)[["VERB_PHRASES"]][["TOKENS"]]

#Because NUMBER is an integer vector, calculations can also be done
#directly on the output

vp_len <- synt.units.fcn(example02)[["VERB_PHRASES"]][["LENGTHS"]]
mean(vp_len)
sd(vp_len)

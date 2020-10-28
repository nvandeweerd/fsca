#example.csv is a csv containing a sentence in CONLL format
example <- read.csv("tests/testthat/data/example.csv", stringsAsFactors = FALSE)

#Run function (full analysis)
synt.units.fcn(example)

#The output of the function is a list which can be subset
str(synt.units.fcn(example)[["VERB_PHRASES"]])


#subset further to return the counts, lengths or tokens
synt.units.fcn(example)[["VERB_PHRASES"]][["NUMBER"]]
synt.units.fcn(example)[["VERB_PHRASES"]][["LENGTHS"]]
synt.units.fcn(example)[["VERB_PHRASES"]][["TOKENS"]]

#Because NUMBER is an integer vector, calculations can also be done
#directly on the output

vp_len <- synt.units.fcn(example02)[["VERB_PHRASES"]][["LENGTHS"]]
mean(vp_len)
sd(vp_len)

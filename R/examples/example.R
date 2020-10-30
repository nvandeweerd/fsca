
#`example`is a csv containing a sentence in CONLL format
data(example)

#Run function (full analysis)
getUnits(example)

#get counts of units
getUnits(example, what = "number")

#get lengths of units
getUnits(example, what = "lengths")

#get tokens
getUnits(example, what = "tokens")

#get tokens of a specific unit only
getUnits(example, what = "tokens", unit = c("NOUN_PHRASES"))

#get tokens of a specific unit only and pastes them together
getUnits(example, what = "tokens", unit = c("NOUN_PHRASES"), paste.tokens = TRUE)



Example analysis using the `fsca` package
================

This vignette illustrates a complete analysis of syntactic complexity in
one text from start to finish.

# Parse Text

The text file `syntaxe-wiki.txt` contains some text from the [French
wikipedia page about Syntax](https://fr.wikipedia.org/wiki/Syntaxe).

> La syntaxe est, à l’origine, la branche de la linguistique qui étudie
> la façon dont les mots se combinent pour former des phrases ou des
> énoncés dans une langue. On distingue la syntaxe, qui concerne les
> expressions \[les mots\], de la sémantique, qui concerne ce qui est
> visé par les expressions \[le sens, la signification/les choses\]. Le
> terme de syntaxe est aussi utilisé en informatique, où sa définition
> est similaire, modulo une terminologie différente. Ainsi, la syntaxe
> est le respect, ou le non-respect, de la grammaire formelle d’un
> langage, c’est-à-dire des règles d’agencement des lexèmes (en
> informatique, ce sont des entités lexicales d’un langage informatique)
> en des termes plus complexes, souvent des programmes. Dans la théorie
> des langages formels, ce qui joue le rôle de lexème est en général
> appelé lettre ou symbole, et les termes produits sont appelés mots.

This text file was POS-tagged with MElt Tagger (Denis & Sagot, 2012) and
dependency parsed with Malt Parser (Nivre, Hall, & Nilsson, 2006). See
[here](http://alpage.inria.fr/statgram/frdep/fr_stat_dep_malt.html) for
instructions.

# Extract parsed text

The output of the pre-processing chain is a text file containing the
sentences in CONLL format. Each sentence is separated by an empty line.

``` r
head(read.table("syntaxe-wiki.txt.outmalt"))
#>   V1      V2      V3    V4    V5                  V6      V7 V8    V9 V10
#> 1  1      La      le     D   DET       g=f|n=s|s=def     100  2   det   _
#> 2  2 syntaxe syntaxe     N    NC         g=f|n=s|s=c       _  3   suj   _
#> 3  3     est    être     V     V m=ind|n=s|p=3|t=pst 1101011  0  root   _
#> 4  4       ,       , PONCT PONCT                 s=w       _  3 ponct   _
#> 5  5       à       à     P     P                   _ 1100110  3   mod   _
#> 6  6      l'      le     D   DET           n=s|s=def     100  7   det   _
#>   V11
#> 1   _
#> 2   _
#> 3   _
#> 4   _
#> 5   _
#> 6   _
```

This output can be reformatted with the `getParse()` function. This
function separates each of the sentences into a list and prepares the
data frames for analysis (changing column names, data type etc.).

``` r
library(fsca)
list <- getParse("syntaxe-wiki.txt.outmalt")
head(list[[1]])
#>   SENTID   TOKEN   LEMMA POS.MELT                DESC POSITION DEP_TYPE
#> 1      1      La      le      DET       g=f|n=s|s=def        1      det
#> 2      1 syntaxe syntaxe       NC         g=f|n=s|s=c        2      suj
#> 3      1     est    être        V m=ind|n=s|p=3|t=pst        3     root
#> 4      1       ,       ,    PONCT                 s=w        4    ponct
#> 5      1       à       à        P                   _        5      mod
#> 6      1      l'      le      DET           n=s|s=def        6      det
#>   DEP_ON
#> 1      2
#> 2      3
#> 3      0
#> 4      3
#> 5      3
#> 6      7
```

# Analyze text

Syntactic units can then be extracted using the `getUnits()` function on
individual sentences (i.e. by subsetting the list of sentences):

``` r
getUnits(list[[1]], paste.tokens = TRUE)
#> $SENTENCES
#> $SENTENCES$NUMBER
#> [1] 1
#> 
#> $SENTENCES$LENGTHS
#> [1] 30
#> 
#> $SENTENCES$TOKENS
#> $SENTENCES$TOKENS[[1]]
#> [1] "La syntaxe est à l' origine la branche de la linguistique qui étudie la façon dont les mots se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> 
#> 
#> $CLAUSES
#> $CLAUSES$NUMBER
#> [1] 3
#> 
#> $CLAUSES$LENGTHS
#> [1] 30 19 15
#> 
#> $CLAUSES$TOKENS
#> $CLAUSES$TOKENS[[1]]
#> [1] "La syntaxe est à l' origine la branche de la linguistique qui étudie la façon dont les mots se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> $CLAUSES$TOKENS[[2]]
#> [1] "qui étudie la façon dont les mots se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> $CLAUSES$TOKENS[[3]]
#> [1] "dont les mots se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> 
#> 
#> $DEP_CLAUSES
#> $DEP_CLAUSES$NUMBER
#> [1] 2
#> 
#> $DEP_CLAUSES$LENGTHS
#> [1] 19 15
#> 
#> $DEP_CLAUSES$TOKENS
#> $DEP_CLAUSES$TOKENS[[1]]
#> [1] "qui étudie la façon dont les mots se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> $DEP_CLAUSES$TOKENS[[2]]
#> [1] "dont les mots se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> 
#> 
#> $CO_CLAUSES
#> $CO_CLAUSES$NUMBER
#> [1] 0
#> 
#> $CO_CLAUSES$LENGTH
#> [1] NA
#> 
#> $CO_CLAUSES$TOKENS
#> $CO_CLAUSES$TOKENS[[1]]
#> [1] "NA"
#> 
#> 
#> 
#> $T_UNITS
#> $T_UNITS$NUMBER
#> [1] 1
#> 
#> $T_UNITS$LENGTHS
#> [1] 30
#> 
#> $T_UNITS$TOKENS
#> $T_UNITS$TOKENS[[1]]
#> [1] "La syntaxe est à l' origine la branche de la linguistique qui étudie la façon dont les mots se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> 
#> 
#> $NOUN_PHRASES
#> $NOUN_PHRASES$NUMBER
#> [1] 12
#> 
#> $NOUN_PHRASES$LENGTHS
#>  [1]  2  2 24 21 17  2  5  2  2  2  2  2
#> 
#> $NOUN_PHRASES$TOKENS
#> $NOUN_PHRASES$TOKENS[[1]]
#> [1] "La syntaxe"
#> 
#> $NOUN_PHRASES$TOKENS[[2]]
#> [1] "l' origine"
#> 
#> $NOUN_PHRASES$TOKENS[[3]]
#> [1] "la branche de la linguistique qui étudie la façon dont les mots se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> $NOUN_PHRASES$TOKENS[[4]]
#> [1] "la linguistique qui étudie la façon dont les mots se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> $NOUN_PHRASES$TOKENS[[5]]
#> [1] "la façon dont les mots se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> $NOUN_PHRASES$TOKENS[[6]]
#> [1] "les mots"
#> 
#> $NOUN_PHRASES$TOKENS[[7]]
#> [1] "des phrases ou des énoncés"
#> 
#> $NOUN_PHRASES$TOKENS[[8]]
#> [1] "des énoncés"
#> 
#> $NOUN_PHRASES$TOKENS[[9]]
#> [1] "une langue"
#> 
#> $NOUN_PHRASES$TOKENS[[10]]
#> [1] "la branche"
#> 
#> $NOUN_PHRASES$TOKENS[[11]]
#> [1] "la linguistique"
#> 
#> $NOUN_PHRASES$TOKENS[[12]]
#> [1] "la façon"
#> 
#> 
#> 
#> $VERB_PHRASES
#> $VERB_PHRASES$NUMBER
#> [1] 4
#> 
#> $VERB_PHRASES$LENGTHS
#> [1]  4 18 12  9
#> 
#> $VERB_PHRASES$TOKENS
#> $VERB_PHRASES$TOKENS[[1]]
#> [1] "est à l' origine"
#> 
#> $VERB_PHRASES$TOKENS[[2]]
#> [1] "étudie la façon dont les mots se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> $VERB_PHRASES$TOKENS[[3]]
#> [1] "se combinent pour former des phrases ou des énoncés dans une langue"
#> 
#> $VERB_PHRASES$TOKENS[[4]]
#> [1] "former des phrases ou des énoncés dans une langue"
```

Syntactic complexity measures can be directly calculated for the entire
list using the `getMeasures()` function:

``` r
getMeasures(list)
#> $MLS
#> [1] 29
#> 
#> $DIVS
#> [1] 9.06
#> 
#> $T_S
#> [1] 1.2
#> 
#> $MLT
#> [1] 24.17
#> 
#> $DIVT
#> [1] 5.38
#> 
#> $C_T
#> [1] 2
#> 
#> $MLC
#> [1] 19.83
#> 
#> $DIVC
#> [1] 6.97
#> 
#> $MLNP
#> [1] 4.71
#> 
#> $DIVNP
#> [1] 5.77
#> 
#> $NP_C
#> [1] 4.33
```

# References

<div id="refs" class="references">

<div id="ref-Denis2012">

Denis, P., & Sagot, B. (2012). Coupling an annotated corpus and a
lexicon for state-of-the-art POS tagging. *Language Resources and
Evaluation*, *46*(4), 721–736.
<https://doi.org/10.1007/s10579-012-9193-0>

</div>

<div id="ref-Nivre2006">

Nivre, J., Hall, J., & Nilsson, J. (2006). MaltParser : A data-driven
parser-generator for dependency parsing. *LREC 2006*, 2216–2219.

</div>

</div>

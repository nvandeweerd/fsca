README
================

This document provides definitions for the syntactic units and explains
<<<<<<< HEAD
in more detail how they were extracted using the `synt.units.fcn()`
=======
in more detail how they were extracted using the *synt.units.fcn()*
>>>>>>> 04c462515aaf086de37b6bca61573070f4cad087
function written in R (R Core Team, 2019).

# Introduction

The input of the function is a dependency parsed sentence in CONLL
<<<<<<< HEAD
format. An example (slightly modified for readability) for Sentence 1 is
provided below.

1)  C’est un point très important.  
    *It is a very important point.* <br>

<!-- end list -->

``` r
lcf100[[91]] %>%
  select(TOKEN, LEMMA, POS = POS.MELT, POSITION, DEP_TYPE, DEP_ON) %>%
  print()
##          TOKEN     LEMMA   POS POSITION   DEP_TYPE DEP_ON
## 1723        C'        ce   CLS        1        suj      2
## 1724       est      être     V        2       root      0
## 1725        un        un   DET        3        det      4
## 1726     point     point    NC        4        ats      2
## 1727      très      très   ADV        5 advmod_ADJ      6
## 1728 important important   ADJ        6       amod      4
## 1729         .         . PONCT        7      ponct      2
```

<br>
=======
format. An example (slightly modified for readability) for Sentence (1)
is provided in Table 1.

1)  C’est un point très important.  
    *It is a very important point.* <br>
    
    | TOKE | N LEMM    | A POS     |  POSI | TION DEP\_ | TYPE DEP\_  | ON |
    | ---: | --------- | --------- | ----: | ---------- | ----------- | :- |
    | 1723 | C’        | ce        |   CLS | 1          | suj         | 2  |
    | 1724 | est       | être      |     V | 2          | root        | 0  |
    | 1725 | un        | un        |   DET | 3          | det         | 4  |
    | 1726 | point     | point     |    NC | 4          | ats         | 2  |
    | 1727 | très      | très      |   ADV | 5          | advmod\_ADJ | 6  |
    | 1728 | important | important |   ADJ | 6          | amod        | 4  |
    | 1729 | .         | .         | PONCT | 7          | ponct       | 2  |
    | <br> |           |           |       |            |             |    |
    
>>>>>>> 04c462515aaf086de37b6bca61573070f4cad087

In this case, the main verb of the sentence is *est* (‘is-3SG.PRES’) and
is labeled as the ‘root’. As the top level node of a sentence, it is not
dependent on any other word, hence the value of 0 in the DEP\_ON column.
Because *est* is the second word of the sentence, the position is 2
(indicated in the POSITION column). All words which are directly
dependent on *est* have the value of 2 in the DEP\_ON column. This
includes the subject *C’* (‘it’) as well as the subject attribute
*point* (‘point’). The sentence-final period is also dependent on the
root. The words which are directly dependent on *point* include the
determiner *un* (‘a’) and the adjective *important* (‘important’), which
is itself modified by the adverb *très* (‘very’). In this way, every
word in the sentence is dependent on one and only one word.\[1\]

# Syntactic structures

The *synt.units.fcn()* function first searches for a list of node words
for a given unit (e.g. nouns for noun phrases) and then extracts all of
the dependencies on each of the node words using the using the
*induced.subgraph()* function from the *igraph* package (Csardi &
Nepusz, 2006). The definitions used for each unit are provided below
along with examples extracted from the corpus. Each example provides the
original sentence followed by the units which were extracted. They have
been simplified for readability by pasting the tokens together.

## Sentences

Following Lu (2010: 481) we defined a sentence as “a group of words
delimited by one of the following punctuation marks that signal the end
of a sentence: period, question mark, exclamation mark, quotation mark
or ellipsis”. Because the CONLL input to the *synt.units.fcn()* function
is already segmented into sentences, no additional query is required.

## Clauses

Clauses are defined as structures with a subject and a finite verb
<<<<<<< HEAD
(Hunt, 1965). After identifying dependent clauses and T-units, clauses
include all T-units as well as all subordinate clauses emedded within
the T-units in a given sentence.
=======
(Hunt, 1965). After identifying dependent clauses (Section
@ref(dep-clauses)) and T-units (Section @ref(tunits)), clauses include
all T-units as well as all subordinate clauses emedded within the
T-units in a given sentence.
>>>>>>> 04c462515aaf086de37b6bca61573070f4cad087

## Dependent clauses

Dependent clauses are clauses which are semantically and/or structurally
dependent on a super-ordinate syntactic structure. They include nominal
<<<<<<< HEAD
clauses, adverbial clauses and adjectival clauses (Hunt, 1965; Lu,
2010). They must contain a finite verb and a subject.
=======
clauses (@ref(nomclause)), adverbial clauses (@ref(advclause)) and
adjectival clauses (@ref(adjclause)) (Hunt, 1965; Lu, 2010). They must
contain a finite verb and a subject.
>>>>>>> 04c462515aaf086de37b6bca61573070f4cad087

### Nominal clauses

Nominal clauses are extracted using subordinate conjunctions
(e.g. *que*, ‘which’) as the main node.

``` r
lcf400[[130]][["TOKEN"]] %>% 
  paste(collapse = " ")
## [1] "Ils prétendent qu' il est impossible de rééduquer un tel jeune criminel ."
synt.units.fcn(lcf400[[130]])[["DEP_CLAUSES"]][["TOKENS"]] %>%
  sapply(., paste, collapse = " ")
## [1] "qu' il est impossible de rééduquer un tel jeune criminel"
```

### Adverbial clauses

Adverbial clauses are also extracted using subordinate conjunctions as
the main node.

``` r
lcf400[[291]][["TOKEN"]] %>% 
  paste(collapse = " ")
## [1] "Quand les lois sont contre le droit , il n' y a qu' une héroïque façon de protester contre elles : les violer ( Hugo ) ."
synt.units.fcn(lcf400[[291]])[["DEP_CLAUSES"]][["TOKENS"]] %>%
  sapply(., paste, collapse = " ")
## [1] "Quand les lois sont contre le droit"
```

### Adjectival clauses

Adjectival clauses are extracted using pronouns and finite verbs that
modify a noun as the main nodes (e.g. *durent* and *arrivent* in the
example below).

``` r
lcf400[[16]][["TOKEN"]] %>% 
  paste(collapse = " ")
## [1] "Des procès qui durent des années , des déclarations d' impôts inhumainement longues et compliquées , un gouvernement qui n' arrive pas à prendre forme ..."
synt.units.fcn(lcf400[[16]])[["DEP_CLAUSES"]][["TOKENS"]] %>%
  sapply(., paste, collapse = " ")
## [1] "qui durent des années"             "qui n' arrive pas à prendre forme"
```

### Special cases

Clauses of the type ‘il y a’ are considered dependent clauses only if
*a* is directly dependent on a finite verb or if there is a finite verb
dependent on *a*. This means that adverbial clauses which function like
‘ago’ in English (e.g. *il y a deux ans…*; ‘two years ago…’) are
captured as dependent clauses but simple declaratives (e.g. *il y a une
maison*; ‘there is a house’) are not.

``` r
lcf100[[68]][["TOKEN"]] %>% 
  paste(collapse = " ")
## [1] "Il y a quelques siècles , les empereurs pourraient modifier les règles selon leur volonté ."
synt.units.fcn(lcf100[[68]])[["DEP_CLAUSES"]][["TOKENS"]] %>%
  sapply(., paste, collapse = " ")
## [1] "Il y a quelques siècles"
```

``` r
lcf100[[59]][["TOKEN"]] %>% 
  paste(collapse = " ")
## [1] "Ensuite , il y a des délits dus aux problèmes personnels survenus à la maison ou à l' école ."
synt.units.fcn(lcf100[[59]])[["DEP_CLAUSES"]][["TOKENS"]] %>%
  sapply(., paste, collapse = " ")
## [1] "NA"
```

Direct interrogatives in the form *est-ce que* are not considered the
head of subordinate clauses. Rather, the head of a clause is the finite
verb dominated by *est* as in interrogatives formed by inversion.

``` r
lcf100[[14]][["TOKEN"]] %>% 
  paste(collapse = " ")
## [1] "Est -ce que les règles sont nécessaires ?"
synt.units.fcn(lcf100[[14]])[["DEP_CLAUSES"]][["TOKENS"]] %>%
  sapply(., paste, collapse = " ")
## [1] "NA"
```

Citations or reported speech enclosed with French guillmets («»), single
or double quotation marks are also considered subordinate clauses.

## Coordinated clauses

Coordinated clauses are clauses which are not semantically and/or
structurally dependent on a super-ordinate syntactic structure but are
conjoined to one or more clauses of syntactically equal status. They may
be joined by a coordinating conjunction (e.g. *et*; ‘and’), punctuation
(e.g. semi-colon, colon, comma) or by juxtaposition and must contain
both a subject and a finite verb. They are extracted from the root nodes
of finite verbs which are not themselves dependent on subordinate
conjunctions or pronouns (to exclude dependent clauses).

``` r
lcf100[[97]][["TOKEN"]] %>% 
  paste(collapse = " ")
## [1] "Dans la prison , il n' ont plus d' éducation , il ne voient que des criminels et ils doivent devenir des adultes en nulle temps ."
synt.units.fcn(lcf100[[97]])[["CO_CLAUSES"]][["TOKENS"]] %>%
  sapply(., paste, collapse = " ")
## [1] "Dans la prison il n' ont plus d' éducation"       
## [2] "il ne voient que des criminels"                   
## [3] "et ils doivent devenir des adultes en nulle temps"
```

## T-units

We use Hunt’s (1970: 199) definition of a t-unit as “one main clause
plus any subordinate clause or non-clausal structure that is attached to
or embedded in it”. Identifying t-units therefore depends on the
identification of coordinated clauses since a sentence can only contain
multiple t-units if it contains multiple coordinated clauses. A sentence
that does not contain any coordinated clauses simply has one t-unit,
provided it has at least one finite verb. Consistent with Hunt (1965),
we do not classify sentence fragments (clauses without a finite verb) as
t-units. Therefore, if a sentence has no coordinated clauses (and one
finite verb) it has one t-unit. All additional coordinated clauses
within a sentence are separate t-units. The three coordinated clauses in
the sentence above therefore account for three t-units.

``` r
synt.units.fcn(lcf100[[97]])[["CO_CLAUSES"]][["NUMBER"]] 
## [1] 3
synt.units.fcn(lcf100[[97]])[["T_UNITS"]][["NUMBER"]] 
## [1] 3
```

## Noun phrases

We use Lu’s (2010) definition of a noun phrase as a *complex nominal*
(see Cooper, 1976) which includes: nouns plus adjective(s),
possessive(s), prepositional phrase(s), relative clause(s),
participle(s), or appositive(s), nominal clause(s). We also include
words (nouns, adverbs and pronouns) that are have a determiner
(e.g. *une maison* ‘a house’; *cet autre* ‘this other’) in our
definition. Following Lu (2010) and Cooper (1976), we also include
gerunds and infinitives in subject position. The root nodes of noun
phrases therefore include (within each t-unit) common nouns and proper
nouns as well as gerunds and non-finite verbs when they are dependent on
a finite verb and have a subject dependency label.

``` r
lcf400[[358]][["TOKEN"]] %>% 
  paste(collapse = " ")
## [1] "La question sur une nouvelle réforme de l' État est un grand problème qui se pose aujourd' hui en Belgique ."
synt.units.fcn(lcf400[[358]])[["NOUN_PHRASES"]][["TOKENS"]] %>%
  sapply(., paste, collapse = " ")
## [1] "La question sur une nouvelle réforme de l' État"       
## [2] "une nouvelle réforme de l' État"                       
## [3] "l' État"                                               
## [4] "un grand problème qui se pose aujourd' hui en Belgique"
## [5] "La question"                                           
## [6] "une nouvelle réforme"                                  
## [7] "un grand problème"
```

## Verb phrases

As in Lu (2010) we count both finite and non-finite verb phrases. These
are extracted by taking all words which are dependent on a verb within a
t-unit. Auxiliary verbs do not constitute their own verb phrase but are
considered part of the main verb they modify. However, verb phrases with
modal verbs are considered separate verb phrases. After extracting the
dependents of each verb node, the units are cleaned by removing subjects
and pre-verbal modifiers. When two verbs are coordinated they are also
considered a singular verb phrase (e.g. *ne sont pas d’accord et
présentent de solutions différents*; ‘do not agree and present
different solutions’).

``` r
lcf400[[31]][["TOKEN"]] %>% 
  paste(collapse = " ")
## [1] "Dotée d' un éventail de mots et d' expressions , la parole constitue le moyen de communication par excellence ."
synt.units.fcn(lcf400[[31]])[["VERB_PHRASES"]][["TOKENS"]] %>%
  sapply(., paste, collapse = " ")
## [1] "Dotée d' un éventail de mots et d' expressions"    
## [2] "constitue le moyen de communication par excellence"
```

# References

<div id="refs" class="references">

<div id="ref-Cooper1976">

Cooper, T. C. (1976). Measuring written syntactic patterns of second
language learners of German. *Journal of Educational Research*, *69*(5),
176–183. <https://doi.org/10.1080/00220671.1976.10884868>

</div>

<div id="ref-Csardi2006">

Csardi, G., & Nepusz, T. (2006). The igraph software package for complex
network research. *InterJournal (Complex Systems)*.

</div>

<div id="ref-Green2011">

Green, N. (2011). Dependency Parsing. *WDS’11 proceedings of contributed
papers, part i*. Prague.

</div>

<div id="ref-Hunt1965">

Hunt, K. (1965). *Grammatical structures written at three grade levels*.
Champaign, IL: NCTE.

</div>

<div id="ref-Hunt1970">

Hunt, K. (1970). Do sentences in the second language grow like those in
the first? *TESOL Quarterly1*, *4*(3), 195–202.

</div>

<div id="ref-Lu2010">

Lu, X. (2010). Automatic analysis of syntactic complexity in second
language writing. *International Journal of Corpus Linguistics*,
*15*(4), 474–496. <https://doi.org/10.1075/ijcl.15.4.02lu>

</div>

<div id="ref-RCoreTeam2019">

R Core Team. (2019). *R: A language and environment for statistical
computing*. Retrieved from <https://www.r-project.org/>

</div>

</div>

1.  For an overview of dependency parsing see Green (2011).

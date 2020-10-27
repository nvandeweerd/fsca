#synt.units.fcn(): An R function to extract syntactic units from L2 French texts


# install.packages("igraph")
# install.packages("purrr")

#load helper functions
#helper functions for syntactic extraction function



#import xml file (parsed texts) as data frame
xmlconll.fcn <- function(file) {
  current.text <- scan(file, what = character(), sep = "\n" ) #read in XML files
  df <- data.frame("text" = current.text, "sent" = rep(NA, length(current.text)))
  index <- 0
  for (x in seq(nrow(df)))
  {
    if (grepl("<s type=.*>", df$text[x])) {
      index <- index + 1
      df$sent[x] <- index

    } else {df$sent[x] <- index}

  }
  df <- df[which(grepl(pattern = "^<.*>$", df$text) == FALSE),]
  df$text <- as.character(df$text)
  for (y in seq(nrow(df))){
    df$text[y] <- paste(df$sent[y], df$text[y], sep="\t")
  }
  current.df <- read.table(text = df$text, #separate into columns
                           sep = "\t",
                           colClasses = "character",
                           quote = "")[,c(1:9)]
  colnames(current.df) <- c("SENTID",
                            "TOKEN",
                            "POS.TT",
                            "LEMMA",
                            "POS.MELT",
                            "DESC",
                            "POSITION",
                            "DEP_TYPE",
                            "DEP_ON")
  sent_list <- split(current.df, current.df$SENTID)
  return(sent_list)
}
#function to add a column based on the value in another column
colref_add.fcn <- function(x, from, to, what) {
  #x = data.frame
  #from = the column which contains the reference
  #to = the name of the new column
  #what = the column with the information to be copied to the new column
  for (i in seq(nrow(x))) {
    if (! is.na(x[[from]][i])) {
      x[[to]][i] <- toString(x[[what]][as.numeric(x[[from]][[i]])])
    }else {x[[to]][i] <- NA}
  }
  return(x)
}
#function to get the dependencies from a given node
node_deps.fcn <- function (x, graph) {

  node_deps <-as.numeric( #
    names(
      V( #get the vertices
        induced.subgraph( #of the subgraph
          graph,subcomponent(graph, x, mode = "in"))))) #which x is the main node
  return(node_deps)
}
#function to exclude certain parts of a segment
exclude.fcn <- function(x,y, keep = "exclude") {
  if(is.null(x)) {return(NULL)
  }else{
    #if the lengths are unequal return x
    if(length(x) != length(y)) {return(x)
    }else{
      excluded <- mapply(function(x,y) x[which(! x %in% y)], x, y, SIMPLIFY = FALSE)
      #delete the excluded words
      if(keep == "exclude"){return(excluded)}
      #delete the excluded words but keep the whole segment as well
      if(keep == "include"){return(unique(c(x, excluded)))}
      #delete elements which do not contain the words
      if(keep == "check"){
        checked <- x[which(mapply(function(x,y) any(x %in% y), x, y))]
        if(length(checked) == 0) {return(NULL)}
        if(length(checked) != 0) {return(checked)}
      }
    }}
}
#function to extract noun phrases
np.fcn <- function(TU, coordinators, punct) {

  TOKEN <-"TOKEN" #name of the column with the tokens
  POSITION <- "POSITION" #name of columnn with the word index
  DEP_ON <-  "DEP_ON" #name of column with the dependency heads
  DEP_TYPE <-"DEP_TYPE" #name of column with the dependency relations
  POS <- "POS.MELT"  #name of column with the part of speech tags
  LEMMA <- "LEMMA" #name of column with the lemmas
  DEP_ON_POS <- "DEP_ON_POS"
  DEP_ON_DEPTYPE <- "DEP_ON_DEPTYPE"
  DEP_ON_LEMMA <- "DEP_ON_LEMMA"

  #graph of t-unit
  g_tu <- graph_from_data_frame(TU[,c(POSITION, DEP_ON)])
  #common nouns and proper nouns serve as the root nodes
  nouns <- TU[[POSITION]][which(TU[[POS]] %in% c("NC", "NPP"))]
  #as well as infinitives and gerunds in subject position
  subjects <-  TU[[POSITION]][which(
    (TU[[DEP_TYPE]] %in% c("suj")) &
      (TU[[POS]] %in% c("VINF", "VPR"))  )]
  #non-nouns which have a determiner
  det<- TU[[DEP_ON]][which(
    (! TU[[DEP_ON_POS]] %in% c("NC", "NPP")) &
      (TU[[DEP_TYPE]] == "det") &
      (! TU[[DEP_ON_POS]] == "ADJ")
  )]
  nouns <- c(nouns, subjects, det)

  #embedded noun phrases
  noun_complements <- lapply(nouns, function(x) TU[[POSITION]][which(
    (TU[[DEP_TYPE]] %in% c("mod_rel", "dep", "mod")) &
      (TU[[DEP_ON]] == x) &
      (TU[[POS]] != "ADJ")   )])
  NOUN_PHRASES <- extract.fcn(nouns, exclude = noun_complements, g_tu, punct = punct, keep = "include")

  #coordinated noun phrases
  coord_NP <- coord_phrases.fcn(TU, g_tu,  "(NC|NPP)")
  if (length(coord_NP) != 0) {
    NOUN_PHRASES <- c(NOUN_PHRASES, coord_NP)
  }
  #remove unit-final coordinators
  final_coord <- lapply(lapply(NOUN_PHRASES, tail, 1), function(x) x[which( x %in% coordinators)])
  NOUN_PHRASES <- exclude.fcn(NOUN_PHRASES, final_coord)

  #remove < 2 words
  NOUN_PHRASES <- remove_empty.fcn(NOUN_PHRASES)
  return(NOUN_PHRASES)
}
#function to extract verb phrases
vp.fcn <- function(TU, estceque, coordinators, punct) {
  TOKEN <-"TOKEN" #name of the column with the tokens
  POSITION <- "POSITION" #name of columnn with the word index
  DEP_ON <-  "DEP_ON" #name of column with the dependency heads
  DEP_TYPE <-"DEP_TYPE" #name of column with the dependency relations
  POS <- "POS.MELT"  #name of column with the part of speech tags
  LEMMA <- "LEMMA" #name of column with the lemmas
  DEP_ON_POS <- "DEP_ON_POS"
  DEP_ON_DEPTYPE <- "DEP_ON_DEPTYPE"
  DEP_ON_LEMMA <- "DEP_ON_LEMMA"

  #graph of t-unit
  g_tu <- graph_from_data_frame(TU[,c(POSITION, DEP_ON)])


  verbs <-  TU[[POSITION]][which(
    (TU[[POS]] %in% c("V", "VPP", "VINF", "VPR" , "VIMP", "VS")) &
      (! TU[[DEP_TYPE]] %in% c("aux_caus","aux_pass","aux_tps")) )]

  #disregard 'est-ce que'
  if (estceque == TRUE) {
    verbs <- verbs[! sapply(verbs, function(x) any((TU[[DEP_ON]] == x) &
                                                     (TU[[LEMMA]] == "ce") &
                                                     (TU[[DEP_TYPE]] == "suj") &
                                                     (TU[[DEP_ON_LEMMA]] == "être") ))] }

  #remove subjects and verb modifiers
  subjects <- lapply(verbs, function(x) TU[[POSITION]][which(
    (TU[[DEP_TYPE]] == "suj") &
      (TU[[DEP_ON]] == x) &
      #only if the subject occurs before the verb (not inversion)
      (as.numeric(TU[[POSITION]]) < as.numeric(x)) )])
  verb_modifiers <-  lapply(verbs, function(x)
    TU[[POSITION]][which(
      ((TU[[DEP_TYPE]] %in% c("mod", "mod_rel", "dep", "de_obj")) |
         (TU[[POS]]) %in% c("PROREL", "PROWH", "CS", "ADV", "ADVWH"))&
        (TU[[DEP_ON]] == x) &
        (TU[[LEMMA]] != "ne") &
        (as.numeric(TU[[POSITION]]) < as.numeric(x)))  ])
  verb_complements <- mapply(c, subjects, verb_modifiers, SIMPLIFY = FALSE)
  VERB_PHRASES <- extract.fcn(verbs, exclude = verb_complements, g_tu, punct = punct, keep = "exclude")

  #coordinated noun phrases
  coord_VP <- coord_phrases.fcn(TU, g_tu, "(V|VPP|VINF|VPR|VIMP)")
  if (length(coord_VP) != 0) {
    VERB_PHRASES <- c(VERB_PHRASES, coord_VP)
  }
  #remove unit-final coordinators
  final_coord <- lapply(lapply(VERB_PHRASES, tail, 1), function(x) x[which( x %in% coordinators)])
  VERB_PHRASES <- exclude.fcn(VERB_PHRASES, final_coord)

  #delete units that are less than two words in length
  VERB_PHRASES <- remove_empty.fcn(VERB_PHRASES)
  return(VERB_PHRASES)
}
#function to extract syntactic units
extract.fcn <- function(nodes, exclude = NULL, graph, punct, keep = "exclude") {

  #graph: graph of the sentence
  #punct: positions of punctuation to remove
  if (length(nodes) == 0) {return(NULL)}
  units <- lapply(nodes, node_deps.fcn, graph) #get the dependencies off all node words

  if (! is.null(exclude)){
    exclude_deps <- lapply(exclude, function(x) unname(unlist(sapply(x, node_deps.fcn, graph ))))
    if(keep == "include"){
      units <- exclude.fcn(units, exclude_deps, keep = "include")}
    if(keep == "exclude"){
      units <- exclude.fcn(units, exclude_deps, keep = "exclude")
    }
  }
  units <- lapply(units, function(x) x[which(! x %in% punct)])#remove punctuation
  units <- units[which(sapply(units, function(x) length(x)>1))] #delete units that are only one word long
  if(length(units) != 0) {return(units)}
  if(length(units) == 0) {return(NULL)}
}
#function to remove empty segments
remove_empty.fcn <- function(list) {
  if (is.null(list)) {return(NULL)
  }else{list <- list[sapply(list, function(x) length(x) > 1)]
  return(list)}
}
#function to identify citations/quoted speech
citations.fcn <- function(x, POSITION = "POSITION", LEMMA = "LEMMA") {

  if (grepl("(«[^»]+»)|(\"[^\"]+\")|(\'[^\']+\')", paste(x[[LEMMA]], collapse = " "))) {
    positions <- paste(paste(x[[POSITION]], x[[LEMMA]], sep = "_"), collapse = " ")
    sch.ex <- "(\\d+_«[^»]+»)|(\\d+_\"[^\"]+\")|(\\d+_\'[^\']+\')"
    citations <- regmatches(positions, regexpr(sch.ex, positions))
    citations <- lapply(citations, function(x)  unlist(strsplit(gsub("(\\d+)_[^ ]+", "\\1", x), split = " ")))
    return(citations)
  }else{return(NULL)}
}
#function to identify coordinated phrases
coord_phrases.fcn <- function(x, g, pos, POSITION = "POSITION", POS = "POS.MELT") {
  positions <- paste(paste(x[[POSITION]], x[[POS]], sep = "_"), collapse = " ")
  sch.ex <- paste0("\\d+_", pos, " \\d+_CC (\\d+_DET)? \\d+_", pos)
  coord_phrases <- regmatches(positions, regexpr(sch.ex, positions))
  coord_phrases <- lapply(coord_phrases, function(x)  unlist(strsplit(gsub("(\\d+)_[^ ]+", "\\1", x), split = " ")))
  coord_phrases <- lapply(coord_phrases, function(x) x[c(1, length(x))])
  coord_phrases <- lapply(coord_phrases, function(x) c(unlist(sapply(unlist(x), node_deps.fcn, g), use.names = FALSE)))
  return(coord_phrases)
}
#function to return the results
results.fcn <- function (unit, sentence, token = "TOKEN", position = "POSITION") {

  if (is.null(unit)) {return(
    list("NUMBER" = 0,
         "LENGTH" = NA,
         "TOKENS" = NA)
  )}
  return(list(
    "NUMBER" = length(unit),
    "LENGTHS" = sapply(unit, function(x) length(x)),
    "TOKENS" = lapply(unit, function(x) sentence[[token]][which(sentence[[position]] %in% x)])))
}
#function to use names of variables as the list element name
named.list.fcn <- function(...) {

  l <- list(...)
  names(l) <- as.character( match.call()[-1] )
  l
}

#function to split a segment at a given place
splitat.fcn <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

#' Generate a list of syntactic units
#'
#' @param sentence A data frame which contains a sentence in CONLL format


synt.units.fcn <- function(sentence){
#function to extract syntactic units from dependency parsed sentence in CONLL format

##Prepare dataframe####
    #add column names as  variables
    #Note: this also needs to be done within the np.fcn and vp.fcn in helper_syntactic.R)
    TOKEN <-"TOKEN" #name of the column with the tokens
    POSITION <- "POSITION" #name of columnn with the word index
    DEP_ON <-  "DEP_ON" #name of column with the dependency heads
    DEP_TYPE <-"DEP_TYPE" #name of column with the dependency relations
    POS <- "POS.MELT"  #name of column with the part of speech tags
    LEMMA <- "LEMMA" #name of column with the lemmas
    #add a column to the df with the information from dependencies
    sentence <- colref_add.fcn(sentence, DEP_ON , "DEP_ON_POS", POS)
    sentence <- colref_add.fcn(sentence, DEP_ON , "DEP_ON_DEPTYPE", DEP_TYPE)
    sentence <- colref_add.fcn(sentence, DEP_ON , "DEP_ON_LEMMA", LEMMA)
    DEP_ON_POS <- "DEP_ON_POS"
    DEP_ON_DEPTYPE <- "DEP_ON_DEPTYPE"
    DEP_ON_LEMMA <- "DEP_ON_LEMMA"
    #get punctuation
    punct <- as.numeric(sentence[[POSITION]][which(sentence[[POS]] == "PONCT")])
    #get coordinators
    coordinators <- as.numeric(sentence[[POSITION]][which(sentence[[POS]] == "CC")])
    #get citations
    citations <- unlist(citations.fcn(sentence))
    #check if there are direct interogatives
    estceque <- grepl("être ce que", paste(sentence[[LEMMA]], collapse = " "))
    #check for 'il y a' clauses
    ilya <- grepl("il y avoir", paste(sentence[[LEMMA]], collapse = " "))
    #make a graph for the sentence
    g <- graph_from_data_frame(sentence[,c(POSITION, DEP_ON)])

##Sentences####
    #all tokens except punctuation
    SENTENCES <- list(as.numeric(sentence[[POSITION]][which(! sentence[[POSITION]] %in% punct)]))

##Subordinate Clauses####
    #subordinating conjunctions serve as the root nodes
    subordinators <-  sentence[[POSITION]][which(
                                            (sentence[[POS]] %in% c("CS")) &
                                            (!sentence[[DEP_TYPE]] %in% c("root"))  )]
    if (length(subordinators) != 0) {
    #not parce que if it is the root of the sentence
    subordinators <- subordinators [! sapply(subordinators, function (x) any (
                                          (sentence[[POSITION]] == x) &
                                          (sentence[[DEP_ON_DEPTYPE]] == "root") &
                                          (sentence[[DEP_ON_LEMMA]] == "parce"))  )]}
    #est-ce que
    if (estceque == TRUE) {
      subordinators <- subordinators[! sapply(subordinators, function(x) any((sentence[[POSITION]] == x) &
                                                       (sentence[[DEP_TYPE]] == "obj") &
                                                       (sentence[[DEP_ON_LEMMA]] == "être") ))] }
    SUB_CLAUSES <- extract.fcn(subordinators, exclude = NULL, g, punct)

    #il y a clauses
    if (ilya == TRUE) {
      ilya_node <-sentence[[DEP_ON]][which(
                                              (sentence[[LEMMA]] == c("y")) &
                                              (sentence[[DEP_TYPE]] == c("aff")) &
                                              (sentence[[DEP_ON_LEMMA]] == "avoir"))]

      ilya_node <- ilya_node[sapply(ilya_node, function(x) any(
                              #is there another finite verb dependant on the il y a clause
                              ((sentence[[DEP_ON]] == x) &
                              (sentence[[POS]] %in% c("V", "VPP", "VIMP", "VS"))) |
                              #or is the il y a clause dependent on another finite verb itself
                              ((sentence[[POSITION]] == x) &
                                (sentence[[DEP_ON_POS]] %in% c("V", "VPP", "VIMP", "VS")  )) ))]
      if (length(ilya_node) == 0) {ILYA_CLAUSES <- NULL
      }else{
      ilya_rest <- lapply(ilya_node, function(x) sentence[[POSITION]][which(
                                              (sentence[[DEP_ON]] == x) &
                                              (sentence[[DEP_TYPE]] %in% c("mod", "obj")) )])
      ILYA_CLAUSES <- extract.fcn(ilya_node, exclude = ilya_rest, g, punct)
      SUB_CLAUSES <- c(SUB_CLAUSES, ILYA_CLAUSES)}}

    #citations
    sub_cit <- lapply(citations.fcn(sentence), function(x) x[which(! x %in% punct)])
    if(length(sub_cit) != 0) {SUB_CLAUSES <- c(SUB_CLAUSES, sub_cit)}

    #exclude clauses that are missing a subject
    if (! is.null(SUB_CLAUSES)) {
      subjects <- lapply(SUB_CLAUSES, function(x) sentence[[POSITION]][which(
        (sentence[[DEP_TYPE]] == "suj") &
        (sentence[[POSITION]] %in% x))])
      SUB_CLAUSES <- exclude.fcn(SUB_CLAUSES, subjects, keep = "check")
      if (! is.null(SUB_CLAUSES)){
      finverbs <- lapply(SUB_CLAUSES, function(x) sentence[[POSITION]][which(
        (sentence[[POS]] %in%  c("V", "VPP", "VIMP", "VS")) &
          (sentence[[POSITION]] %in% x))])
      SUB_CLAUSES <- exclude.fcn(SUB_CLAUSES, finverbs, keep = "check")}
      }
      subjects <- NULL


##Relative Clauses####

    #pronouns and relative pronouns serve as the root nodes
    relativizers <-  sentence[[POSITION]][which(
        (sentence[[POS]] %in% c("PRO", "PROREL")) &
        (!sentence[[DEP_ON_POS]] %in% c("PRO"))
          )]

    #as well as modified nouns
    noun_rel <- sentence[[POSITION]][which(
      (sentence[[DEP_TYPE]] %in% c("mod", "mod_rel", "dep")) &
      (sentence[[DEP_ON_POS]] %in% c("NC", "NPP")) &
        #only finite verbs
      (sentence[[POS]] %in% c("V", "VIMP", "VS", "VPP") )  )]


    relativizers <- c(relativizers, noun_rel)
    REL_CLAUSES <- extract.fcn(relativizers, exclude = NULL, g, punct)

    #exclude clauses that are missing a subject
    if (! is.null(REL_CLAUSES)) {
      subjects <- lapply(REL_CLAUSES, function(x)
        sentence[[POSITION]][which((sentence[[DEP_TYPE]] == "suj") &
                                  (sentence[[POSITION]] %in% x))])
      REL_CLAUSES <- exclude.fcn(REL_CLAUSES, subjects, keep = "check")

    #exclude coordinators
    if (! is.null(REL_CLAUSES)) {
      rel_coord <-  lapply(REL_CLAUSES, function(x) sentence[[DEP_ON]][which(
        (sentence[[DEP_TYPE]] %in% c("coord")) &
          (sentence[[POSITION]] %in% x) )])
      if (length(rel_coord) == length(relativizers)) {
      rel_coord <- mapply(function(x,y) x[which(x %in% y)], rel_coord, relativizers)
      rel_coord <-  lapply(rel_coord, function(x) sentence[[POSITION]][which(
        (sentence[[DEP_TYPE]] %in% c("coord")) &
          (sentence[[DEP_ON]] == x) )])
      REL_CLAUSES <- exclude.fcn(REL_CLAUSES, rel_coord)}
        }
      }

##Dependent Clauses####
    #dependent clauses are subordinate clauses + relative clauses
    if ((is.null(SUB_CLAUSES)) & (is.null(REL_CLAUSES)))
          {DEP_CLAUSES <- NULL
    }else if ((is.null(SUB_CLAUSES)) & (! is.null(REL_CLAUSES)))
          {DEP_CLAUSES <- REL_CLAUSES
    }else if ((!is.null(SUB_CLAUSES)) & (is.null(REL_CLAUSES)))
          {DEP_CLAUSES <- SUB_CLAUSES
    }else if ((!is.null(SUB_CLAUSES)) & (!is.null(REL_CLAUSES)))
          {DEP_CLAUSES <- c(SUB_CLAUSES, REL_CLAUSES)}

##Coordinated Clauses####
    ##finite verbs
    finverbs <-  sentence[[POSITION]][which(
      (sentence[[POS]] %in% c("V", "VPP", "VIMP", "VS")) &
        #not auxilliaries
        (! sentence[[DEP_TYPE]] %in% c("aux_pass","aux_tps", "ato")) &
        #not itself dependent on a relative pronoun or subordinating conjunction
        (! sentence[[DEP_ON_POS]] %in% c("PROREL", "PROWH", "CS")) &
        (! sentence[[POSITION]] %in% citations) )]

      if (length(finverbs) != 0 ){#must have a subject
      finverbs <- finverbs[sapply(finverbs, function(x) any((sentence[[DEP_ON]] == x) &
                                                              (sentence[[DEP_TYPE]] %in% c("suj")) ))]}
      if(length(finverbs) != 0){#must not have a relative pronoun as a daughter
      finverbs <- finverbs[! sapply(finverbs, function(x) any((sentence[[DEP_ON]] == x) &
                                                                (sentence[[POS]] %in% c("PROREL", "PROWH")) ))]}
      if(length(finverbs) != 0){#! relative pronoun < preposition < verb (ex: 'sans que')
      prepositions <- lapply(finverbs, function(x) sentence[[POSITION]][which(
                                                                (sentence[[DEP_ON]] == x) &
                                                                (sentence[[POS]] %in% c("P")) )])
      if (length(prepositions) != 0){

        prep_rel <- sapply(prepositions, function(x) sentence[[POSITION]][which(
                                                                (sentence[[DEP_ON]] %in% x) &
                                                               (sentence[[POS]] %in% c("PROREL", "PROWH")) )])
        finverbs <- finverbs[!sapply(prep_rel, length) > 0 ]}
      }

    #disregard il y a clauses
    if (ilya == TRUE) {
      finverbs <- finverbs[! sapply(finverbs, function(x) any((sentence[[DEP_ON]] == x) &
                                                       (sentence[[LEMMA]] == "y") &
                                                       (sentence[[DEP_TYPE]] == "aff") &
                                                       (sentence[[DEP_ON_LEMMA]] == "avoir") ))] }


    #if there are less than 2 finite verbs, the sentence has no coordinated clauses
      if (length(finverbs) < 2) {CO_CLAUSES <- NULL
      }else{

    #sentences that are split with a colon (parataxis)
    colon <- as.numeric(sentence[[POSITION]][which(
        sentence[[LEMMA]] == ":")])
          if (length(colon) != 0) {
            juxtapositions <- splitat.fcn(sentence[[POSITION]], colon)
          if (length(juxtapositions) == length(finverbs)) {
            CO_CLAUSES <- lapply(juxtapositions, function(x)x[which(! x %in% punct)])}
              }else{juxtapositions <- NULL}

      #if the number of finite verbs != the number of juxtaposed sentences,
      #then use finite verbs as the root nodes
      if (length(finverbs) != length(juxtapositions)) {
        CO_CLAUSES <- extract.fcn(finverbs, exclude = NULL, g, punct)
          #which clauses are contained within other clauses
          subset <- sapply(seq(length(CO_CLAUSES)), function(x) all(CO_CLAUSES[[x]] %in% unlist(CO_CLAUSES[-x])))
            if (any(subset == TRUE)) {
              for (i in seq(length(CO_CLAUSES))) {
                if(subset[i] == FALSE) {
                  CO_CLAUSES[[i]] <- unlist(sapply(CO_CLAUSES[[i]], function(x) x[which(
                    ! x %in% unlist(CO_CLAUSES[subset]))]))}
                   }
        #reorder the clauses based on the position of the first word (same order as sentence)
        CO_CLAUSES <- CO_CLAUSES[order(sapply(CO_CLAUSES, `[[`, 1))] }
        #if the coordinator is in the final position of a clause, move it to the first position of the following clause
        coords <- lapply(CO_CLAUSES, function(x) sentence[[POSITION]][which(#which coordinators in the last position
                                                      (sentence[[POSITION]] %in% tail(x, 1)) &
                                                      (sentence[[POS]] %in% c("CC", "P")) )])
                  #move coordinators to first position of following clause
                  for (i in seq(length(CO_CLAUSES)-1)) {
                  if (length(coords[[i]]) != 0)
                    {CO_CLAUSES[[i]] <- CO_CLAUSES[[i]][which(! CO_CLAUSES[[i]] %in% coords[[i]])]
                    CO_CLAUSES[[i+1]] <- as.numeric(c(coords[[i]], CO_CLAUSES[[i+1]]))}
                  }
        }
      }

##T-units####
    #finite verbs serve as the root nodes
    finverbs <-  sentence[[POSITION]][which(
        (sentence[[POS]] %in% c("V", "VPP", "VIMP")) &
        (! sentence[[DEP_TYPE]] %in% c("aux_caus","aux_pass","aux_tps")) )]
    #if there are no coordinated clauses
    if (length(CO_CLAUSES) == 0) {
      #and there are no finite verbs, then there are no T-units
      if (length(unlist(finverbs)) == 0) {
          T_UNITS <- NULL
        #if there are no coordinated clauses but there is a finite verb, the whole sentence is the t-unit
        }else {T_UNITS <- list(as.numeric(sentence[[POSITION]][which(! sentence[[POSITION]] %in% punct)]))}
      #otherwise, the t-units are the coordinated clauses
      }else {T_UNITS <- CO_CLAUSES}


##Clauses####
    #clauses are the t-units + dependent clauses
    CLAUSES <- c(T_UNITS, DEP_CLAUSES)


##Noun Phrases####
    #Noun phrases are extracted from within t-units
    if (length(T_UNITS) > 1) {TUs <- lapply(T_UNITS, function(x) sentence[unlist(as.numeric(x)),])
    }else{TUs <- list(sentence)}

    NOUN_PHRASES <- purrr::flatten(lapply(TUs, function(x)np.fcn(x,
                                                                 coordinators = coordinators,
                                                                 punct = punct) ))

##Verb Phrases####

    #Verb phrases are extracted from within t-units
    VERB_PHRASES <- purrr::flatten(lapply(TUs, function(x)vp.fcn(x, estceque = estceque,
                                                                 coordinators = coordinators,
                                                                 punct = punct) ))


##Results####
    units <- named.list.fcn(SENTENCES, CLAUSES, DEP_CLAUSES, CO_CLAUSES, T_UNITS, NOUN_PHRASES, VERB_PHRASES)
    synt.units <- lapply(units, results.fcn, sentence)

  return(synt.units)
}




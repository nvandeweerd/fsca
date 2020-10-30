#synt.units.fcn(): An R function to extract syntactic units from L2 French texts

# Helper functions ####


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
  current.df <- utils::read.table(text = df$text, #separate into columns
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
      igraph::V( #get the vertices
        igraph::induced.subgraph( #of the subgraph
          graph,igraph::subcomponent(graph, x, mode = "in"))))) #which x is the main node
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
np.fcn <- function(TU, coordinators, punct,
                   colnames = c(TOKEN = "TOKEN",
                                POSITION = "POSITION",
                                DEP_ON = "DEP_ON",
                                DEP_TYPE = "DEP_TYPE",
                                POS = "POS.MELT",
                                LEMMA = "LEMMA",
                                DEP_ON_POS = "DEP_ON_POS",
                                DEP_ON_DEPTYPE = "DEP_ON_DEPTYPE",
                                DEP_ON_LEMMA = "DEP_ON_LEMMA")) {

  TOKEN <- colnames["TOKEN"]
  POSITION <- colnames["POSITION"]
  DEP_ON <-  colnames["DEP_ON"]
  DEP_TYPE <- colnames["DEP_TYPE"]
  POS <- colnames["POS"]
  LEMMA <- colnames["LEMMA"]
  DEP_ON_POS <- colnames["DEP_ON_POS"]
  DEP_ON_DEPTYPE <- colnames["DEP_ON_DEPTYPE"]
  DEP_ON_LEMMA <- colnames["DEP_ON_LEMMA"]


  #graph of t-unit
  g_tu <- igraph::graph_from_data_frame(TU[,c(POSITION, DEP_ON)])
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
  final_coord <- lapply(lapply(NOUN_PHRASES, utils::tail, 1), function(x) x[which( x %in% coordinators)])
  NOUN_PHRASES <- exclude.fcn(NOUN_PHRASES, final_coord)

  #remove < 2 words
  NOUN_PHRASES <- remove_empty.fcn(NOUN_PHRASES)
  return(NOUN_PHRASES)
}
#function to extract verb phrases
vp.fcn <- function(TU, estceque, coordinators, punct,
                   colnames = c(TOKEN = "TOKEN",
                                POSITION = "POSITION",
                                DEP_ON = "DEP_ON",
                                DEP_TYPE = "DEP_TYPE",
                                POS = "POS.MELT",
                                LEMMA = "LEMMA",
                                DEP_ON_POS = "DEP_ON_POS",
                                DEP_ON_DEPTYPE = "DEP_ON_DEPTYPE",
                                DEP_ON_LEMMA = "DEP_ON_LEMMA")) {
  TOKEN <- colnames["TOKEN"]
  POSITION <- colnames["POSITION"]
  DEP_ON <-  colnames["DEP_ON"]
  DEP_TYPE <- colnames["DEP_TYPE"]
  POS <- colnames["POS"]
  LEMMA <- colnames["LEMMA"]
  DEP_ON_POS <- colnames["DEP_ON_POS"]
  DEP_ON_DEPTYPE <- colnames["DEP_ON_DEPTYPE"]
  DEP_ON_LEMMA <- colnames["DEP_ON_LEMMA"]

  #graph of t-unit
  g_tu <- igraph::graph_from_data_frame(TU[,c(POSITION, DEP_ON)])


  verbs <-  TU[[POSITION]][which(
    (TU[[POS]] %in% c("V", "VPP", "VINF", "VPR" , "VIMP", "VS")) &
      (! TU[[DEP_TYPE]] %in% c("aux_caus","aux_pass","aux_tps")) )]

  #disregard 'est-ce que'
  if (estceque == TRUE) {
    verbs <- verbs[! sapply(verbs, function(x) any((TU[[DEP_ON]] == x) &
                                                     (TU[[LEMMA]] == "ce") &
                                                     (TU[[DEP_TYPE]] == "suj") &
                                                     (TU[[DEP_ON_LEMMA]] == "\u00EAtre") ))] }

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
  final_coord <- lapply(lapply(VERB_PHRASES, utils::tail, 1), function(x) x[which( x %in% coordinators)])
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

  if (grepl("(\u00AB[^\u00BB]+\u00BB)|(\"[^\"]+\")|(\'[^\']+\')", paste(x[[LEMMA]], collapse = " "))) {
    positions <- paste(paste(x[[POSITION]], x[[LEMMA]], sep = "_"), collapse = " ")
    sch.ex <- "(\\d+_\u00AB[^\u00BB]+\u00BB)|(\\d+_\"[^\"]+\")|(\\d+_\'[^\']+\')"
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


# Main functions ####

#' Generate a List of Syntactic Units
#'
#' This function returns a list of syntactic units (sentences, t-units, clauses
#' dependent clauses, coordinated clauses, verb phrases and noun phrases) from
#' a dependency parsed sentence in CONLL format.
#' @usage
#' synt.units.fcn(df, colnames = c(TOKEN = "TOKEN", POSITION = "POSITION",
#' DEP_ON = "DEP_ON", DEP_TYPE = "DEP_TYPE", POS = "POS.MELT", LEMMA = "LEMMA"))
#'
#' @param df A data frame which contains a sentence in CONLL format
#' @param colnames A named vector containing the names of the following columns:
#'
#' - TOKEN: name of the column with the tokens
#' - POSITION: name of column with the word index
#' - DEP_ON: name of column with the dependency heads
#' - DEP_TYPE: name of column with the dependency relations
#' - POS: name of column with the part of speech tags
#' - LEMMA: name of column with the lemmas

#' @return The output is a list containing, for each  syntactic
#' unit:
#'
#' - the NUMBER identified units (integer)
#' - the LENGTHS of units (integer vector)
#' - the TOKENS belonging to each unit (list of character vectors)
#'
#' @example R/examples/example01.R
#' @export

synt.units.fcn <- function(df,
                           colnames = c(TOKEN = "TOKEN",
                                POSITION = "POSITION",
                                DEP_ON = "DEP_ON",
                                DEP_TYPE = "DEP_TYPE",
                                POS = "POS.MELT",
                                LEMMA = "LEMMA")){

##Prepare dataframe####
    #add column names as variables
    TOKEN <- colnames["TOKEN"]
    POSITION <- colnames["POSITION"]
    DEP_ON <-  colnames["DEP_ON"]
    DEP_TYPE <- colnames["DEP_TYPE"]
    POS <- colnames["POS"]
    LEMMA <- colnames["LEMMA"]
    #add a column to the df with the information from dependencies
    df <- colref_add.fcn(df, DEP_ON , "DEP_ON_POS", POS)
    df <- colref_add.fcn(df, DEP_ON , "DEP_ON_DEPTYPE", DEP_TYPE)
    df <- colref_add.fcn(df, DEP_ON , "DEP_ON_LEMMA", LEMMA)
    colnames <- c(colnames, DEP_ON_POS = "DEP_ON_POS",
                  DEP_ON_DEPTYPE = "DEP_ON_DEPTYPE",
                  DEP_ON_LEMMA = "DEP_ON_LEMMA")
    DEP_ON_POS <- colnames["DEP_ON_POS"]
    DEP_ON_DEPTYPE <- colnames["DEP_ON_DEPTYPE"]
    DEP_ON_LEMMA <- colnames["DEP_ON_LEMMA"]

    #get punctuation
    punct <- as.numeric(df[[POSITION]][which(df[[POS]] == "PONCT")])
    #get coordinators
    coordinators <- as.numeric(df[[POSITION]][which(df[[POS]] == "CC")])
    #get citations
    citations <- unlist(citations.fcn(df))
    #check if there are direct interogatives
    estceque <- grepl("\u00EAtre ce que", paste(df[[LEMMA]], collapse = " "))
    #check for 'il y a' clauses
    ilya <- grepl("il y avoir", paste(df[[LEMMA]], collapse = " "))
    #make a graph for the df
    g <- igraph::graph_from_data_frame(df[,c(POSITION, DEP_ON)])

##Sentences####
    #all tokens except punctuation
    SENTENCES <- list(as.numeric(df[[POSITION]][which(! df[[POSITION]] %in% punct)]))

##Subordinate Clauses####
    #subordinating conjunctions serve as the root nodes
    subordinators <-  df[[POSITION]][which(
                                            (df[[POS]] %in% c("CS")) &
                                            (!df[[DEP_TYPE]] %in% c("root"))  )]
    if (length(subordinators) != 0) {
    #not parce que if it is the root of the df
    subordinators <- subordinators [! sapply(subordinators, function (x) any (
                                          (df[[POSITION]] == x) &
                                          (df[[DEP_ON_DEPTYPE]] == "root") &
                                          (df[[DEP_ON_LEMMA]] == "parce"))  )]}
    #est-ce que
    if (estceque == TRUE) {
      subordinators <- subordinators[! sapply(subordinators, function(x) any((df[[POSITION]] == x) &
                                                       (df[[DEP_TYPE]] == "obj") &
                                                       (df[[DEP_ON_LEMMA]] == "\u00EAtre") ))] }
    SUB_CLAUSES <- extract.fcn(subordinators, exclude = NULL, g, punct)

    #il y a clauses
    if (ilya == TRUE) {
      ilya_node <-df[[DEP_ON]][which(
                                              (df[[LEMMA]] == c("y")) &
                                              (df[[DEP_TYPE]] == c("aff")) &
                                              (df[[DEP_ON_LEMMA]] == "avoir"))]

      ilya_node <- ilya_node[sapply(ilya_node, function(x) any(
                              #is there another finite verb dependant on the il y a clause
                              ((df[[DEP_ON]] == x) &
                              (df[[POS]] %in% c("V", "VPP", "VIMP", "VS"))) |
                              #or is the il y a clause dependent on another finite verb itself
                              ((df[[POSITION]] == x) &
                                (df[[DEP_ON_POS]] %in% c("V", "VPP", "VIMP", "VS")  )) ))]
      if (length(ilya_node) == 0) {ILYA_CLAUSES <- NULL
      }else{
      ilya_rest <- lapply(ilya_node, function(x) df[[POSITION]][which(
                                              (df[[DEP_ON]] == x) &
                                              (df[[DEP_TYPE]] %in% c("mod", "obj")) )])
      ILYA_CLAUSES <- extract.fcn(ilya_node, exclude = ilya_rest, g, punct)
      SUB_CLAUSES <- c(SUB_CLAUSES, ILYA_CLAUSES)}}

    #citations
    sub_cit <- lapply(citations.fcn(df), function(x) x[which(! x %in% punct)])
    if(length(sub_cit) != 0) {SUB_CLAUSES <- c(SUB_CLAUSES, sub_cit)}

    #exclude clauses that are missing a subject
    if (! is.null(SUB_CLAUSES)) {
      subjects <- lapply(SUB_CLAUSES, function(x) df[[POSITION]][which(
        (df[[DEP_TYPE]] == "suj") &
        (df[[POSITION]] %in% x))])
      SUB_CLAUSES <- exclude.fcn(SUB_CLAUSES, subjects, keep = "check")
      if (! is.null(SUB_CLAUSES)){
      finverbs <- lapply(SUB_CLAUSES, function(x) df[[POSITION]][which(
        (df[[POS]] %in%  c("V", "VPP", "VIMP", "VS")) &
          (df[[POSITION]] %in% x))])
      SUB_CLAUSES <- exclude.fcn(SUB_CLAUSES, finverbs, keep = "check")}
      }
      subjects <- NULL


##Relative Clauses####

    #pronouns and relative pronouns serve as the root nodes
    relativizers <-  df[[POSITION]][which(
        (df[[POS]] %in% c("PRO", "PROREL")) &
        (!df[[DEP_ON_POS]] %in% c("PRO"))
          )]

    #as well as modified nouns
    noun_rel <- df[[POSITION]][which(
      (df[[DEP_TYPE]] %in% c("mod", "mod_rel", "dep")) &
      (df[[DEP_ON_POS]] %in% c("NC", "NPP")) &
        #only finite verbs
      (df[[POS]] %in% c("V", "VIMP", "VS", "VPP") )  )]


    relativizers <- c(relativizers, noun_rel)
    REL_CLAUSES <- extract.fcn(relativizers, exclude = NULL, g, punct)

    #exclude clauses that are missing a subject
    if (! is.null(REL_CLAUSES)) {
      subjects <- lapply(REL_CLAUSES, function(x)
        df[[POSITION]][which((df[[DEP_TYPE]] == "suj") &
                                  (df[[POSITION]] %in% x))])
      REL_CLAUSES <- exclude.fcn(REL_CLAUSES, subjects, keep = "check")

    #exclude coordinators
    if (! is.null(REL_CLAUSES)) {
      rel_coord <-  lapply(REL_CLAUSES, function(x) df[[DEP_ON]][which(
        (df[[DEP_TYPE]] %in% c("coord")) &
          (df[[POSITION]] %in% x) )])
      if (length(rel_coord) == length(relativizers)) {
      rel_coord <- mapply(function(x,y) x[which(x %in% y)], rel_coord, relativizers)
      rel_coord <-  lapply(rel_coord, function(x) df[[POSITION]][which(
        (df[[DEP_TYPE]] %in% c("coord")) &
          (df[[DEP_ON]] == x) )])
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
    finverbs <-  df[[POSITION]][which(
      (df[[POS]] %in% c("V", "VPP", "VIMP", "VS")) &
        #not auxilliaries
        (! df[[DEP_TYPE]] %in% c("aux_pass","aux_tps", "ato")) &
        #not itself dependent on a relative pronoun or subordinating conjunction
        (! df[[DEP_ON_POS]] %in% c("PROREL", "PROWH", "CS")) &
        (! df[[POSITION]] %in% citations) )]

      if (length(finverbs) != 0 ){#must have a subject
      finverbs <- finverbs[sapply(finverbs, function(x) any((df[[DEP_ON]] == x) &
                                                              (df[[DEP_TYPE]] %in% c("suj")) ))]}
      if(length(finverbs) != 0){#must not have a relative pronoun as a daughter
      finverbs <- finverbs[! sapply(finverbs, function(x) any((df[[DEP_ON]] == x) &
                                                                (df[[POS]] %in% c("PROREL", "PROWH")) ))]}
      if(length(finverbs) != 0){#! relative pronoun < preposition < verb (ex: 'sans que')
      prepositions <- lapply(finverbs, function(x) df[[POSITION]][which(
                                                                (df[[DEP_ON]] == x) &
                                                                (df[[POS]] %in% c("P")) )])
      if (length(prepositions) != 0){

        prep_rel <- sapply(prepositions, function(x) df[[POSITION]][which(
                                                                (df[[DEP_ON]] %in% x) &
                                                               (df[[POS]] %in% c("PROREL", "PROWH")) )])
        finverbs <- finverbs[!sapply(prep_rel, length) > 0 ]}
      }

    #disregard il y a clauses
    if (ilya == TRUE) {
      finverbs <- finverbs[! sapply(finverbs, function(x) any((df[[DEP_ON]] == x) &
                                                       (df[[LEMMA]] == "y") &
                                                       (df[[DEP_TYPE]] == "aff") &
                                                       (df[[DEP_ON_LEMMA]] == "avoir") ))] }


    #if there are less than 2 finite verbs, the df has no coordinated clauses
      if (length(finverbs) < 2) {CO_CLAUSES <- NULL
      }else{

    #dfs that are split with a colon (parataxis)
    colon <- as.numeric(df[[POSITION]][which(
        df[[LEMMA]] == ":")])
          if (length(colon) != 0) {
            juxtapositions <- splitat.fcn(df[[POSITION]], colon)
          if (length(juxtapositions) == length(finverbs)) {
            CO_CLAUSES <- lapply(juxtapositions, function(x)x[which(! x %in% punct)])}
              }else{juxtapositions <- NULL}

      #if the number of finite verbs != the number of juxtaposed dfs,
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
        #reorder the clauses based on the position of the first word (same order as df)
        CO_CLAUSES <- CO_CLAUSES[order(sapply(CO_CLAUSES, `[[`, 1))] }
        #if the coordinator is in the final position of a clause, move it to the first position of the following clause
        coords <- lapply(CO_CLAUSES, function(x) df[[POSITION]][which(#which coordinators in the last position
                                                      (df[[POSITION]] %in% utils::tail(x, 1)) &
                                                      (df[[POS]] %in% c("CC", "P")) )])
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
    finverbs <-  df[[POSITION]][which(
        (df[[POS]] %in% c("V", "VPP", "VIMP")) &
        (! df[[DEP_TYPE]] %in% c("aux_caus","aux_pass","aux_tps")) )]
    #if there are no coordinated clauses
    if (length(CO_CLAUSES) == 0) {
      #and there are no finite verbs, then there are no T-units
      if (length(unlist(finverbs)) == 0) {
          T_UNITS <- NULL
        #if there are no coordinated clauses but there is a finite verb, the whole df is the t-unit
        }else {T_UNITS <- list(as.numeric(df[[POSITION]][which(! df[[POSITION]] %in% punct)]))}
      #otherwise, the t-units are the coordinated clauses
      }else {T_UNITS <- CO_CLAUSES}


##Clauses####
    #clauses are the t-units + dependent clauses
    CLAUSES <- c(T_UNITS, DEP_CLAUSES)


##Noun Phrases####
    #Noun phrases are extracted from within t-units
    if (length(T_UNITS) > 1) {TUs <- lapply(T_UNITS, function(x) df[unlist(as.numeric(x)),])
    }else{TUs <- list(df)}

    NOUN_PHRASES <- purrr::flatten(lapply(TUs, function(x)np.fcn(x,
                                                                 coordinators = coordinators,
                                                                 punct = punct, colnames) ))

##Verb Phrases####

    #Verb phrases are extracted from within t-units
    VERB_PHRASES <- purrr::flatten(lapply(TUs, function(x)vp.fcn(x, estceque = estceque,
                                                                 coordinators = coordinators,
                                                                 punct = punct) ))


##Results####
    units <- named.list.fcn(SENTENCES, CLAUSES, DEP_CLAUSES, CO_CLAUSES, T_UNITS, NOUN_PHRASES, VERB_PHRASES)
    synt.units <- lapply(units, results.fcn, df)

  return(synt.units)
}

#' Extract elements from the list of all syntactc units
#'
#' This function is a wrapper function for `synt.units.fcn`
#' @usage
#' getUnits(df, colnames = c(TOKEN = "TOKEN", POSITION = "POSITION", DEP_ON =
#'  "DEP_ON", DEP_TYPE = "DEP_TYPE", POS = "POS.MELT", LEMMA = "LEMMA"),
#'  what = "all", units = c("SENTENCES", "CLAUSES", "CO_CLAUSES", "DEP_CLAUSES",
#'  "T_UNITS", "NOUN_PHRASES", "VERB_PHRASES"), paste.tokens = FALSE)
#'
#' @param df a data frame which contains a sentence in CONLL format
#' @param colnames a named vector containing the names of the following columns:
#' - TOKEN: name of the column with the tokens
#' - POSITION: name of column with the word index
#' - DEP_ON: name of column with the dependency heads
#' - DEP_TYPE: name of column with the dependency relations
#' - POS: name of column with the part of speech tags
#' - LEMMA: name of column with the lemmas
#' @param what one of the following character strings:
#' - "number": returns the number identified units (integer)
#' - "lengths": returns the lengths of units (integer vector)
#' - "tokens": returns the tokens belonging to each unit (list of character vectors)
#' @param units a vector containing the units to be extracted:
#' - "SENTENCES": returns sentences
#' - "CLAUSES": returns all clauses
#' - "DEP_CLAUSES": returns all dependent clauses
#' - "CO_CLAUSES": returns all coordinated clauses
#' - "NOUN_PHRASES": returns all noun phrases
#' - "VERB_PHRASES": returns all verb phrases
#' @param paste.tokens a logical value indicating whether tokens be pasted
#' together

#' @return
#' If `what` is "all" (the default) then the full output of `synt.units.fcn`
#' will be returned as a list.
#'
#' If `what` is "number" then the number of identified units will be returned as
#' a named vector.
#'
#' If `what` is "lengths" then the lengths of the identified units will be
#' returned as a list of integer vectors.
#'
#' If `what` is "tokens" then the tokens belonging to each unit will be returned
#' as a list of character vectors.

#' @example R/examples/example02.R
#'
#' @export

getUnits <- function(df, colnames = c(TOKEN = "TOKEN",
                                      POSITION = "POSITION",
                                      DEP_ON = "DEP_ON",
                                      DEP_TYPE = "DEP_TYPE",
                                      POS = "POS.MELT",
                                      LEMMA = "LEMMA"),
                     what = "all",
                     units = c("SENTENCES",
                               "CLAUSES",
                               "CO_CLAUSES",
                               "DEP_CLAUSES",
                               "T_UNITS",
                               "NOUN_PHRASES",
                               "VERB_PHRASES"),
                     paste.tokens = FALSE){
    list <- synt.units.fcn(df, colnames)
      if(what == "all"){x <- list
      if(paste.tokens == TRUE){
        for (i in seq(length(x))){
          x[[i]][[3]] <-  purrr::map(x[[i]][[3]] , paste, collapse = " ")
        }
      }
      }
      if(what == "number") {x <- sapply(list, `[[`, 1)[units]}
      if(what == "lengths") {x <- lapply(list, `[[`, 2)[units]}
      if(what == "tokens") {x <- lapply(list, `[[`, 3)[units]
      if(paste.tokens == TRUE) {
        x <- lapply(x, function(x) purrr::map(x, paste, collapse = " "))
      }}
    return(x)
}

#' Calculate syntactic measures
#'
#' @usage
#' getMeasures(input, colnames = c(TOKEN = "TOKEN", POSITION = "POSITION", DEP_ON =
#'  "DEP_ON", DEP_TYPE = "DEP_TYPE", POS = "POS.MELT", LEMMA = "LEMMA"), round.to = 2)
#'
#' @param input a data frame which contains a sentence in CONLL format or a list
#' of data frames in CONLL format (see \code{\link{getUnits}})
#' @param colnames a named vector containing the names of the following columns:
#' - TOKEN: name of the column with the tokens
#' - POSITION: name of column with the word index
#' - DEP_ON: name of column with the dependency heads
#' - DEP_TYPE: name of column with the dependency relations
#' - POS: name of column with the part of speech tags
#' - LEMMA: name of column with the lemmas
#' @param round.to integer for rounding measures

#' @return
#' a list of syntactic complexity measures:
#' - MLS: mean length of sentence
#' - DIVS: standard deviation of sentence length
#' - T_S: t-units per sentence
#' - MLT: mean length of t-unit
#' - DIVT: standard deviation of t-unit length
#' - C_T: clauses per sentence
#' - MLC: mean length of clause
#' - DIVC: standard deviation of clause length
#' - MLNP: mean length of noun phrases
#' - DIVNP: standard deviation of noun phrase length
#' - NP_C: noun prases per clause

#' @examples
#' data(example)
#' getMeasures(example, round.to = 2)

#' @export

getMeasures <- function(input, colnames = c(
                          TOKEN = "TOKEN",
                          POSITION = "POSITION",
                          DEP_ON = "DEP_ON",
                          DEP_TYPE = "DEP_TYPE",
                          POS = "POS.MELT",
                          LEMMA = "LEMMA"
                        ), round.to = 2) {
  if (is.data.frame(input)) {
    input <- list(input)
  }
  n <- apply(do.call("rbind", lapply(input, getUnits, colnames = colnames, what = "number")), 2, sum)

  len <- lapply(input, getUnits, colnames = colnames, what = "lengths")
  means <- rep(NA, length(n))
  names(means) <- names(n)
  for (i in seq(length(n))) {
    means[i] <- mean(unlist(sapply(len, function(x) (x[[i]])), use.names = FALSE), na.rm = TRUE)
  }
  sds <- rep(NA, length(n))
  names(sds) <- names(n)
  for (i in seq(length(n))) {
    sds[i] <- stats::sd(unlist(sapply(len, function(x) (x[[i]])), use.names = FALSE), na.rm = TRUE)
  }

  df <- data.frame(t(do.call("rbind", list("n" = n, "mean.len" = means, "sd.len" = sds))))

  measures <- list(
    "MLS" = df["SENTENCES", "mean.len"],
    "DIVS" = df["SENTENCES", "sd.len"],
    "T_S" = df["T_UNITS", "n"] / df["SENTENCES", "n"],
    "MLT" = df["T_UNITS", "mean.len"],
    "DIVT" = df["T_UNITS", "sd.len"],
    "C_T" = df["CLAUSES", "n"] / df["T_UNITS", "n"],
    "MLC" = df["CLAUSES", "mean.len"],
    "DIVC" = df["CLAUSES", "sd.len"],
    "MLNP" = df["NOUN_PHRASES", "mean.len"],
    "DIVNP" = df["NOUN_PHRASES", "sd.len"],
    "NP_C" = df["NOUN_PHRASES", "n"] / df["CLAUSES", "n"]
  )

  measures <- lapply(measures, round, round.to)

  return(measures)
}









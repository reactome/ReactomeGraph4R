## for cypher queries

# clauses - MATCH, WHERE, RETURN

.MATCH <- function(clause.list) {
  final.clause <- ""
  
  # glue clauses in the list
  for (list.idx in 1:length(clause.list)) {
    clause <- as.character(clause.list[[list.idx]])
    clause <- paste0("MATCH p", list.idx, " = ", clause) # no need to worry about names of elements in the list
    final.clause <- ifelse(list.idx == 1, clause, paste(final.clause, clause))
  }
  final.clause
}


# add id, species, name, etc 
.WHERE <- function(node, ...) {
  # make a start
  clause <- "WHERE "
  
  # list(`property` = arg)
  filters <- list(...)
  
  db <- "Reactome" # set as default, for .genIdTerm()
  if ("databaseName" %in% names(filters)) {
    # get database name
    db <- filters[["databaseName"]]
    
    # can't fetch data if adding Reactome/PubMed as databaseName so just remove it
    if (any(filters[["databaseName"]] %in% c("Reactome", "PubMed"))) {
      filters <- filters[names(filters) != "databaseName"]
    }
  }
  
  # remove NULL elements in filters
  filters <- filters[!sapply(filters, is.null)]
  
  # complete WHERE clause by adding filter arguments (eg. id, species) from a query function
  for (filter in names(filters)) {
    # check if the property name correct
    .checkInfo(filter, "property")
    
    # specifically handle id & species info
    if (filter == "id") {
      add <- paste0(node, .genIdTerm(filters[[filter]], database = db))
    } else if (filter == "speciesName") {
      # automatically change different forms of species names into 'displayName'
      add <- paste0(node, '.speciesName = "', .matchSpecies(filters[[filter]], "displayName"), '"')
    } else if (filter == "schemaClass") {
      # only for function matchObject() for now 
      add <- paste0('"', filters[[filter]], '"', ' IN LABELS(', node, ')')
    } else {
      # add dquotes for those include alphabet
      tmp <- ifelse(grepl("^[0-9]+$", filters[[filter]]), filters[[filter]], paste0('"', filters[[filter]], '"'))
      add <- paste0(node, ".", filter, " = ", tmp)
    }
    
    # glue the elements
    clause <- ifelse(!grepl(node, clause), paste0(clause, add), paste0(clause, " AND ", add))
  }
  clause
}


# graph object needs 'relationships'
.RETURN <- function(node, numOfMatch=1) {
  clause <- 'RETURN '
  nodes <- paste(node, collapse = ",")
  clause <- paste0(clause, nodes)
  
  # add `relationships()` for each path
  numOfMatch <- seq(1, numOfMatch) # vectorize
  rels <- paste0("relationships(p", numOfMatch, ")")
  rels <- paste(rels, collapse = ",")
  clause <- paste0(clause, ",", rels)
  clause
}


# variable length relationships
.varLen <- function(clause, rel, depth, all) {
  # have a check!
  .checkInfo(rel, "relationship")
  # in case of 0 & negative values - the minimum depth is 1
  if (depth < 1) depth <- 1
  
  # since depth = 1 by default
  if (!all && depth == 1) {
    message("Retrieving immediately connected instances... Specify depth-related arguments for more depths")
  }
  
  # replace
  if (all) {
    new.rel <- paste0(rel, "*")
  } else {
    new.rel <- ifelse(depth > 1, paste0(rel, "*1..", as.integer(depth)), rel)
  }
  rel <- sub('.*\\|', '', rel) # use the last part to replace
  new.rel <- sub('.*\\|', '', new.rel)
  new.clause <- gsub(paste0(rel, "\\]-"), paste0(new.rel, "\\]-"), clause)
  new.clause
}


# generate the ID term - stId, dbId, external id
.genIdTerm <- function(id, database="Reactome") {
  # remove blanks
  id <- gsub("\\s", "", id)
  
  if (is.null(database) || !database %in% c("Reactome", "PubMed")) { 
    # other non-Reactome ids
    if (grepl("^[0-9]+$", id)) {
      term <- paste0('.identifier = ', id)
    } else {
      # add quotes
      term <- paste0('.identifier = "', id, '"')
    }
  } else if (database == "Reactome") {
    id <- toupper(id)
    if (grepl("^R-[A-Z]{3}-", id)) {
      term <- paste0('.stId = "', id, '"')
    } else if (grepl("^[0-9]+$", id)) {
      term <- paste0('.dbId = ', id)
    } else {
      stop("Is this id correct?", call.=FALSE)
    }
  } else if (database == "PubMed") {
    term <- paste0('.pubMedIdentifier = ', id)
  } 
  term
}



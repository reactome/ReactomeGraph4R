## for cypher queries

# clauses - MATCH, WHERE, RETURN

.MATCH <- function(clause.list) {
  num.clause <- length(clause.list)
  final.clause <- ""
  # glue clauses in the list
  for (num in 1:num.clause) {
    clause <- as.character(clause.list[[num]])
    clause <- paste0("MATCH p", num, " = ", clause) # no need to worry about names of elements in the list
    final.clause <- ifelse(num == 1, clause, paste(final.clause, clause))
  }
  final.clause
}


# add id, species, name, etc 
.WHERE <- function(node, ...) {
  # make a start
  clause <- "WHERE "
  
  # list(`property` = arg)
  filters <- list(...)
  filters <- filters[!sapply(filters, is.null)] # remove NULL elements
  if ("databaseName" %in% names(filters)) {
    # can't fetch data if adding Reactome as databaseName so just remove it
    if (filters[["databaseName"]] == "Reactome") filters <- filters[names(filters) != "databaseName"]
  }
  
  # complete WHERE clause by adding filter arguments (eg. id, species) in a query function
  for (filter in names(filters)) {
    .checkInfo(filter, "property")
    if (filter == "id") {
      db <- ifelse("databaseName" %in% names(filters), filters[["databaseName"]], "Reactome") 
      add <- paste0(node, .genIdTerm(filters[[filter]], database = db))
    } else if (filter == "speciesName") {
      add <- paste0(node, '.speciesName = "', .matchSpecies(filters[[filter]], "displayName"), '"')
    } else {
      # add dquotes for those include alphabet
      tmp <- ifelse(grepl("^[0-9]+$", filters[[filter]]), filters[[filter]], paste0('"', filters[[filter]], '"'))
      add <- paste0(node, ".", filter, " = ", tmp)
    }
    clause <- ifelse(!grepl(node, clause), paste0(clause, add), paste0(clause, " AND ", add))
  }
  clause
}


# graph object needs 'relationships'
.RETURN <- function(node, type, numOfMatch=1) {
  clause <- 'RETURN '
  nodes <- paste(node, collapse = ",")
  clause <- paste0(clause, nodes)
  # add `relationships()` for each path
  if (type == "graph") {
    numOfMatch <- seq(1, numOfMatch) # vectorize
    rels <- paste0("relationships(p", numOfMatch, ")")
    rels <- paste(rels, collapse = ",")
    clause <- paste0(clause, ",", rels)
  }
  clause
}


# variable length relationships
.varLen <- function(clause, rel, depth, all) {
  # have a check!
  .checkInfo(rel, "relationship")
  if (depth < 1) depth <- 1
  
  # depth = 1 by default
  if (!all && depth == 1) {
    message("Retrieving immediate connected instances... Specify depth-related arguments for more depths")
  }
  
  # replace
  new.rel <- ifelse(all, paste0(rel, "*"), ifelse(depth > 1, paste0(rel, "*1..", as.integer(depth)), rel))
  new.clause <- gsub(rel, new.rel, clause)
  new.clause
}


# generate the ID term - stId, dbId, external id
.genIdTerm <- function(id, database="Reactome") {
  id <- gsub("\\s", "", id) # remove blanks
  if (database == "Reactome") {
    id <- toupper(id)
    if (grepl("^R-[A-Z]{3}-", id)) {
      term <- paste0('.stId = "', id, '"')
    } else if (grepl("^[0-9]+$", id)) {
      term <- paste0(".dbId = ", id)
    } else {
      stop("Is this id correct?", call.=FALSE)
    }
  } else { # non-Reactome ids
    if (grepl("^[0-9]+$", id)) {
      term <- paste0(".identifier = ", id)
    } else {
      # add quotes
      term <- paste0('.identifier = "', id, '"')
    }
  }
  term
}




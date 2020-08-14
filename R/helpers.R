## helper functions

# hooks
.onAttach <- function(libname, pkgname) {
  # connect and store the variable to namespace
  con <- .login()
  options(con = con)
  
  if (con$ping() != 200) {
    # an error msg has been printed in the above line `con$ping()`
    stop("FYI - tutorials for graph db: https://reactome.org/dev/graph-database", call.=FALSE)
  } else {
    dbi <- neo4r::call_neo4j('MATCH (dbi:DBInfo) RETURN dbi.version', con) # get version
    packageStartupMessage(paste0("Connection built, welcome to Reactome Graph Database v", dbi[["dbi.version"]]$value, "!"))
  }
}


# log into local neo4j server
.login <- function() {
  # specify port
  port <- 7474
  while (TRUE) {
    url <- paste0("http://localhost:", port)
    ans <- utils::askYesNo(paste0("Is the url '", url, "'?"))
    
    if (ans) break
    if (is.na(ans)) stop("Cancel", call.=FALSE)
    port <- readline(prompt="specify port: ")
  }
  
  # get user & pwd if NEO4J_AUTH is not none
  if (utils::askYesNo("Does Neo4J require authentication?")) {
    user <- readline(prompt="Username: ")
    suppressMessages(password <- getPass::getPass("Password: ")) # prevent warnings in R CHECK
  } else {
    user <- "neo4j"
    password <- "neo4j"
  }
  
  # get neo4r connexion object
  con <- neo4r::neo4j_api$new(url=url, user=user, password=password)
  con
}


# match species names (similar to that one in CS pkg)
.matchSpecies <- function(species, output=c("displayName", "taxId", "dbId", "name", "abbreviation")) {
  # ensure correct input
  output <- match.arg(output, several.ok = TRUE)
  species <- as.character(species)
  
  # use the connexion object locally
  con <- getOption("con")
  
  # get all species info
  query <- 'MATCH (s:Species) RETURN s'
  all.species <- neo4r::call_neo4j(query, con)
  all.species <- all.species[['s']]
  
  # to see what data type this species arg is by checking which column it belongs to
  species.data.type <- colnames(all.species)[apply(all.species, 2, function(col) species %in% unlist(col))]
  if (length(species.data.type) == 0) stop(paste0(sQuote(species), " not listed in Reactome"))

  # output
  species.data.type <- species.data.type[1] # in case type==c("displayName","name")
  species.row <- all.species[all.species[[species.data.type]] == species, ] 

  if (length(unique(species.row$taxId)) > 1) {
    warning("This species is not unique, please use IDs or full name instead")
    as.data.frame(species.row) # transform tibble df
  } else {
    as.data.frame(unique(species.row[ ,output]))
  }
}


# check if value(s) in db or not (mostly used in internal checks)
# info is a character, not vector
.checkInfo <- function(info, type=c("label", "relationship", "property")) {
  type <- match.arg(type, several.ok = FALSE)
  info <- gsub("^:", "", info)
  
  con <- getOption("con") # get connexion
  if (type == "label") {
    info <- strsplit(info, split=':', fixed=TRUE)[[1]]
    terms <- con$get_labels()
  } else if (type == "relationship") {
    info <- strsplit(info, split='|', fixed=TRUE)[[1]]
    terms <- con$get_relationships()
  } else if (type == "property") {
    terms <- con$get_property_keys()
  }
  terms <- terms$labels # to character
  
  throw <- info[!info %in% terms]
  throw <- throw[throw != "id"] # exclude 'id' which represents 'dbId' & 'stId' & 'identifier'
  if (length(throw) > 0) {
    throw <- paste(throw, collapse = ", ")
    return(FALSE)
    warning(paste0("The ", type, " '", throw, "' is not in this database"), call.=FALSE)
  } else {
    return(TRUE)
  }
}


# call neo4j API
.callAPI <- function(query, return.names=NULL, type, verbose=FALSE, msg=NULL, ...) {
  # being wordy if 'type' arg missing
  if (verbose) message("Type argument not specified, retrieving 'row' data... For graph data, specify type='graph'")

  # get the connexion object locally
  con <- getOption("con")
  
  # call API
  json.res <- neo4r::call_neo4j(query = query,
                                con = con,
                                type = type,
                                output = "json", # output in json format
                                ...)
  # parse json data
  .parseJSON(json.res, return.names=return.names, type=type)
}


# get value of specific attribute(s) with a given id/name
.getSlotValue <- function(dbo, dbo.type=c("id", "name"), resource="Reactome", slot) {
  # assign value
  id <- NULL -> name
  if (dbo.type == "id") {
    id <- dbo
  } else {
    name <- dbo
  }
  
  # check info
  .checkInfo(slot, "property")
  
  # retrieve
  c.MATCH <- .MATCH(list('(dbo:DatabaseObject)'))
  c.WHERE <- .WHERE("dbo", id=id, displayName=name)
  c.RETURN <- .RETURN(paste0("dbo.", slot), type="row") # can input >1 slots!
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  .callAPI(query, slot, type="row")
}


# get all labels/keys of node(s)
# more types to be added
.getNodeInfo <- function(node.where, info=c("keys", "labels")) {
  info <- match.arg(info)
  query <- paste('MATCH (dbo:DatabaseObject)',
                 node.where,
                 paste0('UNWIND ', info, '(dbo) AS info'),
                 'RETURN distinct(info)')
  res <- .callAPI(query, return.names=info, type="row")
  res[[info]]
}


# ameliorate names of 'row' data list
# `\\<` & `\\>` - double-escaping to prevent replacing strings within a word
.goodName <- function(name) {
  name <- gsub('\\<pe\\>', 'physicalEntity', name)
  name <- gsub('\\<re\\>', 'referenceEntity', name)
  name <- gsub('\\<dbo\\>', 'databaseObject', name)
  name <- gsub('\\<rle\\>', 'reactionLikeEvent', name)
  name <- gsub('\\<lr\\>', 'literatureReference', name)
  name
}


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


# call neo4j API
.callAPI <- function(query, return.names=NULL, type, unique=TRUE, error.info=NULL, ...) {
  # get the connexion object locally
  con <- getOption("con")
  
  # call API
  if (type == "row") {
    # return in json format since neo4r would raise errors (from tibble)
    json.res <- neo4r::call_neo4j(query=query, con=con, type=type, output="json", ...)
    # parse json data
    res <- .parseJSON(json.res, return.names=return.names, unique=unique, error.info=error.info)
  } else {
    # graph data can use neo4r R output
    res <- neo4r::call_neo4j(query=query, con=con, type=type, output="r", ...)
    res <- lapply(res, as.data.frame) # turn tibble df to df
  }
  res
}


# get final results for queries
.finalRes <- function(query, return.names=NULL, type, unique=TRUE, error.info=NULL, ...) {
  if (type == "row") {
    # retrieve graph data first
    suppressMessages( # suppress if retrieving attributes
      graph.res <- .callAPI(query, return.names, "graph", error.info=error.info, ...)
    )
    
    # retrieve row data
    res.query <- gsub(",relationships\\s*\\([^\\)]+\\)", "", query) # remove ',relationships(p)' in RETURN clause
    res <- .callAPI(res.query, return.names, "row", unique, error.info, ...)
    
    if ("relationships" %in% names(graph.res)) {
      # add relationships from graph data into row data if any
      res <- .processRowOutput(list(row = res, graph = graph.res))
    }
  } else {
    res <- .callAPI(query, return.names, "graph", error.info=error.info, ...)
  }
  res
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
  if (length(species.data.type) == 0) {
    stop(sQuote(species), ' not listed in Reactome. Try `matchObject(schemaClass="Species")` to get valid species inputs', call.=FALSE)
  }
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


# get all labels/keys of node(s)
# species not required
# more info types to be added
.getNodeInfo <- function(node.where, info=c("keys", "labels")) {
  info <- match.arg(info)
  query <- paste('MATCH (dbo:DatabaseObject)',
                 node.where,
                 paste0('UNWIND ', info, '(dbo) AS info'),
                 'RETURN distinct(info)')
  res <- .callAPI(query, return.names=info, type="row")
  res[[1]][ ,1]
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



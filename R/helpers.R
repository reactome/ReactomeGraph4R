## helper functions

# hooks
.onAttach <- function(libname, pkgname) {
  # connect and store the variable to namespace
  con <- .login()
  options(con = con)
  
  if (con$ping() != 200) {
    # an error msg has been printed in the above line `con$ping()`
    stop("FYI - tutorials for graph db: https://reactome.org/dev/graph-database")
  } else {
    dbi <- call_neo4j('MATCH (dbi:DBInfo) RETURN dbi.version', con) # get version
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
    if (is.na(ans)) stop("Cancel")
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
  if (length(species.data.type) == 0) stop(paste0(species, " not listed in Reactome"))

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


# generate the ID term in WHERE lines (for stId & dbId)
.genIdTerm <- function(id) {
  id <- toupper(id)
  if (grepl("^R-[A-Z]{3}-", id)) {
    paste0('.stId = "', id, '"')
  } else if (grepl("^[0-9]+$", id)) {
    paste0(".dbId = ", id)
  } else {
    stop("Is this id correct?")
  }
}





## write Cypher





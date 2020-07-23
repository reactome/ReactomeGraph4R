## helper functions

# hooks
.onAttach <- function(libname, pkgname) {
  # connect and store the variable to namespace
  con <- .login()
  options(con = con)
  #assign("con", con, envir = asNamespace(reactome.graphdb4r)) ##thinking...
  
  if (con$ping() != 200) {
    # an error msg has been printed in the above line `con$ping()`
    stop("Try to check your database and user info?")
  } else {
    packageStartupMessage("Connection built, welcome to Reactome Graph Database!") 
  }
}


# log into local neo4j server
.login <- function() {
  url <- "http://localhost:7474"
  while (!utils::askYesNo(paste0("Is the url '", url, "'?"))) {
    new.port <- readline(prompt="specify port: ")
    url <- paste0("http://localhost:", new.port)
    
    new.ans <- utils::askYesNo(paste0("Is the url ", url, "?"))
    if (new.ans) break
    if (is.na(new.ans)) stop("Cancel")
  }
  
  # get user & pwd if NEO4J_AUTH is not none
  if (utils::askYesNo("Does NeoJ require authentication?")) {
    user <- readline(prompt="Username: ")
    password <- readline(prompt="Password: ")
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
  con <- getOption("con")
  
  # get all species info
  query <- 'MATCH (s:Species) RETURN s'
  all.species <- call_neo4j(query, con)
  all.species <- all.species[['s']]
  
  # to see what data type this species arg is by checking which column it belongs to
  species.data.type <- colnames(all.species)[apply(all.species, 2, function(col) species %in% unlist(col))]
  if (length(species.data.type) == 0) stop("Please input a species listed in Reactome")

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
  if (grepl("^R-...-", id)) {
    paste0('.stId = ', '"', id, '"')
  } else if (grepl("^[0-9]+$", id)) {
    paste0('.dbId = ', id)
  } else {
    stop("Is this id correct?")
  }
}





## write Cypher





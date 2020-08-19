# basic error messages
.basicErrMsgs <- function(input.list) {
  message("  Finding possible errors...")
  
  # get local connexion object
  con <- getOption("con")
  
  # get the spellCheck function
  spellCheck <- utils::getFromNamespace(".spellCheck", "reactome4r")
  
  # spell check id
  if (!is.null(input.list[["id"]])) {
    id.spellcheck <- spellCheck(input.list[["id"]])
    if (!is.null(id.spellcheck)) {
      message(
        rlang::format_error_bullets(c("i" = "'id' spell check:",
                                      id.spellcheck))
      )
    }
  }
  # spell check name
  if (!is.null(input.list[["name"]])) {
    name.spellcheck <- spellCheck(input.list[["name"]])
    if (!is.null(name.spellcheck)) {
      message(
        rlang::format_error_bullets(c("i" = "'displayName' spell check (note that the space is required):",
                                      name.spellcheck))
      )
    }
  }
  # correct schemaClass name
  if (!is.null(input.list[["class"]])) {
    message(
      rlang::format_error_bullets(c("i" = "'schemaClass': details see https://reactome.org/content/schema"))
    )
  }
  # correct database name
  if (input.list[["database"]] != "Reactome") {
    message(
      rlang::format_error_bullets(c("i" = "'databaseName': try `matchObject(schemaClass='ReferenceDatabase')` to get details"))
    )                          
  }
  # check species
  if (!is.null(input.list[["species"]])) {
    species <- input.list[["species"]]
    species.name <- .matchSpecies(species, "displayName")
    # if species not correct, an error pops out & stop here <--
    
    # get all associated species
    tmp.WHERE <- .WHERE('dbo', id=input.list[["id"]], displayName=input.list[["name"]], 
                        schemaClass=input.list[["class"]], databaseName=input.list[["database"]])
    all.species <- neo4r::call_neo4j(paste0('MATCH (dbo:DatabaseObject) ', tmp.WHERE, ' RETURN dbo.speciesName'), con)
    # check if the given species matched or not
    if (!species.name %in% all.species) {
      message(
        rlang::format_error_bullets(c("x" = paste0("Species " ,sQuote(species), ":"),
                                      paste0(sQuote(species.name), " is not in the Species attribute of the given object"))
                                   )
      
      )
    }
  }
}


# send basic error messages if no data returned, or print syntax error
.sendErrors <- function(res, error.info=NULL) {
  if (length(res) == 0) {
    # (no changes, no records)
    message(rlang::format_error_bullets(c("x"="No data returned")))
    
    # send custom errors
    if(!is.null(error.info)) {
      .basicErrMsgs(error.info)
      stop("Try to check above argument inputs. \n", 
           "  Or perhaps no result object can be found at all (`-_-)", call.=FALSE)
    }
  } else if ("error_message" %in% names(res)) {
    # query syntax error
    stop(paste0(res[["error_code"]], "\n", res[["error_message"]]), call.=FALSE)
  }
}


# check if value(s) in db or not (mostly used in internal checks)
# info is a character, NOT vector
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
    warning(rlang::format_error_bullets(c("x" = paste0("The ", type, " '", throw, "' is not in this database"))))
  } else {
    return(TRUE)
  }
}


# verify the main input arguments
.verifyInputs <- function(id=NULL, name=NULL, class=NULL, species=NULL, database="Reactome") {
  # make sure having at least one input
  if (is.null(class) && is.null(id) && is.null(name)) {
    stop("Must specify 'id' or 'displayName' or 'class'", call.=FALSE)
  } else if (missing(class) && is.null(id) && is.null(name)) {
    stop("Must specify either 'id' or 'displayName'", call.=FALSE)
  }

  if (database != "Reactome") {
    # remove species filter for an external id
    # `with(parent.frame())` -- execute in the parent environment
    species <- NULL
    with(parent.frame(), {species <- NULL})
  }
  list(id=id, name=name, class=class, species=species, database=database)
}


# check the class input matching with specific Class(s) or not
.checkClass <- function(id, displayName, class, database="Reactome", stopOrNot=FALSE) {
  # get labels of the node
  labels <- .getNodeInfo(.WHERE('dbo', id=id, displayName=displayName, databaseName=database), "labels")
  
  # send error
  if (!any(class %in% labels)) {
    if (stopOrNot) {
      stop(rlang::format_error_bullets(
              c("x" = paste0("This is not a ", paste(sQuote(class), collapse=","), " object"))),
          call.=FALSE)
    } else {
      message(rlang::format_error_bullets(
              c("x" = paste0("This is not a ", paste(sQuote(class), collapse=","), " object")))
             )
      # will return NULL
    }
  } else {
    # get the schema class
    return(labels[labels %in% class])
  }
}



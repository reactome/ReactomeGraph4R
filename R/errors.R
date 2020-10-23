# basic error messages
#' @import ReactomeContentService4R 
.basicErrMsgs <- function(input.list) {
  message("  Finding possible errors...")
  
  # get local connexion object
  con <- getOption("con")
  
  # get the spellCheck function
  spellCheck <- utils::getFromNamespace(".spellCheck", "ReactomeContentService4R")
  errBullets <- utils::getFromNamespace("format_error_bullets", "rlang")
  
  # spell check id
  if (!is.null(input.list[["id"]])) {
    id.spellcheck <- spellCheck(input.list[["id"]])
    if (!is.null(id.spellcheck)) {
      message(errBullets(c("i" = "'id' check:", id.spellcheck)))
    }
  }
  # spell check name
  if (!is.null(input.list[["name"]])) {
    name.spellcheck <- spellCheck(input.list[["name"]])
    if (!is.null(name.spellcheck)) {
      message(errBullets(
          c("i" = "'displayName' check (note that the space is required):",
             name.spellcheck)))
    }
  }
  # correct schemaClass name
  if (!is.null(input.list[["class"]])) {
    message(errBullets(
      c("i" = "'schemaClass': details see https://reactome.org/content/schema"))
    )
  }
  # correct database name
  if (input.list[["database"]] != "Reactome") {
    message(errBullets(
      c("i" = "'databaseName': try `matchObject(schemaClass='ReferenceDatabase')` to get details"))
    )   
  }
  # check species
  if (!is.null(input.list[["species"]])) {
    species <- input.list[["species"]]
    species.name <- .matchSpecies(species, "displayName")
    # if species not correct, an error pops out & stop here <--
    
    # get all associated species
    tmp.WHERE <- .WHERE('dbo', id=input.list[["id"]], 
                        displayName=input.list[["name"]], 
                        schemaClass=input.list[["class"]], 
                        databaseName=input.list[["database"]])
    all.species <- neo4r::call_neo4j(paste0('MATCH (dbo:DatabaseObject) ', 
                                    tmp.WHERE, ' RETURN dbo.speciesName'), con)
    # check if the given species matched or not
    if (!species.name %in% all.species) {
      message(errBullets(
          c("x" = paste0("Species " ,sQuote(species), ":"),
            paste0(sQuote(species.name), 
            " is not in the Species attribute of the given object")))
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
           "  Or perhaps just no result object can be found", call.=FALSE)
    }
  }
}


# check if value(s) in db or not (mostly used in internal checks)
# info is a character, NOT vector for "label" & "relationship"
.checkInfo <- function(info, type=c("label", "relationship", "property")) {
  type <- match.arg(type, several.ok = FALSE)
  # remove colon
  info <- gsub("^:", "", info)
  
  # get connexion
  con <- getOption("con")
  
  # get all labels/relationships/properties existing in the current database
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
  
  # get those not in db
  throw <- info[!info %in% terms]
  # exclude 'id' which represents 'dbId' & 'stId' & 'identifier' in this pkg
  throw <- throw[throw != "id"]
  if (length(throw) > 0) {
    throw <- paste(throw, collapse = ", ")
    message(rlang::format_error_bullets(c("x" = paste0("The ", type, " '", 
                                        throw, "' is not in this database"))))
    return(FALSE)
  } else {
    return(TRUE)
  }
}


# verify the main input arguments & output a list
.verifyInputs <- function(id=NULL, name=NULL, class=NULL, species=NULL, 
                          database="Reactome", type) {
  # being wordy if 'type' arg missing
  # `with(parent.frame())` -- execute in the parent environment
  if (!is.null(type)) {
    with(parent.frame(), {
      if (missing(type)) {
        message("Type argument not specified, retrieving 'row' data... For graph data, specify type='graph'")
      }
      type <- match.arg(type, several.ok=FALSE)
    })
  }
  
  # make sure having at least one input
  if (is.null(class) && is.null(id) && is.null(name)) {
    stop("Must specify 'id' or 'displayName' or 'schemaClass'", call.=FALSE)
  } else if (missing(class) && is.null(id) && is.null(name)) {
    stop("Must specify either 'id' or 'displayName'", call.=FALSE)
  }

  if (is.null(database) || database != "Reactome") {
    # remove species filter for an external id
    species <- NULL
    with(parent.frame(), {species <- NULL})
  }
  
  # return a list summarizing these args
  list(id=id, name=name, class=class, species=species, database=database)
}


# check the class input matching with specific Class(es) or not
.checkClass <- function(id, displayName, class, database="Reactome", stopOrNot=FALSE) {
  # get labels of the node
  labels <- .getNodeInfo(.WHERE('dbo', id=id, displayName=displayName, 
                                databaseName=database), "labels")
  
  # send error
  if (!any(class %in% labels)) {
    if (stopOrNot) {
      stop(rlang::format_error_bullets(
              c("x" = paste0("Please specify an instance of Class ", 
                             paste(sQuote(class), collapse=",")))),
          call.=FALSE)
    } else {
      message(rlang::format_error_bullets(
              c("x" = paste0("This is not an instance of Class ", 
                             paste(sQuote(class), collapse=","))))
             )
      # will return NULL
    }
  } else {
    # get the schema class
    return(labels[labels %in% class])
  }
}



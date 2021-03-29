# easily access graph data without Cypher


#' Basic query for database objects
#' 
#' This function can fetch instance by setting the following arguments:
#' - \strong{id}: a Reactome dbId/stId, or non-Reactome id
#' - \strong{displayName}: a display name of a Reactome object
#' - \strong{schemaClass}: a specific schema class, 
#' see \href{https://reactome.org/content/schema/DatabaseObject}{Data Schema}
#' - \strong{property}: a property of a node or relationship, access the full 
#' list of properties: `con <- getOption("con"); con$get_property_keys()`
#' - \strong{relationship}: a relationship between nodes, access the full 
#' list of relationships: `con <- getOption("con"); con$get_relationships()`
#' - Species information can see 
#' \href{https://reactome.org/content/schema/objects/Species}{here}, or run 
#' `View(matchObject(schemaClass = "Species"))[['databaseObject']]` to view a 
#' full table
#' 
#' 
#' @param id Reactome stId or dbId, or non-Reactome identifier
#' @param displayName displayName of a database object
#' @param schemaClass schema class of a database object
#' @param species name or taxon id or dbId or abbreviation of specified species
#' @param returnedAttributes specific attribute(s) to be returned. 
#' If set to `NULL`, all attributes returned
#' @param property a list of property keys and values, 
#' e.g. `list(isChimeric = TRUE, isInDisease = TRUE)`
#' @param relationship a relationship type
#' @param limit the number of returned objects
#' @param databaseName database name. All databases see 
#' \href{https://reactome.org/content/schema/objects/ReferenceDatabase}{here}
#' @return Reactome database object(s) that meets all specified conditions
#' @examples
#' # fetch instance by class
#' all.species <- matchObject(schemaClass = "Species")
#' 
#' # fetch instance by name
#' matchObject(displayName = "RCOR1 [nucleoplasm]", 
#'            returnedAttributes=c("stId", "speciesName"))
#' 
#' # fetch instance by id
#' ## Reactome id
#' matchObject(id = "R-HSA-9626034")
#' ## non-Reactome id
#' matchObject(id = "P60484", databaseName = "UniProt")
#' 
#' # fecth instances by relationship
#' matchObject(relationship="inferredTo", limit=10)
#' 
#' # fetch instances by property
#' property.list <- list(hasEHLD = TRUE, isInDisease = TRUE)
#' matchObject(property = property.list, 
#'     returnedAttributes = c("displayName", "stId", "isInDisease", "hasEHLD"), 
#'     limit=20)
#' 
#' @rdname matchObject
#' @family match 
#' @seealso [multiObjects] for multiple ids
#' @export 

matchObject <- function(id=NULL,displayName=NULL,schemaClass=NULL,species=NULL, 
                        returnedAttributes=NULL,property=NULL,relationship=NULL,
                        limit=NULL, databaseName="Reactome") {
  # check inputs
  type <- "row" # return row data only
  if (is.null(databaseName) || databaseName != "Reactome") species <- NULL
  
  # get a function (short func name)
  infoBullets <- utils::getFromNamespace("format_error_bullets", "rlang")
  
  # check attributes in the db or not
  # NULL also returns TRUE
  .checkInfo(returnedAttributes, "property")
  
  if (!is.null(relationship)) {
    # retrieve data based on relationship
    
    message(infoBullets(c("i" = paste0("Note that other arguments except ",
                    "'limit' should be NULL if you specify 'relationship'"))))
    message(infoBullets(c("i" = "Turn them into NULL")))
    id <- displayName <- schemaClass <- NULL -> species -> returnedAttributes -> property
    
    # check if it's a correct relationship name
    .checkInfo(relationship, "relationship")
    
    c.MATCH <- paste0('MATCH (n1)-[r:', relationship, ']->(n2)')
    c.WHERE <- ""
    c.RETURN <- 'RETURN n1,n2'
    return.names <- c("n1", "n2")
    unique <- FALSE
  } else {
    # retrieve
    c.MATCH <- .MATCH(list('(dbo:DatabaseObject)'))
    c.WHERE <- .WHERE("dbo", id=id, displayName=displayName, 
                  schemaClass=schemaClass, speciesName=species, 
                  databaseName=databaseName)
    
    # modify WHERE clause if 'property' specified
    if (!is.null(property)) {
      if (!is.null(id) || !is.null(displayName)) {
        message(infoBullets(c("i" = paste0("Do not input 'id' or 'displayName'",
                              " if you've specified 'property'"))))
        message(infoBullets(c("i" = "Turn them into NULL")))
      }
      .checkInfo(names(property), "property")
      c.WHERE <- .WHERE("dbo", schemaClass=schemaClass, 
                        speciesName=species, databaseName=databaseName)
      property.WHERE <- paste(vapply(names(property), function(n) 
                         paste0("dbo.", n, " = ", property[[n]]), character(1)), 
                         collapse = " AND ")
      c.WHERE <- ifelse(grepl("=", c.WHERE), 
                        paste0(c.WHERE, " AND ", property.WHERE), 
                        paste0(c.WHERE, property.WHERE))
    }

    if (is.null(returnedAttributes)) {
      nodes4return <- "dbo" # return all attributes
    } else {
      nodes4return <- paste0("dbo.", returnedAttributes)
    }
    c.RETURN <- .RETURN(nodes4return)
    return.names <- .goodName(nodes4return)
    unique <- TRUE
  }
  
  # add limit
  if (!is.null(limit)) {
    c.RETURN <- paste0(c.RETURN, " LIMIT ", limit)
  }
  
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  
  # for generating error messages
  input.list <- list(id=id, name=displayName, class=schemaClass, 
                     species=species, database=databaseName)
  # retrieve
  .finalRes(query, return.names=return.names, type=type, unique=unique, 
            error.info=input.list, basicQuery=TRUE)
}



#' Retrieve multiple Reactome objects
#' 
#' The [matchObject] function takes only one id/name at a time, this method 
#' allows you to input many ids and get an aggregated table for their detailed 
#' information. It can only accept \strong{ids} for now.
#' 
#' 
#' @param ids Reactome stIds/dbIds, or non-Reactome ids
#' @param databaseName database name
#' @param speedUp set `TRUE` to use \code{\link[doParallel]{doParallel}} method
#' @param cluster the number of cluster in \code{\link[parallel]{makeCluster}}
#' @return Reactome database objects for the given ids
#' @rdname multiObjects
#' @importFrom data.table rbindlist
#' @importFrom purrr flatten
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @export 
#' @seealso [matchObject] for details
#' @examples
#' # "ids" can be Reactome or non-Reactome ids
#' ids <- c("P02741", "P08887", "P08505", "Q9GZQ8")
#' res <- multiObjects(ids, databaseName="UniProt", speedUp=TRUE)
#' 

multiObjects <- function(ids, databaseName="Reactome",
                         speedUp=FALSE, cluster=2) {
  if (speedUp) {
    # use doParallel
    cl <- makeCluster(cluster)
    registerDoParallel(cl)
    
    # combine dataframes
    dfcomb <- function(...) {
      rbindlist(list(...), fill=TRUE)
    }
    
    con <- getOption("con")
    id <- NULL # prevent note in R CMD Check
    res <- foreach(id=ids, .packages=c("ReactomeGraph4R","purrr", 
                      "ReactomeContentService4R"), .combine=dfcomb) %dopar% {
      # log in again using user's id & password
      login(con)
      object <- matchObject(id, databaseName=databaseName)
      if (!is.null(object)) {
        return(object[['databaseObject']])
      }
    }
    stopCluster(cl)
  } else {
    # use lapply instead
    all.list <- lapply(seq_along(ids), function(index) 
                        matchObject(id=ids[index], databaseName=databaseName))
    all.list <- flatten(all.list) # automatically drops NULL elements
    res <- rbindlist(all.list, fill=TRUE)
    # it returns 'Null data.table (0 rows and 0 cols)' for an empty list
  }
  res
}



#' MATCH the preceding/following Events
#' 
#' This method can find preceding and following ReactionLikeEvents (RLEs) of a 
#' specific Event with the relationship 'precedingEvent'. 
#' The argument "depth" is used to describe the "variable length relationships" 
#' in Neo4j, default is 1 (i.e. immediately connected); or you can set 
#' `all.depth = TRUE` to retrieve the whole context.
#' 
#' @param event.id stId/dbId of an Event
#' @param event.displayName displayName of an Event
#' @param species name or taxon id or dbId or abbreviation of specified species
#' @param all.depth if set to `TRUE`, all RLE(s) connected to the given Event 
#' in all depths returned
#' @param depth number of depths
#' @param type to return results as a list of dataframes (\strong{'row'}), 
#' or as a graph object (\strong{'graph'})
#' @return preceding/following Events connected to the given Event in specified 
#' depth(s), default depth = 1
#' @examples
#' matchPrecedingAndFollowingEvents("R-HSA-983150", depth=2, type="row")
#' @rdname matchPrecedingAndFollowingEvents
#' @family match
#' @export 

matchPrecedingAndFollowingEvents <- function(event.id=NULL, event.displayName=NULL, 
                                        species=NULL, depth=1, all.depth=FALSE,
                                        type=c("row", "graph")) {
  # ensure the inputs
  input.list <- .verifyInputs(event.id, event.displayName, 
                              species=species, type=type)
  
  # check if it's Event
  .checkClass(id=event.id, displayName=event.displayName, class="Event")
  
  # full query
  ## retrieve preceding/following Events
  MATCH.list <- list('(pevent:Event)<-[:precedingEvent]-(event:Event)',
                     '(event)<-[:precedingEvent]-(fevent:Event)')
  c.MATCH <- .MATCH(MATCH.list)
  ## add depths
  c.MATCH <- .varLen(clause=c.MATCH, rel=':precedingEvent', 
                     depth=depth, all=all.depth)
  ## add conditions
  c.WHERE <- .WHERE(node='event', id=event.id, displayName=event.displayName, 
                    speciesName=species)
  ## return
  c.RETURN <- .RETURN(node=c("pevent", "event", "fevent"), 
                      numOfMatch=length(MATCH.list))
  return.names <- c("precedingEvent", "event", "followingEvent")
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  
  # retrieve final results
  .finalRes(query, return.names, type, error.info=input.list)
}


#' MATCH hierarchy
#' 
#' Reactome data are organized in a hierarchical way: Pathway-Reaction-Entity. 
#' This function retrieves the hierarchical data of a given Event 
#' (Pathway or Reaction) or Entity (PhysicalEntity or ReferenceEntity).
#' 
#' @param id stId or dbId of an Event/Entity; or an external id
#' @param displayName displayName of Event/PhysicalEntity/ReferenceEntity
#' @param databaseName database name
#' @param species name or taxon id or dbId or abbreviation of specified species
#' @param type return results as a list of dataframes (\strong{'row'}), 
#' or as a graph object (\strong{'graph'})
#' @return hierarchical instances of the given id and databaseName
#' @examples
#' # use the Reactome displayName of a UniProt object
#' matchHierarchy(displayName="UniProt:P04637 TP53", 
#'                databaseName="UniProt", type="row")
#' matchHierarchy(id="R-HSA-1369062", type="graph")
#' @rdname matchHierarchy
#' @family match
#' @export 

matchHierarchy <- function(id=NULL, displayName=NULL, databaseName="Reactome", 
                           species=NULL, type=c("row", "graph")) {
  # ensure the inputs
  input.list <- .verifyInputs(id, displayName, species=species, 
                              database=databaseName, type=type)
  
  # get class
  class <- .checkClass(id=id, displayName=displayName, 
                       stopOrNot=TRUE, database=databaseName,
                       class=c("Event", "ReferenceEntity", "PhysicalEntity"))
  
  # full query
  # (RE <--) PE <-- Reactions <-- Pathways
  # retrieve ALL Events linked with 'hasEvent' since it's hierarchy 
  all.MATCH.list <- list('(re:ReferenceEntity)<-[:referenceEntity]-(pe:PhysicalEntity)',
                         '(pe:PhysicalEntity)<-[:input|output|catalystActivity|regulatedBy]-(event:Event)',
                         '(event:Event)<-[:hasEvent*]-(upperevent:Event)')
  
  if (class == "ReferenceEntity") {
    MATCH.list <- all.MATCH.list
    node4where <- 're'
    nodes4return <- c("re", "pe", "event", "upperevent")
  } else if (class == "PhysicalEntity") {
    MATCH.list <- all.MATCH.list[-1]
    node4where <- 'pe'
    nodes4return <- c("pe", "event", "upperevent")
  } else if (class == "Event") {
    MATCH.list <- all.MATCH.list[-c(1,2)]
    node4where <- 'event'
    nodes4return <- c("event", "upperevent")
  }

  c.MATCH <- .MATCH(MATCH.list)
  c.WHERE <- .WHERE(node4where, id=id, displayName=displayName, 
                    databaseName=databaseName, speciesName=species)
  c.RETURN <- .RETURN(nodes4return, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  
  # retrieve
  .finalRes(query, .goodName(nodes4return), type, error.info=input.list)
}


#' MATCH interactors
#' 
#' To retrieve interactions of a given PhysicalEntity (PE), it first finds the 
#' ReferenceEntity matched with the PE, then get the Interactions having 
#' "interactor" relationship with the ReferenceEntity.
#' 
#' @param pe.id stId or dbId of a PhysicalEntity
#' @param pe.displayName displayName of a PhysicalEntity
#' @param species name or taxon id or dbId or abbreviation of specified species
#' @param type return results as a list of dataframes (\strong{'row'}), 
#' or as a graph object (\strong{'graph'})
#' @return interactions of a given PhysicalEntity
#' @examples
#' matchInteractors(996766)
#' @rdname matchInteractors
#' @family match
#' @export 

matchInteractors <- function(pe.id=NULL, pe.displayName=NULL, 
                             species=NULL, type=c("row", "graph")) {
  # ensure inputs
  input.list <- .verifyInputs(pe.id, pe.displayName, species=species, type=type)
  
  # check Class
  .checkClass(id=pe.id, displayName=pe.displayName, class="PhysicalEntity")
  
  # full query
  # PhysicalEntities --> RefEntities <-- Interaction
  MATCH.list <- list('(pe:PhysicalEntity)-[:referenceEntity]->(re:ReferenceEntity)<-[:interactor]-(interaction:Interaction)')
  c.MATCH <- .MATCH(MATCH.list)
  c.WHERE <- .WHERE("pe", id=pe.id, displayName=pe.displayName, 
                    speciesName=species)
  nodes4return <- c("pe", "re", "interaction")
  c.RETURN <- .RETURN(nodes4return, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  
  # retrieve
  .finalRes(query, .goodName(nodes4return), type, error.info=input.list)
}


#' MATCH Reactions in associated Pathway
#' 
#' This method could find all Reactions connected with a given Pathway by 
#' the relationship 'hasEvent'. Also, the input can be a Reaction, the result 
#' would then be Pathway(s) linked via 'hasEvent' together with other Reactions 
#' linked with the Pathways(s).
#' 
#' @param event.id stId or dbId of an Event
#' @param event.displayName displayName of an Event
#' @param species name or taxon id or dbId or abbreviation of a species
#' @param type return results as a list of dataframes (\strong{'row'}), 
#' or as a graph object (\strong{'graph'})
#' @return Reactions connected to the given Pathway/Reaction via 
#' 'hasEvent' relationships
#' @examples
#' matchReactionsInPathway("R-HSA-1369062", type="graph")
#' matchReactionsInPathway("R-HSA-5682285", type="row")
#' @rdname matchReactionsInPathway
#' @family match
#' @export 

matchReactionsInPathway <- function(event.id=NULL, event.displayName=NULL, 
                                    species=NULL, type=c("row", "graph")) {
  # ensure inputs
  input.list <- .verifyInputs(event.id, event.displayName, 
                              species=species, type=type)
  
  # get class
  event.class <- .checkClass(id=event.id, displayName=event.displayName, 
                        class=c("Pathway", "ReactionLikeEvent"), stopOrNot=TRUE)
  
  # full query
  if (event.class == "Pathway") {
    # find Reactions connected to the given Pathway
    MATCH.list <- list('(pathway:Pathway)-[:hasEvent]->(rle:ReactionLikeEvent)')
    node4where <- "pathway"
    nodes4return <- c("pathway", "rle")
  } else if (event.class == "ReactionLikeEvent") {
    # find the Pathway connected with the given Reaction
    # and get other Reactions that also connect to the Pathway
    MATCH.list <- list('(rle:ReactionLikeEvent)<-[:hasEvent]-(pathway:Pathway)-[:hasEvent]->(otherReactionLikeEvent:ReactionLikeEvent)')
    node4where <- "rle"
    nodes4return <- c("rle", "pathway", "otherReactionLikeEvent")
  }
  c.MATCH <- .MATCH(MATCH.list)
  c.WHERE <- .WHERE(node4where, id=event.id, displayName=event.displayName, 
                    speciesName=species)
  c.RETURN <- .RETURN(nodes4return, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  
  # retrieve
  .finalRes(query, .goodName(nodes4return), type, error.info=input.list)
}


#' MATCH biological referrals
#' 
#' This method retrieves Reactome objects that are connected with the given 
#' object in a _reverse_ relationship. For example, to find Pathways containing 
#' the given Reaction.
#' 
#' For now it just focuses on biological referrals in the following Classes: 
#' "Event", "PhysicalEntity", "Regulation", "CatalystActivity","ReferenceEntity"
#' , "Interaction", "AbstractModifiedResidue".
#' 
#' @param id stId or dbId of a Reactome object
#' @param displayName displayName of a Reactome object
#' @param main if set to `TRUE`, only \strong{first-class} referrals returned
#' @param all.depth if set to `TRUE`, connected objects in all depths returned
#' @param depth number of depths
#' @param species name or taxon id or dbId or abbreviation of a species
#' @param type return results as a list of dataframes (\strong{'row'}), 
#' or as a graph object (\strong{'graph'})
#' @return referrals of the given instance
#' @examples
#' matchReferrals("113454", type="row")
#' \dontrun{
#' matchReferrals("R-HSA-112479", main=FALSE, all.depth=TRUE, type="row")
#' }
#' @rdname matchReferrals
#' @family match
#' @export 

matchReferrals <- function(id=NULL, displayName=NULL, main=TRUE, depth=1, 
                       all.depth=FALSE, species=NULL, type=c("row", "graph")) {
  # ensure inputs
  input.list <- .verifyInputs(id, displayName, species=species, type=type)
  
  # all first-class referrals (relationships) of schema class objects of our interest
  # collected from https://reactome.org/content/schema/ (Aug. 2020) 
  referral.list <- list(
    Event = c("hasEvent", "modification", "templateEvent", "catalyzedEvent", 
              "consumedByEvent", "producedByEvent", "regulatedEntity"),
    PhysicalEntity = c("activeUnit", "diseaseEntity", "entityOnOtherCell",
                       "hasCandidate", "hasComponent", "hasMember", "input",
                       "modification", "normalEntity", "output", "physicalEntity",
                       "regulator", "repeatedUnit", "requiredInputComponent"),
    Regulation = c("modification", "regulatedBy", "negativelyRegulates", 
                   "positivelyRegulates", "isRequired"),
    CatalystActivity = c("catalystActivities", "catalystActivity", "modification"),
    ReferenceEntity = c("interactor", "modification", "source", "target",
                        "referenceEntity", "referenceSequence", "secondReferenceSequence",
                        "referenceGene", "isoformParent", "referenceTranscript"),
    Interaction = "modification",
    AbstractModifiedResidue = c("hasModifiedResidue", "modification")
  )
  
  # select those really in the database
  suppressMessages(
    referral.list <- lapply(referral.list, function(class) {
      class[vapply(class, function(rel) 
              .checkInfo(rel, "relationship"), logical(1))] 
    })
  )
  
  # make sure it's one of our interested schema classes
  class <- .checkClass(id=id, displayName=displayName, 
                       class=names(referral.list), stopOrNot=TRUE)
  
  # full cypher query
  ## to return first-class referrals or all
  relationships <- ifelse(main, paste0(":", paste(referral.list[[class]], collapse = "|")), "")
  
  MATCH.list <- list(paste0('(dbo:DatabaseObject)-[', relationships, ']->(n:', class, ')'))
  c.MATCH <- .MATCH(MATCH.list)
  c.MATCH <- .varLen(clause=c.MATCH, rel=relationships, 
                     depth=depth, all=all.depth)
  c.WHERE <- .WHERE("n", id=id, displayName=displayName, speciesName=species)
  # filter out unwanted nodes
  c.WITH <- 'WITH *, nodes(p1) AS ns'
  c.WHERE.2 <- 'WHERE ALL(node IN ns WHERE NOT node:InstanceEdit)'
  c.RETURN <- .RETURN(c("n", "dbo"), length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.WITH, c.WHERE.2, c.RETURN)
  
  # retrieve
  .finalRes(query, .goodName(c(class, "dbo")), type, error.info=input.list)
}


#' MATCH roles of PhysicalEntity
#' 
#' This function retrieves the role(s) of a given PhysicalEntity including:
#' - Input
#' - Output
#' - Regulator
#' - Catalyst
#' 
#' @param pe.id stId or dbId of a PhysicalEntity
#' @param pe.displayName displayName of a PhysicalEntity
#' @param species name or taxon id or dbId or abbreviation of a species
#' @param type return results as a list of dataframes (\strong{'row'}), 
#' or as a graph object (\strong{'graph'})
#' @return information of the given PhysicalEntity and its role(s)
#' @examples
#' matchPEroles(pe.id = "R-HSA-8944354", type = "graph")
#' matchPEroles(pe.displayName = "2SUMO1:MITF [nucleoplasm]", 
#'              species = "pig", type = "row")
#' @rdname matchPEroles
#' @family match
#' @export 

matchPEroles <- function(pe.id=NULL, pe.displayName=NULL, 
                         species=NULL, type=c("row", "graph")) {
  # ensure inputs
  input.list <- .verifyInputs(pe.id, pe.displayName, species=species, type=type)
  
  # check the Class
  .checkClass(id=pe.id, displayName=pe.displayName, class="PhysicalEntity")
  
  # full query
  # find existing ‘roles’ - input, output, regulator, catalyst
  MATCH.list <- list('(pe:PhysicalEntity)<-[:input|output|catalystActivity|regulatedBy]-(dbo:DatabaseObject)')
  c.MATCH <- .MATCH(MATCH.list)
  c.WHERE <- .WHERE('pe', id=pe.id, displayName=pe.displayName, 
                    speciesName=species)
  nodes4return <- c("pe", "dbo")
  c.RETURN <- .RETURN(nodes4return, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  
  # retrieve
  .finalRes(query, .goodName(nodes4return), type, error.info=input.list)
}


#' MATCH diseases of PhysicalEntity/Reaction/Pathway
#' 
#' To find Diseases related to a PhysicalEntity or an Event, or 
#' get PhysicalEntities/Events associated with a Disease in reverse
#' 
#' @param id stId or dbId of a PhysicalEntity/Event/Disease
#' @param displayName displayName of a PhysicalEntity/Event/Disease
#' @param species name or taxon id or dbId or abbreviation of aspecies
#' @param type return results as a list of dataframes (\strong{'row'}), 
#' or as a graph object (\strong{'graph'})
#' @return Disease(s) related to the given PhysicalEntity/Reaction/Pathway; 
#' or instances related to the given Disease
#' @examples
#' matchDiseases(displayName="neuropathy", species="M. musculus", type="row")
#' matchDiseases(id="R-HSA-162588", type="graph")
#' @rdname matchDiseases
#' @family match
#' @export 

matchDiseases <- function(id=NULL, displayName=NULL, species=NULL, 
                          type=c("row", "graph")) {
  # ensure inputs
  input.list <- .verifyInputs(id, displayName, species=species, type=type)
  
  # check the Class
  class <- .checkClass(id=id, displayName=displayName, stopOrNot=TRUE,
                       class=c("PhysicalEntity", "Event", "Disease"))
  
  if (class == "Disease") {
    message("Retrieving instances associated with the given Disease...")
    
    # find instances associated with the given Disease
    MATCH.list <- list(paste0('(disease:Disease)<-[:disease]-(dbo:DatabaseObject)'))
    # no species slot in Disease
    c.WHERE <- .WHERE('disease', id=id, displayName=displayName)
    nodes4return <- c("disease", "dbo") -> return.names
  } else {
    message("Retrieving Diseases associated with the given instance...")
    
    # check if the instance is in Disease or not
    isInDisease <- matchObject(id=id, displayName=displayName, 
                              returnedAttributes="isInDisease", species=species)
    isInDisease <- isInDisease[["databaseObject"]][["isInDisease"]]
    
    if (!isInDisease) {
      stop("No associated disease", call.=FALSE)
    } else {
      # find Diseases associated with the given PhysicalEntity/Reaction/Pathway
      MATCH.list <- list(paste0('(n:', class,')-[:disease]->(disease:Disease)'))
      c.WHERE <- .WHERE('n', id=id, displayName=displayName, speciesName=species)
      nodes4return <- c("n", "disease")
      return.names <- c(class, "disease")
    }
  }
  c.MATCH <- .MATCH(MATCH.list)
  c.RETURN <- .RETURN(nodes4return, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  
  # retrieve
  .finalRes(query, .goodName(return.names), type, error.info=input.list) 
}


#' MATCH objects related to a paper
#' 
#' Fetch Reactome instances related to a paper by its PubMed id or title
#' 
#' @param pubmed.id PubMed identifier of a paper
#' @param displayName paper title
#' @param type return results as a list of dataframes (\strong{'row'}), 
#' or as a graph object (\strong{'graph'})
#' @return Reactome instances associated with a paper
#' @examples
#' # fetch Reactome instances by paper title
#' matchPaperObjects(displayName="Chaperone-mediated autophagy at a glance")
#' 
#' # fetch Reactome instances by pubmed id
#' matchPaperObjects(pubmed.id="20797626", type="graph")
#' matchPaperObjects(pubmed.id="23515720", type="row")
#' @rdname matchPaperObjects
#' @family match
#' @export 

matchPaperObjects <- function(pubmed.id=NULL, displayName=NULL, 
                              type=c("row", "graph")) {
  # ensure inputs
  input.list <- .verifyInputs(pubmed.id, displayName, type=type)
  
  # check Class
  .checkClass(id=pubmed.id, displayName=displayName, database="PubMed", 
              class="LiteratureReference")
  
  # get objects associated with the given LiteratureReference
  MATCH.list <- list('(lr:LiteratureReference)<-[:literatureReference]-(dbo:DatabaseObject)')
  c.MATCH <- .MATCH(MATCH.list)
  c.WHERE <- .WHERE('lr', pubMedIdentifier=pubmed.id, displayName=displayName)
  nodes4return <- c("lr", "dbo")
  c.RETURN <- .RETURN(nodes4return, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  
  # retrieve
  .finalRes(query, .goodName(nodes4return), type, error.info=input.list) 
}


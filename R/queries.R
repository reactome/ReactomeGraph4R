# easily access graph data without Cypher


#' Basic query for database object 
#' 
#' Fetch instance by dbId/stId/displayName
#' 
#' @param id stId or dbId of database object
#' @param name name of database object
#' @param species name or taxon id or dbId or abbreviation of specified species
#' @return data of the given id or name
#' @examples
#' matchObject(name = "RCOR1 [nucleoplasm]")
#' @rdname matchObject
#' @family match
#' @export 

matchObject <- function(id=NULL, name=NULL, species="human") {
  # make sure the input
  if (is.null(name) && is.null(id)) {
    stop("Must specify either an 'id' or a 'name'")
  }
  
  if (missing(species)) {
    message("Species not specified, returning human data...")
  }
  
  # retrieve
  c.MATCH <- .MATCH(list('(dbo:DatabaseObject)'))
  c.WHERE <- .WHERE("dbo", id=id, displayName=name, speciesName=species)
  nodes4return <- "dbo"
  c.RETURN <- .RETURN(nodes4return, type="row")
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  
  .callAPI(query, return.names=.goodName(nodes4return), type="row")
}


#' MATCH the preceding/following Events
#' 
#' @param id a stable or db id of an Event
#' @param name an Event name
#' @param species name or taxon id or dbId or abbreviation of specified species
#' @param all.depth if set to `TRUE`, all RLEs connected to the given Event in all depths returned
#' @param depth number of depths
#' @param type to return results as a list of dataframes (\strong{'row'}) or as a graph object (\strong{'graph'})
#' @return preceding/following Events connected to the given Event in specified depth(s), default depth = 1
#' @examples
#' matchPrecedingAndFollowingEvents("R-HSA-983150", all.depth=TRUE, type="row")
#' @rdname matchPrecedingAndFollowingEvents
#' @family match
#' @export 

matchPrecedingAndFollowingEvents <- function(id=NULL, name=NULL, species=NULL, depth=1, 
                                             all.depth=FALSE, type=c("row", "graph")) {
  # ensure the inputs
  if (is.null(name) && is.null(id)) {
    stop("Must specify either an 'id' or a 'name'")
  }
  isVerbose <- missing(type)
  type <- match.arg(type, several.ok=FALSE)
  
  # full query
  ## retrieve preceding/following Events
  match.list <- list('(pevent:Event)<-[:precedingEvent]-(event:Event)',
                     '(event)<-[:precedingEvent]-(fevent:Event)')
  c.MATCH <- .MATCH(match.list)
  ## add depths
  c.MATCH <- .varLen(clause=c.MATCH, rel=':precedingEvent', depth=depth, all=all.depth)
  ## add conditions
  c.WHERE <- .WHERE(node='event', id=id, displayName=name, speciesName=species)
  ## return
  c.RETURN <- .RETURN(node=c("pevent", "event", "fevent"), type=type, numOfMatch=length(match.list))
  return.names <- c("precedingEvent", "event", "followingEvent")
  
  query <- paste(c.MATCH, c.WHERE, c.RETURN)

  # call API
  tmp.id <- ifelse(is.null(id), name, id)
  new.msg <- paste0("This Event ", sQuote(tmp.id), " probably has no other connected Events")
  .callAPI(query, return.names, type, isVerbose, new.msg)
}


#' MATCH hierarchy
#' 
#' Retrieve hierarchical data: Pathway-Reaction-Entity
#' 
#' @param id stId or dbId of Event/PhysicalEntity; or id of an external database
#' @param resource database name. All listed databases see \href{here}{https://reactome.org/content/schema/objects/ReferenceDatabase}
#' @param species name or taxon id or dbId or abbreviation of specified species
#' @param type return results as a list of dataframes (\strong{'row'}) or as a graph object (\strong{'graph'})
#' @return hierarchical instances of the given id and resource
#' @examples
#' matchHierarchy(id="P04637", resource="UniProt", type="row")
#' matchHierarchy(id="R-HSA-196015", type="row")
#' matchHierarchy(id="R-HSA-1369062", type="graph")
#' 
#' @rdname matchHierarchy
#' @family match
#' @export 

matchHierarchy <- function(id, resource="Reactome", species=NULL, type=c("row", "graph")) {
  # ensure inputs
  isisVerbose <- missing(type)
  type <- match.arg(type, several.ok=FALSE)
  
  # get class
  id.labels <- .getNodeInfo(.WHERE("dbo", id=id, databaseName=resource), "labels")
  if ("ReferenceEntity" %in% id.labels || "PhysicalEntity" %in% id.labels) {
    class <- "Entity"
  } else if ("Event" %in% id.labels) {
    class <- "Event"
  } else {
    stop("Please input an Event or Entity id", call.=FALSE)
  }
  
  # full query
  # (RE <--) PE <-- Reactions <-- Pathways
  # retrieve ALL Events linked with 'hasEvent' since it's hierarchy 
  all.MATCH.list <- list('(re:ReferenceEntity)<-[:referenceEntity]-(pe:PhysicalEntity)',
                         '(pe:PhysicalEntity)<-[:input|output|catalystActivity|regulatedBy]-(event:Event)',
                         '(event:Event)<-[:hasEvent*]-(upperevent:Event)')

  if (resource == "Reactome") {
    # throw unwanted lines
    ifelse(class == "Entity", throws <- 1, throws <- 1:2)
    MATCH.list <- all.MATCH.list[-throws]
    # select the node in WHERE
    node <- ifelse(class == "Entity", 'pe', 'event')
    # RETURN
    ifelse(class == "Event", nodes4return <- c("event", "upperevent"), 
                             nodes4return <- c("pe", "event", "upperevent"))
  } else {
    MATCH.list <- all.MATCH.list
    node <- "re" # in WHERE
    nodes4return <- c("re", "pe", "event", "upperevent") # in RETURN
  }
  
  c.MATCH <- .MATCH(MATCH.list)
  c.WHERE <- .WHERE(node, id=id, databaseName=resource, species=species)
  c.RETURN <- .RETURN(nodes4return, type, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  
  # retrieve
  .callAPI(query, .goodName(nodes4return), type, isisVerbose)
}


#' MATCH interactors
#' 
#' Retrieve interactions of a given PhysicalEntity
#' 
#' @param pe.id stId or dbId of PhysicalEntity
#' @param species name or taxon id or dbId or abbreviation of specified species
#' @param type return results as a list of dataframes (\strong{'row'}) or as a graph object (\strong{'graph'})
#' @return interactions of a given PhysicalEntity
#' @examples
#' matchInteractors(996766)
#' @rdname matchHierarchy
#' @family match
#' @export 

matchInteractors <- function(pe.id, species=NULL, type=c("row", "graph")) {
  # ensure inputs
  isVerbose <- missing(type)
  type <- match.arg(type, several.ok=FALSE)
  
  # full query
  # PhysicalEntities --> RefEntities <-- Interaction
  MATCH.list <- list('(pe:PhysicalEntity)-[:referenceEntity]->(re:ReferenceEntity)<-[:interactor]-(interaction:Interaction)')
  c.MATCH <- .MATCH(MATCH.list)
  c.WHERE <- .WHERE("pe", id=pe.id, species=species)
  nodes4return <- c("pe", "re", "interaction")
  c.RETURN <- .RETURN(nodes4return, type, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  # retrieve
  .callAPI(query, .goodName(nodes4return), type, isVerbose)
}


#' MATCH Reactions in associated Pathway
#' 
#' 
#' @param event.id stId or dbId of Event
#' @param event.name name of Event
#' @param species name or taxon id or dbId or abbreviation of specified species
#' @param type return results as a list of dataframes (\strong{'row'}) or as a graph object (\strong{'graph'})
#' @return Reactions related to the given Pathway/Reaction
#' @examples
#' matchReactionsInPathway("R-HSA-1369062", type="graph")
#' matchReactionsInPathway("R-HSA-5682285", type="row")
#' @rdname matchReactionsInPathway
#' @family match
#' @export 

matchReactionsInPathway <- function(event.id=NULL, event.name=NULL, species=NULL, type=c("row", "graph")) {
  # ensure inputs
  if (is.null(event.id) && is.null(event.name)) {
    stop("Must specify either an 'id' or a 'name'")
  }
  
  isVerbose <- missing(type)
  type <- match.arg(type, several.ok=FALSE)
  
  # get class
  event.labels <- .getNodeInfo(.WHERE("dbo", id=event.id), "labels")
  if ("Pathway" %in% event.labels) {
    event.class <- "Pathway"
  } else if ("ReactionLikeEvent" %in% event.labels) {
    event.class <- "ReactionLikeEvent"
  } else {
    stop("Please input an Event id", call.=FALSE)
  }
  
  # full query
  if (event.class == "Pathway") {
    # find Reactions connected to the given Pathway
    MATCH.list <- list('(pathway:Pathway)-[:hasEvent]->(rle:ReactionLikeEvent)')
    node4where <- "pathway"
    nodes4return <- c("pathway", "rle")
  } else if (event.class == "ReactionLikeEvent") {
    # find the Pathway connected with the given Reaction, and get other Reactions that also connect to the Pathway
    MATCH.list <- list('(rle:ReactionLikeEvent)<-[:hasEvent]-(pathway:Pathway)-[:hasEvent]->(otherReactionLikeEvent:ReactionLikeEvent)')
    node4where <- "rle"
    nodes4return <- c("rle", "pathway", "otherReactionLikeEvent")
  }
  c.MATCH <- .MATCH(MATCH.list)
  c.WHERE <- .WHERE(node4where, id=event.id, displayName=event.name, species=species)
  c.RETURN <- .RETURN(nodes4return, type, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN)
  # retrieve
  .callAPI(query, .goodName(nodes4return), type, isVerbose)
}



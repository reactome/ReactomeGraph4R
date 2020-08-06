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
  ### need to return graph data? ###
  
  # make sure the input
  if (is.null(name) && is.null(id)) stop("Must specify either an 'id' or a 'name'")
  if (missing(species)) message("Species not specified, returning human data...")
  
  # retrieve
  c.MATCH <- .MATCH(list('(dbo:DatabaseObject)'))
  c.WHERE <- .WHERE("dbo", id=id, displayName=name, speciesName=species)
  c.RETURN <- .RETURN("dbo", type="row")
  query <- paste(c.MATCH, c.WHERE, c.RETURN, collapse = ";")
  .callAPI(query, type="row")
}


#' MATCH horizontally linked Events
#' 
#' @param id a stable or db id of an Event
#' @param name an Event name
#' @param species name or taxon id or dbId or abbreviation of specified species
#' @param all.depth if set to `TRUE`, all RLEs connected to the given Event in all depths returned
#' @param depth number of depths
#' @param type to return results as a list of dataframes (\strong{'row'}) or as a graph object (\strong{'graph'})
#' @return preceding/following Events connected to the given Event in specified depth(s), default depth = 1
#' @examples
#' matchEventPaths("R-HSA-983150", all.depth=TRUE, type="row")
#' @rdname matchEventPaths
#' @family match
#' @export 

matchEventPaths <- function(id=NULL, name=NULL, species=NULL, depth=1, 
                            all.depth=FALSE, type=c("row", "graph")) {
  # ensure the inputs
  if (is.null(name) && is.null(id)) stop("Must specify either an 'id' or a 'name'")
  verbose <- ifelse(missing(type), TRUE, FALSE) 
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

  query <- paste(c.MATCH, c.WHERE, c.RETURN, collapse = ";")
  
  # call API
  tmp.id <- ifelse(is.null(id), name, id)
  new.msg <- paste0("This Event ", sQuote(tmp.id), " probably has no linked Events")
  .callAPI(query, type, verbose, new.msg)
}


#' MATCH hierarchy
#' 
#' Retrieve hierarchical data: Pathway-Reaction-Entity
#' 
#' @param id stId or dbId of Event/PhysicalEntity; or id of an external database
#' @param resource database name. All listed databases see \href{here}{https://reactome.org/content/schema/objects/ReferenceDatabase}
#' @param species name or taxon id or dbId or abbreviation of specified species
#' @param class Class of the given id. For external ids, only "Entity" can be specified
#' @param type return results as a list of dataframes (\strong{'row'}) or as a graph object (\strong{'graph'})
#' @return hierarchical instances of the given id and resource
#' @examples
#' \dontrun{
#' ### there are errors for this query using neo4r! check later ###
#' matchHierarchy(id="P04637", resource="UniProt", class="Entity", type="row")
#' }
#' matchHierarchy(id="R-HSA-196015", class="Entity", type="row")
#' matchHierarchy(id="R-HSA-1369062", class="Event", type="graph")
#' 
#' @rdname matchHierarchy
#' @family match
#' @export 

matchHierarchy <- function(id, resource="Reactome", class=c("Entity", "Event"), species=NULL, type=c("row", "graph")) {
  # ensure inputs
  if (missing(class)) stop("Please make sure what the schema class of this id is")
  class <- match.arg(class, several.ok=FALSE)
  if (resource != "Reactome" && class == "Event") stop("'Event' is only for Reactome ids")
  verbose <- ifelse(missing(type), TRUE, FALSE) 
  type <- match.arg(type, several.ok=FALSE)
  
  # full query
  # (RE <--) PE <-- Reactions <-- Pathways
  # retrieve ALL Events linked with 'hasEvent' since it's hierarchy 
  all.MATCH.list <- list('(re:ReferenceEntity)<-[:referenceEntity]-(pe:PhysicalEntity)',
                         '(pe:PhysicalEntity)<-[:input|output|catalystActivity|regulatedBy]-(event:Event)',
                         '(event:Event)<-[:hasEvent*]-(upevent:Event)')

  if (resource == "Reactome") {
    # throw unwanted lines
    ifelse(class == "Entity", throws <-  1, throws <- 1:2)
    MATCH.list <- all.MATCH.list[-throws]
    # select the node in WHERE
    node <- ifelse(class == "Entity", 'pe', 'event')
    # RETURN
    ifelse(class == "Event", nodes4return <- c("event", "upevent"), 
                             nodes4return <- c("pe", "event", "upevent"))
  } else {
    MATCH.list <- all.MATCH.list
    node <- "re" # in WHERE
    nodes4return <- c("re", "pe", "event", "upevent") # in RETURN
  }
  c.MATCH <- .MATCH(MATCH.list)
  c.WHERE <- .WHERE(node, id=id, databaseName=resource, species=species)
  c.RETURN <- .RETURN(nodes4return, type, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN, collapse = ";")
  # retrieve
  .callAPI(query, type, verbose)
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
#' \dontrun{
#' matchInteractors(996766) ### get errors using neo4r as well!! ###
#' }
#' @rdname matchHierarchy
#' @family match
#' @export 

matchInteractors <- function(pe.id, species=NULL, type=c("row", "graph")) {
  # ensure inputs
  verbose <- ifelse(missing(type), TRUE, FALSE) 
  type <- match.arg(type, several.ok=FALSE)
  
  # full query
  # PhysicalEntities --> RefEntities <-- Interaction
  MATCH.list <- list('(pe:PhysicalEntity)-[:referenceEntity]->(re:ReferenceEntity)<-[:interactor]-(i:Interaction)')
  c.MATCH <- .MATCH(MATCH.list)
  c.WHERE <- .WHERE("pe", id=pe.id, species=species)
  c.RETURN <- .RETURN(c("pe", "re", "i"), type, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN, collapse = ";")
  # retrieve
  .callAPI(query, type, verbose)
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
#' matchReactionsInPathway("R-HSA-1369062", type="row")
#' matchReactionsInPathway("R-HSA-5682285", type="graph")
#' @rdname matchReactionsInPathway
#' @family match
#' @export 

matchReactionsInPathway <- function(event.id=NULL, event.name=NULL, species=NULL, type=c("row", "graph")) {
  # ensure inputs
  if (is.null(event.id) && is.null(event.name)) stop("Must specify either an 'id' or a 'name'")
  verbose <- ifelse(missing(type), TRUE, FALSE) 
  type <- match.arg(type, several.ok=FALSE)
  
  # get class
  event.labels <- .getNodeInfo(.WHERE("dbo", id=event.id), "labels")
  event.labels <- event.labels$value
  if ("Pathway" %in% event.labels) {
    event.class <- "Pathway"
  } else if ("ReactionLikeEvent" %in% event.labels) {
    event.class <- "ReactionLikeEvent"
  } else {
    stop("Please input an Event id", call.=FALSE)
  }
  
  # full query
  # (Reactions <--) Pathway --> (other) Reactions
  if (event.class == "Pathway") {
    MATCH.list <- list('(pathway:Pathway)-[:hasEvent]->(rle:ReactionLikeEvent)')
    node4where <- "pathway"
    nodes4return <- c("pathway", "rle")
  } else if (event.class == "ReactionLikeEvent") {
    MATCH.list <- list('(rle:ReactionLikeEvent)<-[:hasEvent]-(pathway:Pathway)-[:hasEvent]->(orle:ReactionLikeEvent)')
    node4where <- "rle"
    nodes4return <- c("rle", "pathway", "orle")
  }
  c.MATCH <- .MATCH(MATCH.list)
  c.WHERE <- .WHERE(node4where, id=event.id, displayName=event.name, species=species)
  c.RETURN <- .RETURN(nodes4return, type, length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.RETURN, collapse = ";")
  # retrieve
  print(query)
  .callAPI(query, type, verbose)
}




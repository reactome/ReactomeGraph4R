# easily access graph data without Cypher

#' MATCH Reactions
#' 
#' @param id a stable or db id of a ReactionLikeEvent
#' @param name a ReactionLikeEvent name
#' @param species name or taxon id or dbId or abbreviation of species
#' @param all.depths if set to `TRUE`, all RLEs connected to the given RLE in all depths returned
#' @param depths number of depths
#' @param type to return the results as a list of dataframes or a graph object
#' @return preceding/following RLEs connected to the given RLE in specified depth(s), default depth = 1
#' @examples
#' matchReactions("R-HSA-983150", all.depths=TRUE, type="row")
#' @rdname matchReactions
#' @family match
#' @importFrom neo4r call_neo4j
#' @export 


matchReactions <- function(id=NULL, name=NULL, species=NULL, all.depths=FALSE, 
                           depths=NULL, type=c("row", "graph")) {
  # ensure the inputs
  if (is.null(name) && is.null(id)) stop("Must specify either an id or a name.")
  if (!all.depths && is.null(depths)) message("Retrieving immediate preceding/following Reactions... Specify depth-related arguments for more depths")
  if (missing(type)) message("Type argument not specified, retrieving 'row' data... For graph data, specify type='graph'")
  type <- match.arg(type, several.ok = FALSE)
  
  # show the connexion object locally
  con <- getOption("con")
  
  # write the full query
  # retrieve preceding/following reactions
  MATCH.1 <- 'MATCH p1 = (prle:ReactionLikeEvent)<-[:precedingEvent]-(rle:ReactionLikeEvent)'
  MATCH.2 <- 'MATCH p2 = (rle)<-[:precedingEvent]-(frle:ReactionLikeEvent)'
  MATCH <- paste(MATCH.1, MATCH.2, collapse = ";")
  # add depths
  if (all.depths) {
    MATCH <- gsub(":precedingEvent", ":precedingEvent*", MATCH)
  } else if (!is.null(depths)) {
    MATCH <- gsub(":precedingEvent", paste0(":precedingEvent*1..", as.integer(depths)), MATCH)
  }
  # add conditions
  WHERE <- paste0('WHERE rle', .genIdTerm(id)) 
  if (grepl("^[0-9]+$", id) && !is.null(species)) {
    WHERE <- paste0(WHERE, " AND rle.speciesName = ", .matchSpecies(species, "displayName"))
  }
  RETURN <- 'RETURN prle,rle,frle'
  # return relationships for graph data
  if (type == "graph") RETURN <- paste0(RETURN, ",relationships(p1),relationships(p2)")
  
  query <- paste(MATCH, WHERE, RETURN, collapse = ";")
  res <- call_neo4j(query, con, type=type)
  # uniq & turn into df
  if (type == "row")  res <- lapply(res, function(tbl) unique(as.data.frame(tbl)))
  res
}






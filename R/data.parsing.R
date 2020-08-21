# parse json data into R objects
# impudently copied and modified some functions from: 
# https://github.com/neo4j-rstats/neo4r/blob/master/R/api_result_parsing.R


# not include stats & meta
.parseJSON <- function(json.res, return.names, error.info=NULL) {
  # return query syntax error if any
  if ("error_code" %in% names(json.res)) {
    stop(json.res[["error_code"]], "\n", json.res[["error_message"]], call.=FALSE)
  }
  
  # transform into list
  json.list <- jsonlite::fromJSON(json.res)
  
  # Get the result element
  json.results <- as.list(json.list[[1]])
  
  # send errors if '(no changes, no records)'
  .sendErrors(json.results, error.info)
  
  # Turn NULL to NA & get the data
  res.data <- .null_to_na(json.results)
  
  .parse_row(res.data, return.names)
}


.null_to_na <- function(list) {
  for (i in 1:purrr::vec_depth(list) - 1){
    list <- purrr::modify_depth(
      list, i, function(x){
        if (is.null(x)){
          NA
        } else {
          x
        }
      }, .ragged = TRUE
    )
  }
  list
}


# Parse 'row' data

#' @importFrom magrittr %>%
#' @importFrom purrr transpose map_depth map

.parse_row <- function(res.data, res.names) {
  # flatten & discard 'meta' data
  res.data <- res.data[["row"]]
  
  # transform data (based on the logic in neo4r)
  res.data <- lapply(res.data, as.list)
  res.data <- lapply(res.data, transpose)
  t.res.data <- transpose(res.data)
  
  # manipulate lists in 2nd depth & merge these lists into a dataframe
  res <- map_depth(t.res.data, 2, .manipulate_list) %>%
          map(.rbindlist_to_df)
  
  # remove repeated rows
  res <- lapply(res, function(x) {
                      x <- unique(x)
                      rownames(x) <- 1:nrow(x) # renew row names
                      x
                     })
  # add names
  if (!is.null(res.names) && length(res) > 0) {
    if (any(grepl("\\.", res.names))) {
      # there are specified attributes of node(s) in RETURN -- e.g. dbo.dbId, dbo.displayName
      # eg: matchObject(displayName="RCOR1 [nucleoplasm]", attribute=c("dbId", "displayName"))
      
      # get unique node names
      node.names <- unique(sapply(strsplit(res.names, split="\\."), function(x) x[[1]]))
      names(res) <- .goodName(node.names)
      if (length(node.names) != length(res)) {
        stop("Check inputed return names & RETURN clause")
      }
      
      # name the columns with specified attributes
      node.names.with.dot <- unique(sapply(strsplit(res.names[grep("\\.", res.names)], split="\\."), function(x) x[[1]]))
      for (name in node.names.with.dot) {
        attr.name <- res.names[grep(name, res.names)]
        attr.name <- gsub(paste0(name, "."), "", attr.name)
        colnames(res[[.goodName(name)]]) <- attr.name
      }
    } else {
      if (length(res.names) != length(res)) {
        stop("Check inputed return names & RETURN clause")
      } else {
        names(res) <- res.names
      }
    }
  }
  res
}


.manipulate_list <- function(list) {
  # combine multiple sub-characters/lists into one list
  list <- lapply(list, function(x) {
    if (length(x) != 1 && all(!is.na(x))) {
      list(x)
    } else {
      x
    }
  })
  
  # remove NA element if any
  if (!all(!unlist(sapply(list, is.na)))) {
    list <- list[!unlist(sapply(list, is.na))] 
  }
  
  list
}


# turn list to data frame
.rbindlist_to_df <- function(list) {
  sublist.len <- sapply(list, length)
  if (length(unique(sublist.len)) == 1) { # all lengths of sublists are same
    as.data.frame(data.table::rbindlist(list))
  } else {
    as.data.frame(data.table::rbindlist(list, fill = TRUE))
  }
}


# include relationships in 'row' data
.processRowOutput <- function(res.list) {
  # get data
  row <- res.list[["row"]]
  graph <- res.list[["graph"]]
  nodes <- as.data.frame(graph[["nodes"]])
  relationships <- as.data.frame(graph[["relationships"]])
  
  # add columns in relationships
  # grab the slots from properties list
  for (node in c("startNode", "endNode")) {
    for (slot in c("dbId", "schemaClass")) {
      col.name <- paste0(node, ".", slot)
      relationships[, col.name] <- sapply(as.character(relationships[, node]), function(x) 
                                                    nodes[nodes$id == x, ]$properties[[1]][[slot]])               
    }
  }
  
  # rearrange columns
  relationships <- relationships[ ,c("id", "type", "startNode", "startNode.dbId", "startNode.schemaClass",
                                     "endNode", "endNode.dbId", "endNode.schemaClass", "properties")]
  # rename columns
  colnames(relationships)[which(colnames(relationships) %in% c("id", "startNode", "endNode"))] <- c("neo4jId", "startNode.neo4jId", "endNode.neo4jId")
  
  # add relationships to row list
  row[["relationships"]] <- as.data.frame(relationships)
  row
}



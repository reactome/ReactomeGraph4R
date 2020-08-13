# parse json data into R objects
# impudently copied and modified some functions from: 
# https://github.com/neo4j-rstats/neo4r/blob/master/R/api_result_parsing.R

# Note: neo4r 0.1.3 doesn't return syntax errors of cypher queries when format='json'

# not include stats & meta
.parseJSON <- function(json.res, return.names, type, msg=NULL) {
  # transform into list
  json.list <- jsonlite::fromJSON(json.res)
  
  # Get the result element
  json.results <- as.list(json.list[[1]])
  
  # check & send error/msg if any
  .errMsg(json.results, msg)
  
  # Turn NULL to NA & get the data
  res.data <- .null_to_na(json.results)
  
  # Get the name of the return elements
  res.names <- return.names
  
  if (type == "row") {
    .parse_row(res.data, res.names)
  } else if (type == "graph") {
    .parse_graph(res.data)
  }
}


# error messages
.errMsg <- function(res, msg=NULL) {
  if (length(res) == 0) {
    # (no changes, no records)
    message("No data returned")
    # send custom info
    if(!is.null(msg)) stop(msg, call.=FALSE)
  } else if ("error_message" %in% names(res)) {
    # query syntax error
    stop(paste0(res[["error_code"]], "\n", res[["error_message"]]), call.=FALSE)
  }
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
  if (!is.null(res.names) && length(res.names) != length(res)) {
    stop("Check inputed return names & RETURN clause") # internal check
  } else if (!is.null(res.names)) {
    names(res) <- res.names
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


# parse 'graph' data
.parse_graph <- function(res.data) {
  # Get the elements
  graph <- as.list(res.data[["graph"]])
  nodes <- graph[["nodes"]]
  relations <- graph[["relationships"]]
  
  # Verify that there is something to return
  if (length(nodes) == 0 & length(relations) == 0) {
    message("No graph data found.")
    message("Either your call can't be converted to a graph \nor there is no data at all matching your call.")
    message("Verify your call or try type = \"row\".")
  }
  
  # Do something only if there are nodes
  if (length(nodes) != 0) {
    res.nodes <- .operate_list(nodes)
  }
  
  # Do something only if there are relations
  if (length(relations) != 0) {
    res.relations <- .operate_list(relations)
  }
  
  # combine nodes & relationships
  res <- purrr::compact(
    list(
      nodes = res.nodes,
      relationships = res.relations
    )
  )
  res
}

# handle nodes & relationships data
.operate_list <- function(list) {
  ## not sure if there are other situations ##
  
  # select the one having maximum number of nodes/relationships
  max.one <- list[which.max(sapply(list, function(x) nrow(x)))]
  max.one <- as.list(max.one[[1]])
  
  # turn dataframe into separate list
  max.one[["properties"]] <- apply(max.one[["properties"]], 1, as.list)
  
  # turn final results into a dataframe
  res.list <- as.data.frame(do.call(cbind, max.one))
  
  # sort by id
  res.list$id <- as.integer(res.list$id)
  res.list[order(res.list$id), ]
}



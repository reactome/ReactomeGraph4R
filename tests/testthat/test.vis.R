context("network graph")


test_that("Visualize returned graph object", {
  graph <- matchPrecedingAndFollowingEvents("R-HSA-983150", depth=2, type="graph")
  edges <- graph$relationships
  nodes <- graph$nodes
  nodes <- unnestListCol(df = nodes, column = "properties")
  
  library(visNetwork)
  # Transform into visNetwork format for nodes & edges
  vis.nodes <- data.frame(id = nodes$id, 
                          label = nodes$displayName, # truncate the long names
                          group = nodes$schemaClass, 
                          title = paste0("<p><b>", nodes$schemaClass, "</b><br>", 
                                         "dbId: ", nodes$dbId, "<br>", nodes$displayName, "</p>"))
  
  vis.edges <- data.frame(from = edges$startNode,
                          to = edges$endNode,
                          label = edges$type)
  
  vis <- visNetwork(vis.nodes, vis.edges)
  expect_is(vis, c("visNetwork", "htmlwidget"))
  expect_output(str(vis), "List of 8")
})

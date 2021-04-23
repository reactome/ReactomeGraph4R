context("network graph")

library(visNetwork)
load("for.test.results.Rdata")

test_that("Visualize returned graph object", {
  edges <- graph$relationships
  nodes <- graph$nodes
  nodes <- unnestListCol(df = nodes, column = "properties")
  
  # Transform into visNetwork format for nodes & edges
  vis.nodes <- data.frame(id = nodes$id, 
                          label = nodes$displayName, # truncate the long names
                          group = nodes$schemaClass, 
                          title = "Test")
  
  vis.edges <- data.frame(from = edges$startNode,
                          to = edges$endNode,
                          label = edges$type)
  
  vis <- visNetwork(vis.nodes, vis.edges)
  expect_s3_class(vis, c("visNetwork", "htmlwidget"))
  expect_output(str(vis), "List of 8")
})

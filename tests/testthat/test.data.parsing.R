context("data parsing")


# not sure if these tests can pass if there is no local GraphDB installed
# here is just some simple tests for the final results, they should have been divided into different steps


test_that("parsed 'row' data returned from neo4r", {
  # `matchObject` only returns row data
  try <- matchReferrals("R-HSA-112479", main=FALSE, depth=3, type="row")
  
  expect_is(try, "list")
  expect_output(str(try), "List of 3")
  expect_equal(names(try), c("PhysicalEntity", "databaseObject", "relationships"))
})



test_that("graph data returned from neo4r", {
  graph <- matchHierarchy(id="1369062", species="human", type="graph")
  unnested.nodes <- unnestListCol(graph$nodes, "properties")
  
  expect_is(graph, "list")
  expect_equal(names(graph), c("nodes", "relationships"))
  expect_is(graph$nodes, "data.frame")
  expect_is(graph$relationships, "data.frame")
  expect_is(unnested.nodes, "data.frame")
})


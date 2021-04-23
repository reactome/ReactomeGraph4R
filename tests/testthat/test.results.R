context("check results")

# row <- matchReferrals("R-HSA-112479", main=FALSE, depth=3, type="row")
# graph <- matchReferrals("R-HSA-112479", main=FALSE, depth=3, type="graph")
# save(row, graph, file = "tests/testthat/for.test.results.Rdata")

load("for.test.results.Rdata")

test_that("check the 'row' data", {
  expect_type(row, "list")
  expect_output(str(row), "List of 3")
  expect_equal(names(row), c("PhysicalEntity", "databaseObject", "relationships"))
})

test_that("check the 'graph' data returned from neo4r", {
  expect_type(graph, "list")
  expect_equal(names(graph), c("nodes", "relationships"))
  expect_is(graph$nodes, "data.frame")
  expect_is(graph$relationships, "data.frame")
  # unnest
  unnested.nodes <- unnestListCol(graph$nodes, "properties")
  expect_is(unnested.nodes, "data.frame")
})

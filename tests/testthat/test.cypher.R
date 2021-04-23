context("check cypher query")

## Basically check those hidden functions

test_that("check cyphers", {
  # glue a query
  relationships <- ":hasEvent|modification"
  class <- "Event"
  MATCH.list <- list(paste0('(dbo:DatabaseObject)-[', relationships, ']->(n:', class, ')'))
  c.MATCH <- .MATCH(MATCH.list)
  # c.MATCH <- .varLen(clause=c.MATCH, rel=relationships, 
  #                    depth=1, all=FALSE)
  # c.WHERE <- .WHERE("n", id="113454", displayName=NULL, speciesName=NULL)
  c.WHERE <- 'WHERE n.dbId = 113454'
  # filter out unwanted nodes
  c.WITH <- 'WITH *, nodes(p1) AS ns'
  c.WHERE.2 <- 'WHERE ALL(node IN ns WHERE NOT node:InstanceEdit)'
  c.RETURN <- .RETURN(c("n", "dbo"), length(MATCH.list))
  query <- paste(c.MATCH, c.WHERE, c.WITH, c.WHERE.2, c.RETURN)
  
  expect_type(query, "character")
  expect_equal(query, "MATCH p1 = (dbo:DatabaseObject)-[:hasEvent|modification]->(n:Event) WHERE n.dbId = 113454 WITH *, nodes(p1) AS ns WHERE ALL(node IN ns WHERE NOT node:InstanceEdit) RETURN n,dbo,relationships(p1)")
})

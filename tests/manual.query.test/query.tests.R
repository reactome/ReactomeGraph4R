# manually run the following tests and check the results

library(ReactomeGraph4R)
login() # already built a connection to the reactome neo4j database

# fetch instance by class
url <- matchObject(schemaClass = "URL")
head(url[["databaseObject"]])

# check error
matchObject(schemaClass = "haha")

# fetch instance by name
matchObject(displayName = "RCOR1 [nucleoplasm]", 
            returnedAttributes=c("stId", "speciesName"))

# fetch instance by id
## Reactome id
matchObject(id = "R-HSA-9626034")

# fetch multiple ids
res <- multiObjects(ids = c("P02741", "Q9GZQ8"), 
                    databaseName="UniProt", speedUp=TRUE)
res[, 1:5]

# get the preceding/following Events
matchPrecedingAndFollowingEvents("R-HSA-983150", depth=2, type="row")

# get hierarchy
matchHierarchy(id="R-HSA-1369062", type="graph")

# get interactors
matchInteractors(996766)

# get reactions associated with a pathway
matchReactionsInPathway("R-HSA-5682285", type="row")

# get referrals
matchReferrals("113454", type="row")

# get PhysicalEntity roles
matchPEroles(pe.id = "R-HSA-8944354", type = "graph")

# get diseases
matchDiseases(displayName="neuropathy", species="M. musculus", type="row")

# fetch Reactome instances using a pubmed id
matchPaperObjects(pubmed.id="20797626", type="graph")

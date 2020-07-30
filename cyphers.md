## Cypher queries of the working list

### WIP


- Get all interactors of a PhysicalEntity (EWAS)
	- ‘interactor’ connection, Interaction class

```
MATCH (pe:PhysicalEntity)-[:referenceEntity]->(re:ReferenceEntity)<-[itr:interactor]-(i:Interaction)
WHERE pe.dbId = 996766
RETURN pe,re,i,itr
```

---

- Get all Reactions in associated Pathway
	- Submit a Pathway name/id, retrieve via ‘hasEvent’ connection

```
MATCH path = (p:Pathway)-[r:hasEvent]->(rle:ReactionLikeEvent)
WHERE p.stId = "R-HSA-1369062"
RETURN rle,relationships(path)
```


  - Submit a Reaction name/id, find Pathway(s) via ‘hasEvent’ connection, and then find all Reactions

```
MATCH path1 = (rle:ReactionLikeEvent)<-[r:hasEvent]-(p:Pathway)
MATCH path2 = (p)-[:hasEvent]->(rles:ReactionLikeEvent)
WHERE rle.stId = "R-HSA-5682285"
RETURN rles
```

---

- Fetch Instances by Attribute
	- Example: Find all ‘Complexes with species Homo sapiens’
	- Example: Find all ‘Pathways with diagram’
	- Error handling for Invalid Attributes (eg: ‘hasComponent’ is not a valid attribute for EntitySets)

_how to get all attributes for instances in one Class_

```
MATCH (n:Complex)
UNWIND keys(n) AS k
RETURN distinct(k)
```
```
MATCH (n:Complex)
WHERE n.speciesName = "Homo sapiens"
RETURN n LIMIT 10
```
Hmmm how to send errors?

---

- Fetch instance by DBID/StId/Name (basic query)

```
MATCH (n)
WHERE n.displayName = "RCOR1 [nucleoplasm]"
RETURN n

MATCH (n {dbId: 996766})
RETURN n
```

---
- Get Pathway hierarchy associated with an identifier (Reactome or external)
	- Link identifier to PhysicalEntities, find Reactions and Pathways associated with PhysicalEntities

_Differentiate Reactome & external ID_

```
MATCH (re:ReferenceEntity)<-[r:referenceEntity]-(pe:PhysicalEntity)
WHERE re.databaseName = "UniProt" AND re.identifier = "P04637"
WITH pe
MATCH (pe)-[:input|output|catalystActivity|regulatedBy]-(event:Event)
RETURN event
```

```
MATCH (pe:PhysicalEntity)-[:input|output|catalystActivity|regulatedBy]-(e:Event)
WHERE pe.stId = "R-HSA-196015"
RETURN e
```

---

- Find Referrals of instance
	- Focus on ‘Biological’ referrals (Events, PhysicalEntities, Regulations, Catalysts, ModifiedResidues)

_No idea_

---

- Find existing ‘roles’ of PhysicalEntity (input, output, regulator, catalyst)

```
MATCH p=(pe:PhysicalEntity)<-[:input|output|catalystActivity|regulatedBy*]-(do:DatabaseObject)
WHERE pe.stId = "R-HSA-8944354"
RETURN pe,do,relationships(p)
```
_how to know the exact relationship??_

---

- Find Diseases associated with PhysicalEntity/Reaction/Pathways
  - Reverse: Find PhysicalEntities/Reactions/Pathways associated with Disease

```
MATCH (pathway:Pathway)
WHERE pathway.stId = "R-HSA-1369062"

CALL apoc.do.when(
  pathway.isInDisease,
  'RETURN pathway.disease',
  'RETURN "No associative disease"',
  pathway:pathway)
YIELD value
RETURN value
```
I want to use the 'if/else' in cypher, but an error returned:

```
Neo.ClientError.Procedure.ProcedureNotFound
There is no procedure with the name `apoc.do.when` registered for this database instance. Please ensure you've spelled the procedure name correctly and that the procedure is properly deployed.
```
https://neo4j.com/docs/labs/apoc/current/cypher-execution/conditionals/

```
MATCH (d:Disease)<-[r:disease]-(pe:PhysicalEntity)
WHERE d.displayName = "cancer" AND pe.speciesName = "Homo sapiens"
RETURN pe LIMIT 50
```

---

- Search for papers in Reactome
	- Start with LiteratureReference class and check ‘literatureReference’ connections to Events, and output associated events


```
MATCH (lr:LiteratureReference)<-[r:literatureReference]-(event:Event)
WHERE lr.pubMedIdentifier = 20797626
RETURN event,r
```


**ERROR HANDLING** - it's so inconvenient that Neo4j doesn't return any error message... 


## Cypher queries of the working list

### WIP


- Get all interactors of a PhysicalEntity (EWAS)
	- ‘interactor’ connection, Interaction class

```
MATCH p1 = (pe:PhysicalEntity)-[:referenceEntity]->(re:ReferenceEntity)<-[:interactor]-(i:Interaction)
WHERE pe.dbId = 996766
RETURN pe,re,i,relationships(p1)
```

---

- Get all Reactions in associated Pathway
	- Submit a Pathway name/id, retrieve via ‘hasEvent’ connection

```
MATCH p1 = (p:Pathway)-[:hasEvent]->(rle:ReactionLikeEvent)
WHERE p.stId = "R-HSA-1369062"
RETURN p,rle,relationships(p1)
```


  - Submit a Reaction name/id, find Pathway(s) via ‘hasEvent’ connection, and then find all Reactions

```
MATCH p = (rle:ReactionLikeEvent)<-[:hasEvent]-(pathway:Pathway)-[:hasEvent]->(rles:ReactionLikeEvent)
WHERE rle.stId = "R-HSA-5682285"
RETURN rle,pathway,rles,relationships(p)
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
WHERE n.displayName = "RCOR1 [nucleoplasm]" AND n.speciesName = "Homo sapiens"
RETURN n

MATCH (n {dbId: 996766})
RETURN n
```

---
- Get Pathway hierarchy associated with an identifier (Reactome or external)
	- Link identifier to PhysicalEntities, find Reactions and Pathways associated with PhysicalEntities

_Differentiate Reactome & external ID_

```
MATCH p1 = (re:ReferenceEntity)<-[:referenceEntity]-(pe:PhysicalEntity)
MATCH p2 = (pe)<-[:input|output|catalystActivity|regulatedBy]-(event:Event)
WHERE re.databaseName = "UniProt" AND re.identifier = "P04637"
RETURN re,pe,event,relationships(p1) AS r1,relationships(p2) AS r2
```

```
MATCH p1 = (pe:PhysicalEntity)<-[:input|output|catalystActivity|regulatedBy]-(event:Event)
WHERE pe.stId = "R-HSA-196015"
RETURN pe,event,relationships(p1)
```

**Hierarchy**: use the function in the last PR (replace rle with event)

---

- Find Referrals of instance
	- Focus on ‘Biological’ referrals (Events, PhysicalEntities, Regulations, Catalysts, ModifiedResidues)

_No idea_

---

- Find existing ‘roles’ of PhysicalEntity (input, output, regulator, catalyst)

```
MATCH p=(pe:PhysicalEntity)<-[:input|output|catalystActivity|regulatedBy]-(do:DatabaseObject)
WHERE pe.stId = "R-HSA-8944354"
RETURN pe,do,relationships(p)
```

---

- Find Diseases associated with PhysicalEntity/Reaction/Pathways
  - Reverse: Find PhysicalEntities/Reactions/Pathways associated with Disease

```
MATCH (pathway:Pathway)
WHERE pathway.stId = "R-HSA-162588"
RETURN pathway,
CASE
WHEN pathway.isInDisease = true
THEN [(pathway)-[:disease]->(d:Disease) | d]
ELSE "No relevant disease" END AS disease
```

```
MATCH p1 = (d:Disease)<-[:disease]-(pe:PhysicalEntity)
WHERE d.displayName = "cancer" AND pe.speciesName = "Homo sapiens"
RETURN d,pe,relationships(p1) LIMIT 50
```

---

- Search for papers in Reactome
	- Start with LiteratureReference class and check ‘literatureReference’ connections to Events, and output associated events


```
MATCH p1 = (lr:LiteratureReference)<-[:literatureReference]-(event:Event)
WHERE lr.pubMedIdentifier = 20797626
RETURN lr,event,relationships(p1)
```


**ERROR HANDLING** - it's so inconvenient that Neo4j doesn't return any error message... 


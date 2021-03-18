module Data.Abnormal.Graph where 

import Data.Argonaut (Json)
import Data.Map (Map, insert)
import Data.Array (snoc)
import Foreign.Object (Object)

type EntityId = String
type CacheKey = String
type PropertyName = String

data NodeData =
      CachedObject
    | CachedOperation
    | ValueNull
    | ValueBoolean Boolean
    | ValueString String
    | ValueNumber Number
    | ValueUncacheableObject
    | ValueArray

data EdgeData =
      ReferenceEdge
    | PropertyEdge PropertyName
    | IndexEdge Int

data Node = Node EntityId NodeData

data EdgeConnection = EdgeConnection EntityId EntityId
data Edge = Edge EntityId EdgeConnection EdgeData

data Graph = Graph (Map EntityId Node) (Array Edge)

addEdge :: Graph -> Edge -> Graph
addEdge (Graph nodes edges) edge
    = 
        let
            newEdges = snoc edges edge
        in
            Graph nodes newEdges

-- check if exact edge exists
-- if it does do not add it

addNode :: Graph -> Node -> Graph
addNode (Graph nodes edges) (Node entityId nodeData)
    = 
        let
            newGraph = insert entityId (Node entityId nodeData) nodes
        in
            Graph newGraph edges

hasNode :: Graph -> EntityId -> Boolean
hasNode g e =   
 -- when adding a noce check if it exists already
 -- if it exists delete the old node
 -- delete outgoing edges
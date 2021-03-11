module Data.Abnormal.Graph where 

import Data.Argonaut (Json)
import Data.Map (Map, insert)
import Data.Array (snoc)

type EntityId = String
type CacheKey = String
type PropertyName = String

data NodeData =
      CachedObject CacheKey
    | CachedOperation CacheKey
    | ValuePrimitive Json
    | ValueArray

data EdgeData =
      ReferenceEdge
    | PropertyEdge PropertyName
    | IndexEdge Number

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

addNode :: Graph -> Node -> Graph
addNode (Graph nodes edges) (Node entityId nodeData)
    = 
        let
            newGraph = insert entityId (Node entityId nodeData) nodes
        in
            Graph newGraph edges


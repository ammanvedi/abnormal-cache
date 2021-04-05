module Data.Abnormal.Graph where 

import Data.Array (snoc, filter)
import Data.Eq (eq, class Eq, (/=))
import Data.Foldable (foldl)
import Data.Map (Map, insert, values, delete, member)
import Data.Show (show, class Show)
import Data.Tuple (Tuple(..))
import Prelude ((<>), (==), (&&))

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

instance eqEdgeData :: Eq EdgeData where
    eq ReferenceEdge ReferenceEdge = true
    eq (PropertyEdge pa) (PropertyEdge pb) = pa == pb
    eq (IndexEdge ia) (IndexEdge ib) = ia == ib
    eq _ _ = false

instance showNodeData :: Show NodeData where
    show CachedObject = "CachedObject"
    show CachedOperation = "CachedOperation"
    show ValueUncacheableObject = "UncacheableObject"
    show ValueArray = "Array"
    show ValueNull = "Null"
    show (ValueBoolean bool) = "Boolean (" <> show bool <> ")"
    show (ValueString str) = "String (" <> show str <> ")"
    show (ValueNumber num) = "Number (" <> show num <> ")"

instance eqNodeData :: Eq NodeData where
    eq CachedObject CachedObject = true
    eq CachedOperation CachedOperation = true
    eq ValueUncacheableObject ValueUncacheableObject = true
    eq ValueArray ValueArray = true
    eq ValueNull ValueNull = true
    eq (ValueBoolean boolA) (ValueBoolean boolB) = boolA == boolB
    eq (ValueString strA) (ValueString strB) = strA == strB
    eq (ValueNumber numA) (ValueNumber numB) = numA == numB
    eq _ _ = false

data Node = Node EntityId NodeData

instance showNode :: Show Node where
    show (Node id nodeData) = "Node (" <> id <> ") " <> show nodeData <> "\n"

instance eqNode :: Eq Node where
    eq (Node idA nodeDataA) (Node idB nodeDataB) = idA == idB && nodeDataA == nodeDataB

data EdgeConnection = EdgeConnection EntityId EntityId

instance eqEdgeConnection :: Eq EdgeConnection where
    eq (EdgeConnection a1 b1) (EdgeConnection a2 b2) = 
        a1 == a2 && b1 == b2

instance showEdgeConnection :: Show EdgeConnection where
    show (EdgeConnection idA idB) = idA <> " -> " <> idB

data Edge = Edge EntityId EdgeConnection EdgeData

instance eqEdge :: Eq Edge where
    eq (Edge idA connectionA dataA) (Edge idB connectionB dataB) =
        idA == idB && connectionA == connectionB && dataA == dataB

instance showEdge :: Show Edge where
    show (Edge id connection ReferenceEdge) = "ReferenceEdge (" <> id <> ") (" <> show connection <> ") \n"
    show (Edge id connection (PropertyEdge name)) = "PropertyEdge " <> name <> " (" <> show connection <> ")\n"
    show (Edge id connection (IndexEdge ix)) = "IndexEdge " <> (show ix) <> " (" <> show connection <> ")\n"

data Graph = Graph (Map EntityId Node) (Array Edge)

instance eqGraph :: Eq Graph where
    eq (Graph nodesA edgesA) (Graph nodesB edgesB) = 
        (nodesA == nodesB) && (edgesA == edgesB)

instance showGraph :: Show Graph where
    show (Graph nodes edges) = 
        let
            nodeString = foldl (\acc node -> acc <> show node) "\n" (values nodes)
            edgeString = foldl (\acc edge -> acc <> show edge) "\n" edges
        in
            nodeString <> "\n \n" <> edgeString


addEdge :: Graph -> Edge -> Graph
addEdge (Graph nodes edges) edge
    = 
        let
            newEdges = snoc edges edge
        in
            Graph nodes newEdges

addNode :: Graph -> Node -> Graph
addNode g (Node entityId nodeData)
    = 
        let
            (Graph nodes edges) = 
                (removeOutgoingEdges (removeNode g entityId) entityId)
            newNodeGraph = insert entityId (Node entityId nodeData) nodes
        in
            Graph newNodeGraph edges

hasNode :: Graph -> EntityId -> Boolean
hasNode (Graph nodes edges) id =   
    member id nodes

removeOutgoingEdges :: Graph -> EntityId -> Graph
removeOutgoingEdges (Graph nodes edges) fromNode
    = Graph nodes filteredEdges
        where
            filteredEdges = filter (\(Edge id (EdgeConnection from to) edgeData) -> from /= fromNode ) edges

removeNode :: Graph -> EntityId -> Graph
removeNode (Graph nodes edges) id =
    Graph (delete id nodes) edges
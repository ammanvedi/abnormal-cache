module Data.Abnormal.StructureAnalysis where

import Data.Abnormal.Graph (CacheKey, Graph(..), NodeData(..), EntityId, addNode, addEdge, Node(..))
import Data.Argonaut (Json, caseJson)
import Data.Map (empty)
import Foreign.Object (Object)

createGraph :: Graph
createGraph = Graph empty []

addObjectToGraph :: Graph -> EntityId -> Object Json -> Graph
addObjectToGraph graph parentId object = graph

addPrimitiveNullToGraph :: Graph -> EntityId -> Graph
addPrimitiveNullToGraph graph parentId = graph

addPrimitiveBoolToGraph :: Graph -> EntityId -> Boolean -> Graph
addPrimitiveBoolToGraph graph parentId value = graph

addPrimitiveNumToGraph :: Graph -> EntityId -> Number -> Graph
addPrimitiveNumToGraph graph parentId value = graph

addPrimitiveStrToGraph :: Graph -> EntityId -> String -> Graph
addPrimitiveStrToGraph graph parentId value = graph

addArrayToGraph :: Graph -> EntityId -> Array Json -> Graph
addArrayToGraph graph parentId arr = graph

handleJsonValue :: Graph -> EntityId -> Json -> Graph
handleJsonValue graph parentId json =
    caseJson
        (\u -> addPrimitiveNullToGraph graph parentId)
        (\b -> addPrimitiveBoolToGraph graph parentId b)
        (\n -> addPrimitiveNumToGraph graph parentId n)
        (\s -> addPrimitiveStrToGraph graph parentId s)
        (\a -> addArrayToGraph graph parentId a)
        (\o -> addObjectToGraph graph parentId o)
        json


-- isCacheableObject :: Object Json -> Boolean



addOperationResultToGraph :: Graph -> CacheKey -> Json -> Graph
addOperationResultToGraph graph operationKey rootData 
    = 
        let 
            operationNode = Node operationKey ( CachedOperation operationKey )
            graphPrime = addNode graph operationNode
        in
            handleJsonValue graphPrime operationKey rootData

module Data.Abnormal.StructureAnalysis where

import Data.Abnormal.Graph (CacheKey, Edge(..), EdgeConnection(..), EdgeData(..), Graph(..), Node(..), NodeData(..), EntityId, addEdge, addNode)
import Data.Argonaut (Json, caseJson, caseJsonString)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (empty)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Foreign.Object (Object, lookup, fold)
import Prelude ((<>))
import Debug.Trace (spy)

createGraph :: Graph
createGraph = Graph empty []

getEdgeId :: EntityId -> EntityId -> (Either String Int) -> String
getEdgeId parentId nodeId propertyName 
    = parentId <> "_" <> nodeId <> "_" <> propertyNameComponent
    where
        propertyNameComponent = 
            case propertyName of 
                Left str -> str
                Right index -> show index

addValueNode :: Graph -> Node -> EntityId -> EntityId -> (Either String Int) -> Graph
addValueNode graph node parentId nodeId propertyName =
    let
        edgeId = getEdgeId parentId nodeId propertyName
        newEdgeConnection = EdgeConnection parentId nodeId
        edgeData =
            case propertyName of
                Left str -> PropertyEdge str
                Right index -> IndexEdge index
        newEdge = Edge edgeId newEdgeConnection edgeData
    in
        addEdge (addNode graph node) newEdge

hasIdentity :: Object Json -> Maybe String
hasIdentity obj =
    case lookup "id" obj of
        (Just jsonValue) -> 
            caseJsonString Nothing (\s -> Just s) jsonValue
        (Nothing) -> Nothing

genKeyString :: String -> String -> String
genKeyString key typeName = 
    "[" <> key <> ":" <> typeName <> "]"

genCacheKey :: Object Json -> String -> EntityId
genCacheKey obj id = 
    fold (\accKey key json ->
            caseJson
                (\u -> accKey <> genKeyString key "null")
                (\b -> accKey <> genKeyString key "boolean")
                (\n -> accKey <> genKeyString key "number")
                (\s -> accKey <> genKeyString key "string")
                (\a -> accKey <> genKeyString key "array")
                (\o -> accKey <> genKeyString key "object")
                json
        ) (id <> "::") obj

addObjectToGraph :: Graph -> EntityId -> EntityId -> (Either String Int) -> Object Json -> Graph
addObjectToGraph graph parentId nodeId propertyName object =
    let 
        newNodeId = 
            case hasIdentity object of
                (Just id) -> genCacheKey object id
                Nothing -> nodeId
        newNode = 
            case hasIdentity object of
                (Just id) -> Node newNodeId CachedObject
                Nothing -> Node newNodeId ValueUncacheableObject
        baseGraph = addValueNode graph newNode parentId newNodeId propertyName
    in 
        fold (\accGraph key json -> 
            let 
                x = spy key "avv"
            in
                (handleJsonValue accGraph newNodeId (Left key) json)
            ) baseGraph object


addPrimitiveNullToGraph :: Graph -> EntityId -> EntityId -> (Either String Int) -> Graph
addPrimitiveNullToGraph graph parentId nodeId propertyName =
    let
        newNode = Node nodeId ValueNull
    in
        addValueNode graph newNode parentId nodeId propertyName

addPrimitiveBoolToGraph :: Graph -> EntityId -> EntityId -> (Either String Int) -> Boolean -> Graph
addPrimitiveBoolToGraph graph parentId nodeId propertyName value =
    let
        newNode = Node nodeId (ValueBoolean value)
    in
        addValueNode graph newNode parentId nodeId propertyName

addPrimitiveNumToGraph :: Graph -> EntityId -> EntityId -> (Either String Int) -> Number -> Graph
addPrimitiveNumToGraph graph parentId nodeId propertyName value =
    let
        newNode = Node nodeId (ValueNumber value)
    in
        addValueNode graph newNode parentId nodeId propertyName

addPrimitiveStrToGraph :: Graph -> EntityId -> EntityId -> (Either String Int) -> String -> Graph
addPrimitiveStrToGraph graph parentId nodeId propertyName value = 
    let
        newNode = Node nodeId (ValueString value)
    in
        addValueNode graph newNode parentId nodeId propertyName

addArrayToGraph :: Graph -> EntityId -> EntityId -> (Either String Int) -> Array Json -> Graph
addArrayToGraph graph parentId nodeId propertyName arr = 
    let
        newNode = Node nodeId ValueArray
        baseGraph = addValueNode graph newNode parentId nodeId propertyName
    in
        foldlWithIndex
            (\ix accGraph json -> handleJsonValue accGraph nodeId (Right ix) json)
            baseGraph
            arr

handleJsonValue :: Graph -> EntityId -> (Either String Int) -> Json -> Graph
handleJsonValue graph parentEntityId propertyName json =
    caseJson
        (\u -> addPrimitiveNullToGraph graph parentEntityId thisNodeEntityId propertyName)
        (\b -> addPrimitiveBoolToGraph graph parentEntityId thisNodeEntityId propertyName b)
        (\n -> addPrimitiveNumToGraph graph parentEntityId thisNodeEntityId propertyName n)
        (\s -> addPrimitiveStrToGraph graph parentEntityId thisNodeEntityId propertyName s)
        (\a -> addArrayToGraph graph parentEntityId thisNodeEntityId propertyName a)
        (\o -> addObjectToGraph graph parentEntityId thisNodeEntityId propertyName o)
        json
    where
        propertyNameComponent =
            case propertyName of 
                Left str -> str
                Right index -> show index
        thisNodeEntityId = parentEntityId <> "_" <> propertyNameComponent
        y = spy thisNodeEntityId "abc"

addOperationResultToGraph :: Graph -> CacheKey -> Json -> Graph
addOperationResultToGraph graph operationKey rootData 
    = 
        let 
            operationNode = Node operationKey CachedOperation
            graphPrime = addNode graph operationNode
        in
            handleJsonValue graphPrime operationKey (Left "root") rootData

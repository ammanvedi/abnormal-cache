module Data.Abnormal.StructureAnalysis where

import Data.Tuple

import Data.Argonaut (Json, caseJson, fromObject, fromString, jsonEmptyObject, stringify, toObject)
import Data.Array (snoc, concat)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Data.Traversable (Accum, foldl, mapAccumL)
import Foreign.Object as FO
import Prelude (otherwise, ($), (&&), (<>), (==))

type ObjKey = String

data ObjValue
    = ValEmpty
    | ValBoolean Boolean
    | ValString String
    | ValNumber Number
    | ValObject (FO.Object Json) ObjSchema

instance objValEq :: Eq ObjValue where
    eq ValEmpty ValEmpty = true
    eq (ValBoolean a) (ValBoolean b) = a == b
    eq (ValString a) (ValString b) = a == b
    eq (ValNumber a) (ValNumber b) = a == b
    eq (ValObject a schemaA) (ValObject b schemaB) = schemaA == schemaB
    eq _ _ = false

instance objValShow :: Show ObjValue where
    show (ValBoolean b)         = "BOOLEAN"
    show ValEmpty               = "EMPTY"
    show (ValString s)          = "STRING"
    show (ValNumber n)          = "NUMBER"
    show (ValObject o schema)   = show schema

data ObjProperty = ObjProperty ObjKey ObjValue

instance eqObjProperty :: Eq ObjProperty where
    eq (ObjProperty ka va) (ObjProperty kb vb) = keyMatch && valMatch
        where
            keyMatch = ka == kb
            valMatch = va == vb

instance objPropertyShow :: Show ObjProperty where
    show (ObjProperty k v) = k <> "::" <> show v

type ObjSchema = Array ObjProperty

handleTraverseJsonObject :: FO.Object Json -> FO.Object Json -> String -> ObjProperty
handleTraverseJsonObject parent child k
     = ObjProperty k (ValObject newO childSchema)
    where
        childSchema = traverseJson child
        keyValue = fromString (genCacheKey childSchema)
        newO = FO.insert k keyValue parent

traverseJsonPosition :: Json -> ObjSchema -> (Tuple String Json) -> Accum ObjSchema Json
traverseJsonPosition parent schema (Tuple key json) = { accum: (snoc schema posResult), value: parent }
    where
        jsonAsObject =
            case toObject parent of
                (Just j) -> j
                Nothing -> FO.empty
        posResult = caseJson
                (\u -> ObjProperty key ValEmpty)
                (\b -> ObjProperty key (ValBoolean b))
                (\n -> ObjProperty key (ValNumber n))
                (\s -> ObjProperty key (ValString s))
                (\a -> ObjProperty key ValEmpty)
                (\o -> handleTraverseJsonObject jsonAsObject o key)
                json


traverseJson :: FO.Object Json -> ObjSchema
traverseJson o = tResult.accum
    where
        withKeys = FO.toArrayWithKey (\k v -> (Tuple k v)) o
        tResult = mapAccumL (traverseJsonPosition (fromObject o)) [] withKeys

type CacheKey = String

newtype CacheReference 
    = CacheReference CacheKey

data NormalisedCacheObject 
    = NormalisedCacheObject CacheKey (FO.Object Json)
    | EmptyNormalisedObject

instance showNormalisedCacheObject :: Show NormalisedCacheObject where
    show (NormalisedCacheObject key o) = "\n KEY :: " <> key <> "\n RAW :: " <> (stringify (fromObject o)) <> "\n"
    -- show (NormalisedCacheObject key o) = "\n" <> key
    show EmptyNormalisedObject = "EMPTY_CACHE_OBJECT"

instance eqNormalisedCacheObject :: Eq NormalisedCacheObject where
    eq (NormalisedCacheObject ka _) (NormalisedCacheObject kb _) = ka == kb
    eq EmptyNormalisedObject EmptyNormalisedObject = true
    eq _ _ = false  

type NormalisedCacheObjects 
    = Array NormalisedCacheObject


genCacheKey :: ObjSchema -> String
genCacheKey schema = show schema

flattenDataObject :: ObjSchema -> (FO.Object Json) -> NormalisedCacheObjects
flattenDataObject [] _ = []
flattenDataObject schema o =
    foldl (\acc (ObjProperty key val) -> 
        case val of
            (ValObject nestedObj nestedSchema) -> concat ([
                acc,
                (flattenDataObject nestedSchema nestedObj)
            ])
            otherwise -> acc
    ) [topLevelKey] schema
    where
        topLevelKey = NormalisedCacheObject (genCacheKey schema) o


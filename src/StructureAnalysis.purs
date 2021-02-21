module Data.Abnormal.StructureAnalysis where

import Data.Tuple
import Debug.Trace

import Data.Argonaut (Json, caseJson, fromObject, fromString, jsonEmptyObject, jsonSingletonObject, stringify, toObject)
import Data.Array (snoc, concat)
import Data.Eq (class Eq)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Data.Traversable (Accum, foldl, mapAccumL)
import Foreign.Object as FO
import Prelude (otherwise, ($), (&&), (<>), (==), pure)

type CacheKey = String

type PartialCacheKey = String

type CacheEntries = Array CacheEntry

data CacheEntry = CacheEntry String (FO.Object Json)

instance showCacheEntry :: Show CacheEntry where
    show (CacheEntry key obj) = "\n key: " <> key <> "\n val: " <> (stringify $ fromObject obj)

instance eqCacheEntry :: Eq CacheEntry where
    eq (CacheEntry ka _) (CacheEntry kb _) = ka == kb

data CacheCtx = CacheCtx CacheEntry CacheEntries

instance showCacheCtx :: Show CacheCtx where
    show (CacheCtx ent ents) =
        "Current Entry: \n" <> (show ent) <> "\n\n Cache:\n" <>
        foldl (\acc entS -> acc <> entS <> "\n") "" strings
        where
            strings = map (\entC -> show entC) ents

instance eqCacheCtx :: Eq CacheCtx where
    eq a b = false

createReferenceLinkInObject :: FO.Object Json -> String -> CacheKey -> FO.Object Json
createReferenceLinkInObject obj key cacheKey =
    FO.insert key refObject obj
    where
        refValue = fromString cacheKey
        refObject = jsonSingletonObject "__CACHEREF__" refValue

initialContext :: FO.Object Json -> CacheEntries -> CacheCtx
initialContext o b = CacheCtx (CacheEntry "" o) b

updateCtxKey :: CacheCtx -> String -> String -> CacheCtx
updateCtxKey ctx keyName typeName =
    CacheCtx (
        CacheEntry newKey obj
    ) entries
    where
        (CacheCtx entry entries) = ctx
        (CacheEntry key obj) = entry
        newKey = key <> "(" <> keyName <> ":" <> typeName <> ")"

addCtxCacheEntry :: CacheCtx -> CacheEntry -> CacheCtx
addCtxCacheEntry ctx ent =
    CacheCtx entry (snoc entries ent)
    where
        (CacheCtx entry entries) = ctx

addCtxCacheEntries :: CacheCtx -> CacheEntries -> CacheCtx
addCtxCacheEntries ctx ents =
    CacheCtx entry (concat [entries, ents])
    where
        (CacheCtx entry entries) = ctx

createCtxCacheLink :: CacheCtx -> String -> String -> CacheCtx
createCtxCacheLink ctx keyName linkKey =
    CacheCtx (
        CacheEntry currentKey newObj
    ) entries
    where
        (CacheCtx entry entries) = ctx
        (CacheEntry currentKey obj) = entry
        newObj = createReferenceLinkInObject obj keyName linkKey


normaliseArray :: Array Json -> CacheEntries -> CacheCtx -> CacheCtx
normaliseArray arr baseCache ctx = 
    foldl (\acc jsonVal -> 
        let
            valueWrapper = FO.empty
            wrapped = FO.insert "root" jsonVal valueWrapper
            result = normalise wrapped baseCache (pure acc)
        in
            case FO.lookup "root" of
                (Just val) -> val
                Nothing -> val

    ) ctx arr


normalise :: FO.Object Json -> CacheEntries -> (Maybe CacheCtx) -> CacheCtx
normalise obj baseCache ctxMaybe = cacheResult
    where
    freshCacheObject = 
        case ctxMaybe of
            (Just ctx) -> ctx
            Nothing -> initialContext obj baseCache
    cacheResult = FO.fold 
        (\acc key val -> 
        let 
            (CacheCtx parentEntry parentEntries) = acc
            (CacheEntry parentKey parentObj) = parentEntry
        in
        caseJson
            (\u -> updateCtxKey acc key "null")
            (\b -> updateCtxKey acc key "bool")
            (\n -> updateCtxKey acc key "num")
            (\s -> updateCtxKey acc key "str")
            (\childArr -> normaliseArray childArr parentEntries acc)
            (\child -> 
                let 
                    (CacheCtx childEntry childEntries) = normalise child parentEntries (pure acc)
                    (CacheEntry childKey childObj) = childEntry
                in
                case (FO.lookup "id" child) of
                    (Just idVal) -> 
                        let 
                            addNestedEntries = addCtxCacheEntries acc childEntries
                            childEntryAdded = addCtxCacheEntry addNestedEntries (CacheEntry childKey childObj)
                            keyAdded = updateCtxKey childEntryAdded (stringify idVal) childKey
                            linkAdded = createCtxCacheLink keyAdded key childKey 
                        in
                            linkAdded
                    Nothing -> updateCtxKey acc key childKey
            )
            val
        ) freshCacheObject obj
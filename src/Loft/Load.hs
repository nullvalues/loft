{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE DeriveAnyClass                     #-}
{-# LANGUAGE DeriveGeneric                      #-}
{-# LANGUAGE StandaloneDeriving                 #-}
{-# LANGUAGE DeriveDataTypeable                 #-}
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE ExtendedDefaultRules               #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Loft.Load 
    (
        main
    )   where

import Debug.Trace                                          (traceShow, traceShowId)
import GHC.Integer.GMP.Internals                            (oneBigNat, xorBigNat)
import GHC.Generics                             
import Data.Maybe                                           (fromJust, Maybe, fromMaybe) 
import Data.List                                as List     (elemIndex, sort)
import Data.Text                                            (pack)
import Data.Aeson                               as Aeson    (ToJSON, FromJSON, parseJSON, (.:), (.:?), toJSON, parseJSON, encode, decode, (.=))
import Data.Aeson.Types                                     (withObject, object)
import Data.Typeable                              
import qualified Data.ByteString.Lazy           as LB                       
import Database.MongoDB                         as Mongo
import Control.Monad.IO.Class                               (liftIO)
import Control.Applicative
import B10.Convert                              as Bx
import Loft.Data.Structure                      as Struct
-- import Network.Wai
-- import Network.HTTP.Types                                   (status200)
-- import Network.Wai.Handler.Warp                             (run)
import Yesod    

data SeveralThings = SeveralThings

mkYesod "SeveralThings" [parseRoutes|
/                   HomeR       GET
/allocations        AlcxnR      GET
/allocationTypes    AlcxnTypesR GET
/fetchJson          JsonR       GET
|]

instance Yesod SeveralThings

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!<p><a href=@{AlcxnTypesR}>See Allocation Types</></p><p><a href=@{AlcxnR}>See Allocations</></p>|]

getAlcxnR :: Handler Html
getAlcxnR = defaultLayout [whamlet|Allocations<p><a href=@{AlcxnTypesR}>See Allocation Types</></p>|]

getAlcxnTypesR :: Handler Html
getAlcxnTypesR = defaultLayout [whamlet|<u>Allocations Types</u><p><a href=@{AlcxnR}>See Allocations</></p>|]

-- need to figure out this signature getJsonR :: Handler Html ?
--getJsonR = return $ object ["msg" .= "this is json", "some" .= "other value"]
getJsonR = do 
    return $ object ["msg" .= "this is json", "some" .= "other value"]-- doMunging

main :: IO ()
main = do
    warp 3000 SeveralThings
    munge
    -- pipe <- connect (host "10.10.99.3")
    -- access pipe master "admin" $ auth "admin" "$s500benz"
    -- access pipe master "loft_db-a0" run
    -- Mongo.close pipe

munge :: IO ()
munge = do
    pipe <- connect (host "10.10.99.3")
    access pipe master "admin" $ auth "admin" "$s500benz"
    access pipe master "loft_db-a0" doMunging
    Mongo.close pipe

doMunging :: Action IO () -- this must be same type signature as run
doMunging = do
    allocations <- fetchAllocations
    sillyTransform allocations -- >>= return
    

run :: Action IO ()
run = do
    clearAllocations
    _ <- insertAllocations
    allocationType <- fetchAllocationType "AT-000"
    --fetchAllocationType "AT-000" >>= printDocs "AllocationType"
    printDocs "AllocationType" allocationType
    allocationTypes <- fetchAllocationTypes
    --fetchAllocationTypes >>= printDocs "AllocationTypes"
    printDocs' "AllocationTypes" allocationTypes
    --lessMongo allocationTypes
    allocations <-fetchAllocations
    printDocs' "Allocations" allocations

clearAssertions :: Action IO ()
clearAssertions = Mongo.delete (select [] "assertions")

clearAllocations :: Action IO ()
clearAllocations = Mongo.delete (select [] "allocations")

insertAllocations :: Action IO [Mongo.Value]
insertAllocations = do
    let thisLoftDate = Struct.LoftDate 
                        { year          = 2022
                        , month         = 7
                        , day           = 25
                        , hour          = 14
                        , minute        = 24
                        , second        = 0
                        , ms            = 0
                        }
        thisLoftDateJson = encode thisLoftDate
        alcxnOne = Struct.Alcxn 
                        { alcxnId           = Bx.convertB10ToBx 100000001
                        , alcxnTypeFkid     = "AT-2"
                        , alcxnName         = "First Client"
                        , alcxnDescription  = Nothing
                        , alcxnCreated      = thisLoftDate
                        , alcxnModified     = thisLoftDate
                        }
        alcxnTwo = Struct.Alcxn 
                        { alcxnId           = Bx.convertB10ToBx 100000002
                        , alcxnTypeFkid     = "AT-2"
                        , alcxnName         = "Second Client"
                        , alcxnDescription  = Just "Anything at all"
                        , alcxnCreated      = thisLoftDate
                        , alcxnModified     = thisLoftDate
                        }    
    Mongo.insertMany "allocations" 
        [ 
            ["_id" =: alcxnId alcxnOne, "name" =: alcxnName alcxnOne, "dateCreated" =: val thisLoftDate , "dateModified" =: val thisLoftDate]
        ,   ["_id" =: alcxnId alcxnTwo, "name" =: alcxnName alcxnTwo, "dateCreated" =: val thisLoftDate, "dateModified" =: val thisLoftDate]
        ]


fetchAllocationType :: String -> Action IO [Document]
fetchAllocationType id = rest =<< Mongo.find (select ["_id"=: ["$eq"=: id]] "allocationTypes")  

fetchAllocationTypes :: Action IO [Document]
fetchAllocationTypes = rest =<< Mongo.find (select [] "allocationTypes") {Mongo.sort = ["values.hidden" =: 1, "_id"=: 1]}  

fetchAllocations :: Action IO [Document]
fetchAllocations = rest =<< Mongo.find (select [] "allocations") {Mongo.sort = ["values.hidden" =: 1, "_id"=: 1]}

printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . exclude ["_id"]) docs

printDocs' :: String -> [Document] -> Action IO ()
printDocs' title docs = liftIO $ putStrLn title >> mapM_ print docs

--createAllocationFromType :: String -> Action IO [Document]
--createAllocationFromType id =     rest =<< fetch (select ["_id"=: ["$eq"=: id]] "allocationTypes")

lessMongo :: [Document] -> Action IO ()
lessMongo documents = do
    --liftIO $ putStrLn "blah" >> mapM_ print documents
    liftIO(mapM_ (print . exclude ["identity"]) documents)

sillyTransform :: [Document] -> Document
sillyTransform documents = do
    mapM_ Aeson.toJSON documents

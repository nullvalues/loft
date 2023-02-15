{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE DeriveAnyClass                     #-}
{-# LANGUAGE DeriveGeneric                      #-}
{-# LANGUAGE StandaloneDeriving                 #-}
{-# LANGUAGE DeriveDataTypeable                 #-}
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE ExtendedDefaultRules               #-}

module Loft.Data.Structure

( SysInfo (..)
, AlcxnTypeIdentityValues (..)
, AlcxnTypeIdentity (..)
, AlcxnType (..)
, Alcxn (..)
, SuperAlcxn (..)
, LoftDate (..)
)

where

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

data SysInfo = SysInfo
    { appIdentifier         :: String
    , appName               :: String
    , versionB10            :: Integer
    , versionB56Assertion   :: String
    , appVersion            :: String
    , databaseVersion       :: String
    , deliveryStage         :: String
    , rootUser              :: String
    , rootUserPass          :: String
    , bootstrapUser         :: String
    , bootstrapPass         :: String
    , authDatabase          :: String
    , serverUri             :: String
    } deriving (Generic, ToJSON, FromJSON, Typeable, Show)

data AlcxnTypeIdentityValues = AlcxnTypeIdentityValues
    { hidden                    :: !Bool
    , inherited                 :: !Bool
    , currencyIsPrimaryValue    :: !Bool
    , currencyIsRequired        :: !Bool
    , currencyUom               :: !Bool
    , currencyDecimalPlaces     :: !Bool
    , hoursIsRequired           :: !Bool
    , hoursDecimalPlaces        :: !Bool
    , hoursFractionsAllowed     :: ![Integer] 
    , allowPrimaryZeroValues    :: !Bool
    } deriving (Generic, ToJSON, FromJSON)

data AlcxnTypeIdentity = AlcnTypeIdentity
    { name      :: !String
    , names     :: !String
    , values    :: !AlcxnTypeIdentityValues
    } deriving (Generic, ToJSON, FromJSON)

data SuperAlcxn = SuperAlcxn
    { isSuperAllocation         :: !Bool 
    , isAllocation              :: !Bool
    , hasSubordinate            :: ![String]
    , hasSubordinateRequired    :: !Bool    
    } deriving (Generic, ToJSON, FromJSON)

data SubordinateAlcxn = SubordinateAlcxn
    { isSubordinationAllowed    :: !Bool
    , isReserved                :: ![String]
    } deriving (Generic, ToJSON, FromJSON)

data AlcxnType = AlcxnType 
    { alcxnTypeId               :: !String
    , alcxnTypeVersion          :: !String
    , alcxnTypeIdentity         :: !AlcxnTypeIdentity
    , alcxnTypeReference        :: ![String]
    , otherFields               :: ![String]
    , superAlcxn                :: !SuperAlcxn
    , subordinateAlcxn          :: !SubordinateAlcxn
    } deriving (Generic, ToJSON, FromJSON)

data LoftDate = LoftDate
    { year          :: Integer
    , month         :: Integer
    , day           :: Integer
    , hour          :: Integer
    , minute        :: Integer
    , second        :: Integer
    , ms            :: Integer
    } deriving (Generic, Show, Eq)

instance ToJSON LoftDate where
    toJSON LoftDate 
            { year          = year
            , month         = month
            , day           = day
            , hour          = hour
            , minute        = minute
            , second        = second
            , ms            = ms
            } = 
        object 
            [ "year"        .= year
            , "month"       .= month
            , "day"         .= day
            , "hour"        .= hour
            , "minute"      .= minute
            , "second"      .= second
            , "ms"          .= ms
            ]

instance FromJSON LoftDate where
    parseJSON = withObject "LoftDate" $ \obj -> do
        year          <- obj .: "year"
        month         <- obj .: "month"
        day           <- obj .: "day"
        hour          <- obj .: "hour"
        minute        <- obj .: "minute"
        second        <- obj .: "second"
        ms            <- obj .: "ms"
        return (LoftDate 
            { year          = year :: Integer
            , month         = month :: Integer
            , day           = day :: Integer
            , hour          = hour :: Integer
            , minute        = minute :: Integer
            , second        = second :: Integer
            , ms            = ms :: Integer
            })
instance Val LoftDate where
    val LoftDate 
            { year          = year
            , month         = month
            , day           = day
            , hour          = hour
            , minute        = minute
            , second        = second
            , ms            = ms
            } = 
        Doc 
            [ "year"        := Int64 (fromIntegral year)
            , "month"       := Int64 (fromIntegral month)
            , "day"         := Int64 (fromIntegral day)
            , "hour"        := Int64 (fromIntegral hour)
            , "minute"      := Int64 (fromIntegral minute)
            , "second"      := Int64 (fromIntegral second)
            , "ms"          := Int64 (fromIntegral ms)
            ]


data Alcxn = Alcxn
    { alcxnId           :: String
    , alcxnTypeFkid     :: String
    , alcxnName         :: String
    , alcxnDescription  :: Maybe String
    , alcxnCreated      :: LoftDate
    , alcxnModified     :: LoftDate
    } deriving (Generic, ToJSON, FromJSON)
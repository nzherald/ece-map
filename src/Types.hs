{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Cases
import           Data.Csv
import qualified Data.Text                     as T
import           GHC.Generics


data School = School
  { schoolNumber          :: String
  , schoolName            :: T.Text
  , schoolStreet          :: T.Text
  , schoolSuburb          :: T.Text
  , schoolTownCity        :: T.Text
  , schoolInstitutionType :: T.Text
  , schoolLongitude       :: Maybe Double
  , schoolLatitude        :: Maybe Double
  , schoolUnder2s         :: Maybe Int
  , schoolTotalRoll       :: Maybe Int
  } deriving (Show, Eq, Generic)

schoolOptions :: Options
schoolOptions = defaultOptions
  { fieldLabelModifier = T.unpack
                         . T.replace "2_s" "2s"
                         . snakify
                         . T.replace "school" ""
                         . T.pack
  }

instance ToNamedRecord School where
  toNamedRecord = genericToNamedRecord schoolOptions

instance FromNamedRecord School where
  parseNamedRecord = genericParseNamedRecord schoolOptions

instance DefaultOrdered School where
  headerOrder = genericHeaderOrder schoolOptions

data ReportLink = ReportLink
  { reportLinkSchoolNumber :: String
  , reportLinkDate         :: String
  , reportLinkUrl          :: T.Text
  } deriving (Show, Eq, Generic)


reportLinkOptions :: Options
reportLinkOptions = defaultOptions
  { fieldLabelModifier = T.unpack . snakify . T.replace "reportLink" "" . T.pack
  }

instance ToNamedRecord ReportLink where
  toNamedRecord = genericToNamedRecord reportLinkOptions

instance FromNamedRecord ReportLink where
  parseNamedRecord = genericParseNamedRecord reportLinkOptions

instance DefaultOrdered ReportLink where
  headerOrder = genericHeaderOrder reportLinkOptions


data Missing
  = Report
  | Summary
  deriving (Show, Eq, Generic)

instance ToField Missing
  where
  toField Report  = "report"
  toField Summary = "summary"


data Ranking
  = VeryWellPlaced
  | WellPlaced
  | FurtherDevelopment
  | NotWellPlaced
  | NoRating
  | Problem Missing
  deriving (Show, Eq, Generic)

instance ToField Ranking
  where
  toField VeryWellPlaced     = "very well placed"
  toField WellPlaced         = "well placed"
  toField FurtherDevelopment = "needs further development"
  toField NotWellPlaced      = "not well placed"
  toField NoRating           = "no rating"
  toField (Problem s)        = toField s

data Ranked = Ranked
  { rankedId   :: String
  , rankedDate :: String
  , rankedRank :: Ranking
  , rankedUrl  :: T.Text
  } deriving (Show, Eq, Generic)

instance ToNamedRecord Ranked
  where
  toNamedRecord = genericToNamedRecord defaultOptions
instance DefaultOrdered Ranked
  where
  headerOrder = genericHeaderOrder defaultOptions


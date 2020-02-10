{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where


import Data.Csv hiding ((.=))
import GHC.Generics
import Data.Text


data School = School 
  { schoolId :: String
  , regionId :: Text
  , districtId :: Text
  , cauId :: Text
  , name :: Text
  , latitude :: Double
  , longitude :: Double
  } deriving (Show, Eq, Generic, FromRecord)

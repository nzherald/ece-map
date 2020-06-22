{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where


import Data.Csv hiding ((.=))
import GHC.Generics
import Data.Text


data School = School
  { schoolId :: String
  , name :: Text
  , latitude :: Maybe Double
  , longitude :: Maybe Double
  } deriving (Show, Eq, Generic, FromRecord)

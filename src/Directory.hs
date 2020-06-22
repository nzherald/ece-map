{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Directory (getSchools) where

import qualified Control.Foldl        as L
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Text            as T
import           Data.Vector

import           Types



getSchools :: IO [School]
getSchools = go . decodeByName <$> BL.readFile "data/Directory-ECE-Current.clean.csv"
  where
    go (Left errorMsg) = error errorMsg
    go (Right (_, v))  = toList v



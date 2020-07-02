{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Directory
  ( getSchools
  )
where

import qualified Control.Foldl                 as L
import qualified Data.ByteString.Lazy          as BL
import           Data.Csv
import qualified Data.Text                     as T
import           Data.Vector

import           Types

{- 
Data downloaded from https://www.educationcounts.govt.nz/data-services/directories/early-childhood-services

Clean by removing extra rows are renaming columns

The same data is also available from https://catalogue.data.govt.nz/dataset/directory-of-educational-institutions/resource/26f44973-b06d-479d-b697-8d7943c97c57?inner_span=True
-}

getSchools :: IO [School]
getSchools = go . decodeByName <$> BL.readFile
  "data/Directory-ECE-Current.clean.csv"
 where
  go (Left  errorMsg) = error errorMsg
  go (Right (_, v)  ) = toList v



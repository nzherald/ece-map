{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where


import qualified Data.ByteString.Lazy as BL
import Data.Csv hiding ((.=))
import qualified Data.Vector as V
import qualified Data.Text as T
import GHC.Generics
import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Control.Monad
import qualified Control.Monad.Parallel as P
import System.FilePath
import System.Directory
import Data.List.Split


data School = School 
  { schoolId :: String
  , regionId :: T.Text
  , districtId :: T.Text
  , cauId :: T.Text
  , name :: T.Text
  , latitude :: Double
  , longitude :: Double
  } deriving (Show, Eq, Generic, FromRecord)



main :: IO ()
main = do
  csvData <- BL.readFile "data/ece_locations.csv"
  case Data.Csv.decode HasHeader csvData of
        Left err -> putStrLn err
        Right v -> forM_ (chunksOf 4 $ V.toList v) (fetchSchools)


fetchSchools :: [School] -> IO ()
fetchSchools s = do
  P.forM_ s fetchSchool


fetchSchool :: School -> IO ()
fetchSchool (School schoolId regionId districtId cauId name latitude longitude) = do
  let p = "data" </> "profiles" </> schoolId -<.> "html"
  exists <- doesFileExist p
  if exists 
  then
     print $ name <> " exists"
  else
    runReq defaultHttpConfig $ do
    r <- req GET -- method
      (https "www.educationcounts.govt.nz" /: "find-an-els" /: "els" /: "profile-and-contact-details") -- safe by construction URL
      (NoReqBody)
      lbsResponse -- specify how to interpret response
      (queryParam "ece" (pure schoolId))
    liftIO $ do
      BL.writeFile p (responseBody r)
      print $ name <> " fetched"


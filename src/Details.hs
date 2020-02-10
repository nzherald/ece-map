{-# LANGUAGE OverloadedStrings #-}
module Details where


import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Control.Monad.IO.Class
import Network.HTTP.Req
import Control.Monad
import qualified Control.Monad.Parallel as P
import System.FilePath
import System.Directory
import Data.List.Split

import Types

details :: FilePath -> IO ()
details fp = do
  csvData <- BL.readFile fp
  case Csv.decode Csv.HasHeader csvData of
        Left err -> putStrLn err
        Right v -> forM_ (chunksOf 4 $ V.toList v) (fetchSchools)


fetchSchools :: [School] -> IO ()
fetchSchools s = do
  P.forM_ s fetchSchool


fetchSchool :: School -> IO ()
fetchSchool (School sch_id _ _ _ n _ _) = do
  let p = "data" </> "profiles" </> sch_id -<.> "html"
  exists <- doesFileExist p
  if exists 
  then
     print $ n <> " exists"
  else
    runReq defaultHttpConfig $ do
    r <- req GET -- method
      (https "www.educationcounts.govt.nz" /: "find-an-els" /: "els" /: "profile-and-contact-details") -- safe by construction URL
      (NoReqBody)
      lbsResponse -- specify how to interpret response
      (queryParam "ece" (pure sch_id))
    liftIO $ do
      BL.writeFile p (responseBody r)
      print $ n <> " fetched"


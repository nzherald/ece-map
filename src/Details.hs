{-# LANGUAGE OverloadedStrings #-}
module Details where


import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class
import Network.HTTP.Req
import qualified Control.Monad.Parallel as P
import System.FilePath
import System.Directory

import Types
import Common

details :: FilePath -> IO ()
details = schools fetchSchools 4

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
      (queryParam "ece" (pure sch_id) <> nzhScraper)
    liftIO $ do
      BL.writeFile p (responseBody r)
      print $ n <> " fetched"


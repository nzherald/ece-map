{-# LANGUAGE OverloadedStrings #-}
module Common where


import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Control.Monad
import Data.List.Split
import Network.HTTP.Req

chunkedSchools :: Csv.FromRecord e => ([e] -> IO b) -> Int -> FilePath -> IO ()
chunkedSchools opp chunkSize fp = do
  csvData <- BL.readFile fp
  case Csv.decode Csv.HasHeader csvData of
        Left err -> error err
        Right v -> forM_ (chunksOf chunkSize $ V.toList v) opp


schools :: Csv.FromRecord e => FilePath -> IO [e]
schools fp = do
  csvData <- BL.readFile fp
  case Csv.decode Csv.HasHeader csvData of
        Left err -> error err
        Right v -> pure $ V.toList v

nzhScraper :: Option scheme
nzhScraper = header "User-Agent" "New Zealand Herald Data Journalism - We try to be polite and careful with our scraping - if we do cause a problem please contact chris.knox@nzherald.co.nz"

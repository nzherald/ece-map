module Common where


import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Control.Monad
import Data.List.Split

schools :: Csv.FromRecord e => ([e] -> IO b) -> Int -> FilePath -> IO ()
schools opp chunkSize fp = do
  csvData <- BL.readFile fp
  case Csv.decode Csv.HasHeader csvData of
        Left err -> putStrLn err
        Right v -> forM_ (chunksOf chunkSize $ V.toList v) opp

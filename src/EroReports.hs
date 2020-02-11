{-# LANGUAGE OverloadedStrings #-}
module EroReports (reports) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Void
import Data.List
import Data.Maybe
import Data.Time
import Text.HTML.TagSoup
import Text.HTML.Scalpel
import Network.HTTP.Req
import Control.Monad.IO.Class
import Control.Monad.Parallel (forM_)
import System.FilePath
import System.Directory

import Types
import Common

type Parser = Parsec Void Text


reports :: FilePath -> IO ()
reports = schools getSchools 4
  where 
    getSchools s = forM_ s get

parseDay :: Parser Day
parseDay = do
  d <- try (count 2 digitChar) <|> count 1 digitChar
  _ <- char '-'
  m <- try (count 2 digitChar) <|> count 1 digitChar
  _ <- char '-'
  y <- count 4 digitChar
  parseTimeM True defaultTimeLocale "%Y-%m-%d" $ y ++ "-" ++ m ++ "-" ++ d

tagLink :: Scraper Text [Text]
tagLink = chroots ("article" // "a") $ do
    content <- attr "href" "a"
    return content


extract :: BS.ByteString -> [Text]
extract v = do
  case decodeUtf8' v of
    Left _ -> [] -- for now just skip decode errors - if there are a lot of them I'll figure out a better strategy
    Right vv -> maybe [] nub $ scrape tagLink $ parseTags vv

extractDate :: Text -> Maybe Day
extractDate = parseMaybe (skipSomeTill printChar parseDay <* printChar)

get :: School -> IO ()
get (School sch_id _ _ _ n _ _) = do
  let p = "data" </> "reports" </> sch_id -<.> "html"
  exists <- doesFileExist p
  if exists 
  then
     print $ n <> " exists"
  else
    runReq defaultHttpConfig $ do
        r <- req GET (https "www.ero.govt.nz" /: "report-view") (NoReqBody) bsResponse (queryParam "id" (pure sch_id) <> nzhScraper)
        let bdy = responseBody r :: BS.ByteString
            links = map (\l -> "https://www.ero.govt.nz" <> l) $ extract bdy
            dates = map show $ catMaybes $ map extractDate links
            summary = zip dates links

        mapM_ fetchReport summary

        liftIO $ do
          BS.writeFile p (responseBody r)
          print $ n <> " fetched"
          BL.writeFile (p -<.> "csv") (Csv.encode $ reverse $ sortOn fst summary)
      where
        fetchReport (d, l)  = do
          case parseUrlHttps $ encodeUtf8  l of
            Nothing -> liftIO $ print $ "could not decode " <> l
            Just (url, _) -> do
              let fo = "data" </> "reports" </> sch_id ++ "-" ++ d -<.> "html"
              rep <- req GET url NoReqBody bsResponse nzhScraper
              liftIO $ BS.writeFile fo (responseBody rep)




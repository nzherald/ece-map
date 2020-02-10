{-# LANGUAGE OverloadedStrings #-}
module EroReports (get) where

import qualified Data.ByteString as BL
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Void
import Data.List
import Data.Maybe
import Data.Time
import Text.HTML.TagSoup
import Text.HTML.Scalpel
import Network.HTTP.Req
import Control.Monad.IO.Class

-- import Types

type Parser = Parsec Void Text


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


extract :: BL.ByteString -> [Text]
extract v = do
   maybe [] nub $ scrape tagLink $ parseTags $ decodeUtf8 v


play = parseMaybe (skipSomeTill printChar parseDay <* printChar)

get :: IO ()
get = do
  runReq defaultHttpConfig $ do
      r <- req GET -- method
        (https "www.ero.govt.nz" /: "report-view")
        (NoReqBody)
        bsResponse -- specify how to interpret response
        (queryParam "id" (pure ("60246"::String)))
      let bdy = responseBody r :: BL.ByteString
          links = extract bdy
          dates = catMaybes $ map play links

      liftIO $ print $ reverse $ sortOn fst $ zip dates links



data ReportLink = 
  ReportDate Day
  ReportLink URL
  deriving (Show, Eq)

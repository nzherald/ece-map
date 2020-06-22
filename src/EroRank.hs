{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module EroRank (ranks) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import System.FilePath
import qualified Control.Monad.Parallel as P
import GHC.Generics

import Types
import Common


data Missing
  = Report
  | Summary
  deriving (Show, Eq, Generic, Csv.ToField)


data Ranking
  = VeryWellPlaced
  | WellPlaced
  | FurtherDevelopment
  | NotWellPlaced
  | NoRating
  | Problem Missing
  deriving (Show, Eq, Generic, Csv.ToField)


data Ranked = Ranked
  { rankedName :: String
  , rankedId :: Text
  , rankedRank :: Ranking
  } deriving (Show, Eq, Generic, Csv.ToRecord)

instance Csv.ToRecord Ranking where
    toRecord r = Csv.record [ ]


ranks :: FilePath -> IO ()
ranks fp = do
  sch <- schools fp
  r <- P.mapM recent sch
  BL.writeFile "data/ranked-schools.csv" $ Csv.encode $ map (\((School n i _ _), rnk) -> Ranked n i rnk) $ zip sch r

recent :: School -> IO Ranking
recent (School id_ _ _ _) = do
  raw <- BL.readFile $ "data" </> "reports" </> id_ -<.> "csv"
  case Csv.decode Csv.NoHeader raw of
    Left e -> error e
    Right (d::V.Vector (String,Text)) -> case V.toList d of
                                         [] -> pure $ Problem Report
                                         h:_ -> readRating id_ $ fst h


readRating :: String -> String -> IO Ranking
readRating id_ s = do
  htmlstr <- T.readFile $ "data" </> "reports" </> (id_ ++ "-" ++ s) -<.> "html"
  pure $ if T.isInfixOf "<b>Very well placed</b>" htmlstr || T.isInfixOf "<strong>Very well placed</strong>" htmlstr
            then VeryWellPlaced
            else if T.isInfixOf "<b>Well placed</b>" htmlstr || T.isInfixOf "<strong>Well placed</strong>" htmlstr
            then WellPlaced
            else if T.isInfixOf "<b>Requires further development</b>" htmlstr ||T.isInfixOf "<strong>Requires further development</strong>" htmlstr
            then FurtherDevelopment
            else if T.isInfixOf "<b>Not well placed</b>" htmlstr || T.isInfixOf "<strong>Not well placed</strong>" htmlstr
            then NotWellPlaced
            else NoRating



-- parseDay :: Parser Day
-- parseDay = do
--   d <- try (count 2 digitChar) <|> count 1 digitChar
--   _ <- char '-'
--   m <- try (count 2 digitChar) <|> count 1 digitChar
--   _ <- char '-'
--   y <- count 4 digitChar
--   parseTimeM True defaultTimeLocale "%Y-%m-%d" $ y ++ "-" ++ m ++ "-" ++ d

-- tagLink :: Scraper Text [Text]
-- tagLink = chroots ("article" // "a") $ do
--     content <- attr "href" "a"
--     return content


-- extract :: BS.ByteString -> [Text]
-- extract v = do
--   case decodeUtf8' v of
--     Left _ -> [] -- for now just skip decode errors - if there are a lot of them I'll figure out a better strategy
--     Right vv -> maybe [] nub $ scrape tagLink $ parseTags vv

-- extractDate :: Text -> Maybe Day
-- extractDate = parseMaybe (skipSomeTill printChar parseDay <* printChar)

-- get :: School -> IO ()
-- get (School sch_id n _ _) = do
--   let p = "data" </> "reports" </> sch_id -<.> "html"
--   exists <- doesFileExist p
--   if exists
--   then
--      print $ n <> " exists"
--   else
--     runReq defaultHttpConfig $ do
--         r <- req GET (https "www.ero.govt.nz" /: "report-view") (NoReqBody) bsResponse (queryParam "id" (pure sch_id) <> nzhScraper)
--         let bdy = responseBody r :: BS.ByteString
--             links = map (\l -> "https://www.ero.govt.nz" <> l) $ extract bdy
--             dates = map show $ catMaybes $ map extractDate links
--             summary = zip dates links

--         mapM_ fetchReport summary

--         liftIO $ do
--           BS.writeFile p (responseBody r)
--           print $ n <> " fetched"
--           BL.writeFile (p -<.> "csv") (Csv.encode $ reverse $ sortOn fst summary)
--       where
--         fetchReport :: (FilePath, Text) -> Req ()
--         fetchReport (d, l)  = do
--           let p = parser <* eof :: Parsec Void Text URI
--           case parseMaybe p l of
--             Nothing -> liftIO $ print $ "could not decode " <> l
--             Just uri ->
--                       do
--                         let fo = "data" </> "reports" </> sch_id ++ "-" ++ d -<.> "html"
--                             (Just (url, _)) = useHttpsURI uri
--                         rep <- req GET url NoReqBody bsResponse nzhScraper
--                         liftIO $ BS.writeFile fo (responseBody rep)
